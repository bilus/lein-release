(ns leiningen.release
  (:require
   [clojure.java.shell     :as sh]
   [clojure.string         :as string]
   [leiningen.core.project :as project]
   leiningen.do)
  (:refer leiningen.do :rename {do lein-do})
  (:import
   [java.util.regex Pattern]))

(defn raise [fmt & args]
  (throw (RuntimeException. (apply format fmt args))))

(def default-clojars-url "clojars@clojars.org:")

(def ^:dynamic config {:clojars-url default-clojars-url})

(def scm-systems
     {:git {:add    ["git" "add"]
            :tag    ["git" "tag"]
            :commit ["git" "commit"]
            :push   ["git" "push" "origin" "master"]
            :status ["git" "status"]}})

(defn detect-scm []
  (or
   (:scm config)
   (cond
     (.exists (java.io.File. ".git"))
     :git
     :no-scm-detected
     (raise "Error: no scm detected! (I know only about git for now)."))))

(defn sh! [& args]
  (let [res (apply sh/sh args)]
    (.println System/out (:out res))
    (.println System/err (:err res))
    (when-not (zero? (:exit res))
      (raise "Error: command failed %s => %s" args res))))

(defn scm! [cmd & args]
  (let [scm   (detect-scm)
        scm-cmd (get-in scm-systems [scm cmd])]
    (if-not scm-cmd
      (raise "No such SCM command: %s in %s" cmd scm))
    (apply sh! (concat scm-cmd args))))

(def maven-version-regexes
     {:major-only                               #"(\d+)(?:-(.+))?"
      :major-and-minor                          #"(\d+)\.(\d+)(?:-(.+))?"
      :major-minor-and-incremental              #"(\d+)\.(\d+)\.(\d+)(?:-(.+))?"})

(defn parse-maven-version [vstr]
  ;; <MajorVersion [> . <MinorVersion [> . <IncrementalVersion ] ] [> - <BuildNumber | Qualifier ]>
  (cond
    (re-matches (:major-only maven-version-regexes) vstr)
    (let [[[_ major qualifier]] (re-seq (:major-only maven-version-regexes) vstr)]
      {:format      :major-only
       :major       major
       :minor       nil
       :incremental nil
       :qualifier   qualifier})

    (re-matches (:major-and-minor maven-version-regexes) vstr)
    (let [[[_ major minor qualifier]] (re-seq (:major-and-minor maven-version-regexes) vstr)]
      {:format      :major-and-minor
       :major       major
       :minor       minor
       :incremental nil
       :qualifier   qualifier})

    (re-matches (:major-minor-and-incremental maven-version-regexes) vstr)
    (let [[[_ major minor incremental qualifier]] (re-seq (:major-minor-and-incremental maven-version-regexes) vstr)]
      {:format      :major-minor-and-incremental
       :major       major
       :minor       minor
       :incremental incremental
       :qualifier   qualifier})
    :else
    {:format :not-recognized
     :major vstr}))

(comment

  (parse-maven-version "1")
  {:format :major-only, :major "1", :minor nil, :incremental nil, :qualifier nil}
  (parse-maven-version "1-SNAPSHOT")
  {:format :major-only, :major "1", :minor nil, :incremental nil, :qualifier "SNAPSHOT"}
  (parse-maven-version "1-b123")
  {:format :major-only, :major "1", :minor nil, :incremental nil, :qualifier "b123"}

  (parse-maven-version "1.2")
  {:format :major-and-minor, :major "1", :minor "2", :incremental nil, :qualifier nil}
  (parse-maven-version "1.2-SNAPSHOT")
  {:format :major-and-minor, :major "1", :minor "2", :incremental nil, :qualifier "SNAPSHOT"}
  (parse-maven-version "1.2-b123")
  {:format :major-and-minor, :major "1", :minor "2", :incremental nil, :qualifier "b123"}

  (parse-maven-version "1.2.3")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier nil}
  (parse-maven-version "1.2.3-SNAPSHOT")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier "SNAPSHOT"}
  (parse-maven-version "1.2.3-b123")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier "b123"}

  (parse-maven-version "1.2.3-rc1")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier "rc1"}

  )

(defn ^:dynamic get-release-qualifier []
  (System/getenv "RELEASE_QUALIFIER"))


;; 1.0.116-SNAPSHOT
;; 1.0.116-v2

;; See: http://mojo.codehaus.org/versions-maven-plugin/version-rules.html
(defn compute-next-development-version [current-version]
  (let [parts             (vec (.split current-version "\\."))
        version-parts     (vec (take (dec (count parts)) parts))
        minor-version     (last parts)
        new-minor-version (str (inc (Integer/parseInt minor-version)) "-SNAPSHOT")]
    (string/join "." (conj version-parts new-minor-version))))

(defn is-snapshot? [vstring]
  (.endsWith vstring "-SNAPSHOT"))

(defn compute-release-version [current-version]
  (str (.replaceAll current-version "-SNAPSHOT" "")
       (get-release-qualifier)))


(defn replace-project-version [old-vstring new-vstring]
  (let [proj-file     (slurp "project.clj")
        new-proj-file (.replaceAll proj-file (format "\\(defproject .+? %s" old-vstring) new-vstring )
        matcher       (.matcher
                       (Pattern/compile (format "(\\(defproject .+? )\"\\Q%s\\E\"" old-vstring))
                       proj-file)]
    (if-not (.find matcher)
      (raise "Error: unable to find version string %s in project.clj file!" old-vstring))
    (.replaceFirst matcher (format "%s\"%s\"" (.group matcher 1) new-vstring))))

(defn set-project-version! [old-vstring new-vstring]
  (spit "project.clj" (replace-project-version old-vstring new-vstring)))

(defn detect-deployment-strategy
  ([project]
     (or (:deploy-via config)
         (and (:repositories project) :lein-deploy)
         :lein-install))
  ([project & _]
     (detect-deployment-strategy project)))

(defn clojars-url []
  (or (:clojars-url config)
      default-clojars-url))

(defmulti deploy! detect-deployment-strategy :default :unrecognized)

(defmethod deploy! :lein-deploy [project artifact]
  (println (format "deploying %s" artifact))
  (lein-do project "deploy"))

(defmethod deploy! :lein-install [project artifact]
  (println (format "installing %s" artifact))
  (lein-do project "install"))

(defmethod deploy! :clojars [project artifact]
  (println (format "deploying %s to clojars at %s" artifact (clojars-url)))
  (sh! "scp" "pom.xml" artifact (clojars-url)))

(defmethod deploy! :shell [project artifact]
  (println (format "executing shell command %s" (:shell config)))
  (apply sh! (:shell config)))

(defmethod deploy! :unrecognized [project artifact]
  (raise "Error: unrecognized deploy strategy: %s" (detect-deployment-strategy project)))


(defn detect-build-strategy
  ([]
     (or (:build-via config) :lein-jar-pom))
  ([& _]
     (detect-build-strategy)))

(defmulti build! detect-build-strategy :default :unrecognized)

(defmethod build! :lein-jar-pom [project target-dir & args]
  (let [artifact (format "%s/%s-%s.jar" target-dir (:name project) (:version project))]
    (when-not (-> artifact java.io.File. .exists)
      (println "creating jar and pom files...")
      (lein-do project "jar," "pom"))
    artifact))

(defmethod build! :lein-uberjar [project target-dir & args]
  (let [artifact (format "%s/%s-%s.jar" target-dir (:name project) (:version project))]
    (println artifact)
    (when-not (-> artifact java.io.File. .exists)
      (println "creating jar and pom files...")
      (sh! "lein" "do" "clean," "uberjar"))
    artifact))

(defmethod build! :unrecognized [project target-dir & args]
  (raise "Error: unrecognized build strategy: %s" (detect-build-strategy project)))

(defn extract-project-version-from-file
  ([]
     (extract-project-version-from-file "project.clj"))
  ([proj-file]
     (let [s (slurp proj-file)
           m (.matcher (Pattern/compile "\\(defproject .+? \"([^\"]+?)\"") s)]
       (if-not (.find m)
         (raise "Error: unable to find project version in file: %s" proj-file))
       (.group m 1))))

(defn update-version-in-project [project release-version]
  (project/merge-profiles project [{:version release-version}]))

(defn prepare-from-snapshot [project current-version release-version]
  (println (format "setting project version %s => %s" current-version release-version))
  (set-project-version! current-version release-version)
  (println "adding, committing and tagging project.clj")
  (scm! :add "project.clj")
  (scm! :commit "-m" (format "lein-release plugin: preparing %s release" release-version))
  (scm! :tag (format "%s-%s" (:name project) release-version)))


(defn update-to-snapshot [current-version release-version]
  (let [next-dev-version (compute-next-development-version (.replaceAll current-version "-SNAPSHOT" ""))]
    (println (format "updating version %s => %s for next dev cycle" release-version next-dev-version))
    (set-project-version! release-version next-dev-version)
    (scm! :add "project.clj")
    (scm! :commit "-m" (format "lein-release plugin: bumped version from %s to %s for next development cycle" release-version next-dev-version))))


(comment

  (binding [get-release-qualifier (fn [] "-v2")]
    (compute-release-version "1.0.116-SNAPSHOT"))

)


(defn release [project & args]
  (binding [config (or (:lein-release project) config)]
    (let [current-version  (get project :version)
          release-version  (compute-release-version current-version)
          release-project  (update-version-in-project project release-version)
          target-dir       (:target-path project (:target-dir project (:jar-dir project "."))) ; target-path for lein2, target-dir or jar-dir for lein1
          ]
      (when (is-snapshot? current-version)
        (prepare-from-snapshot project current-version release-version))
      (->> (build! release-project target-dir)
           (deploy! release-project))
      (when-not (is-snapshot? (extract-project-version-from-file))
        (update-to-snapshot current-version release-version)))))

