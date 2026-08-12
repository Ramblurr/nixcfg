#!/usr/bin/env bb

(ns update
  (:require
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [babashka.process :as process]
   [clojure.string :as str]))

(def packages
  {"epimetheus"
   {:nix-name    "epimetheus"
    :package-file "pkgs/pi/epimetheus/package.nix"
    :lock-file   "pkgs/pi/epimetheus/package-lock.json"
    :source      {:kind :github
                  :owner "noctuid"
                  :repo  "epimetheus"
                  :mode  :release}}
   "pi-hashline-edit"
   {:nix-name    "pi-hashline-edit"
    :package-file "pkgs/pi/pi-hashline-edit/package.nix"
    :lock-file   "pkgs/pi/pi-hashline-edit/package-lock.json"
    :source      {:kind :github
                  :owner "RimuruW"
                  :repo  "pi-hashline-edit"
                  :mode  :head}}
   "pi-mcp-adapter"
   {:nix-name    "pi-mcp-adapter"
    :package-file "pkgs/pi/pi-mcp-adapter/package.nix"
    :lock-file   "pkgs/pi/pi-mcp-adapter/package-lock.json"
    :source      {:kind :github
                  :owner "nicobailon"
                  :repo  "pi-mcp-adapter"
                  :mode  :release}}
   "pi-nrepl"
   {:nix-name    "pi-nrepl"
    :package-file "pkgs/pi/pi-nrepl/package.nix"
    :source      {:kind :github
                  :owner "ramblurr"
                  :repo  "pi-nrepl"
                  :mode  :release}}
   "plannotator"
   {:nix-name    "plannotator-pi-extension"
    :package-file "pkgs/pi/plannotator/package.nix"
    :lock-file   "pkgs/pi/plannotator/package-lock.json"
    :source      {:kind :npm
                  :name "@plannotator/pi-extension"}}})

(defn command-output
  [dir & args]
  (-> (apply process/shell {:dir dir :out :string} args)
      :out
      str/trim))

(defn run!
  [dir & args]
  (apply process/shell {:dir dir} args))

(defn repo-root []
  (command-output nil "git" "rev-parse" "--show-toplevel"))

(defn replace-binding!
  [file key value]
  (let [pattern (re-pattern (str "(?m)(^\\s*" key " = \")[^\"]*(\";)"))
        content (slurp file)]
    (when-not (re-find pattern content)
      (throw (ex-info "Nix binding not found" {:file file :key key})))
    (spit file (str/replace content pattern (str "$1" value "$2")))))

(defn replace-npm-deps-hash!
  [file value]
  (let [pattern #"(?m)^\s*npmDepsHash = .*;$"
        content (slurp file)]
    (when-not (re-find pattern content)
      (throw (ex-info "npmDepsHash binding not found" {:file file})))
    (spit file (str/replace content pattern (str "  npmDepsHash = " value ";")))))

(defn latest-release
  [root {:keys [owner repo]}]
  (command-output root "gh" "api" (str "repos/" owner "/" repo "/releases/latest")
                  "--jq" ".tag_name"))

(defn head-revision
  [root {:keys [owner repo]}]
  (-> (command-output root "git" "ls-remote" (str "https://github.com/" owner "/" repo ".git") "HEAD")
      (str/split #"\s+")
      first))

(defn github-revision
  [root source]
  (case (:mode source)
    :head (head-revision root source)
    :release (latest-release root source)))

(defn github-hash
  [root {:keys [owner repo]} revision]
  (-> (command-output root "nix" "run" "nixpkgs#nix-prefetch-github" "--"
                      "--no-fetch-submodules" "--json" "--rev" revision owner repo)
      (json/parse-string true)
      :hash))

(defn package-version
  [source-dir]
  (-> (slurp (fs/path source-dir "package.json"))
      (json/parse-string true)
      :version))

(defn clone-source!
  [tmp {:keys [owner repo]} revision mode]
  (let [source-dir (str (fs/path tmp "source"))
        url        (str "https://github.com/" owner "/" repo ".git")]
    (if (= mode :release)
      (run! tmp "git" "clone" "--depth" "1" "--branch" revision url source-dir)
      (run! tmp "git" "clone" "--depth" "1" url source-dir))
    source-dir))

(defn npm-package-name
  [package-path]
  (let [path (last (str/split package-path #"node_modules/"))]
    (if (str/starts-with? path "@")
      (str/join "/" (take 2 (str/split path #"/")))
      (last (str/split path #"/")))))

(defn npm-integrity
  [root package-path package]
  (command-output root "nix" "shell" "nixpkgs#nodejs" "--command" "npm" "view"
                  (str (npm-package-name package-path) "@" (get package "version"))
                  "dist.integrity"))

(defn complete-lock-integrities!
  [root lock-file]
  (let [lock     (json/parse-string (slurp lock-file) false)
        packages (get lock "packages")
        complete (reduce-kv
                  (fn [result path package]
                    (if (and (not (str/blank? path))
                             (get package "resolved")
                             (not (get package "integrity"))
                             (not (get package "link")))
                      (assoc result path (assoc package "integrity"
                                                (npm-integrity root path package)))
                      (assoc result path package)))
                  {}
                  packages)]
    (spit lock-file (str (json/generate-string (assoc lock "packages" complete)) "\n"))))

(defn generate-lock!
  [root source-dir lock-file]
  (fs/delete-if-exists (fs/path source-dir "package-lock.json"))
  (run! source-dir "nix" "shell" "nixpkgs#nodejs" "--command" "npm" "install"
        "--package-lock-only" "--ignore-scripts" "--legacy-peer-deps")
  (fs/copy (fs/path source-dir "package-lock.json") lock-file {:replace-existing true})
  (complete-lock-integrities! root lock-file))

(defn npm-source
  [root source]
  (let [version (command-output root "nix" "shell" "nixpkgs#nodejs" "--command" "npm" "view"
                                (:name source) "version")
        url     (command-output root "nix" "shell" "nixpkgs#nodejs" "--command" "npm" "view"
                                (str (:name source) "@" version) "dist.tarball")
        hash    (-> (command-output root "nix" "store" "prefetch-file" "--json" url)
                    (json/parse-string true)
                    :hash)]
    {:version version :url url :hash hash}))

(defn unpack-npm-source!
  [tmp {:keys [name]} version]
  (let [tarball    (command-output tmp "nix" "shell" "nixpkgs#nodejs" "--command" "npm" "pack"
                                   "--ignore-scripts" (str name "@" version) "--silent")
        source-dir (str (fs/path tmp "source"))]
    (fs/create-dirs source-dir)
    (run! tmp "tar" "-xzf" tarball "-C" source-dir "--strip-components=1")
    source-dir))

(defn update-npm-deps!
  [root {:keys [nix-name package-file]}]
  (replace-npm-deps-hash! package-file "lib.fakeHash")
  (let [result (process/sh {:dir root :continue true :out :string :err :string}
                           "nix" "build" (str ".#" nix-name) "--print-build-logs")
        output (str (:out result) (:err result))
        hash   (second (re-find #"got:\s+(sha256-[A-Za-z0-9+/=]+)" output))]
    (when (zero? (:exit result))
      (throw (ex-info "Expected npm dependency hash mismatch" {:package nix-name})))
    (when-not hash
      (throw (ex-info "Could not find npm dependency hash" {:package nix-name
                                                              :output  output})))
    (replace-npm-deps-hash! package-file (pr-str hash))
    (run! root "nix" "build" (str ".#" nix-name) "--print-build-logs")))

(defn update-github!
  [root {:keys [package-file lock-file source] :as package}]
  (let [revision (github-revision root source)
        hash     (github-hash root source revision)]
    (println "Updating" (:nix-name package) "to" revision)
    (replace-binding! package-file "rev" revision)
    (replace-binding! package-file "hash" hash)
    (if lock-file
      (fs/with-temp-dir [tmp]
        (let [source-dir (clone-source! tmp source revision (:mode source))]
          (replace-binding! package-file "version" (package-version source-dir))
          (generate-lock! root source-dir lock-file)))
      (replace-binding! package-file "version" (str/replace revision #"^v" "")))
    (when lock-file
      (update-npm-deps! root package))))

(defn update-npm!
  [root {:keys [package-file lock-file source] :as package}]
  (let [{:keys [version url hash]} (npm-source root source)]
    (println "Updating" (:nix-name package) "to" version)
    (replace-binding! package-file "version" version)
    (replace-binding! package-file "url" url)
    (replace-binding! package-file "hash" hash)
    (fs/with-temp-dir [tmp]
      (generate-lock! root (unpack-npm-source! tmp source version) lock-file))
    (update-npm-deps! root package)))

(defn update-package!
  [root package]
  (case (get-in package [:source :kind])
    :github (update-github! root package)
    :npm (update-npm! root package)))

(defn selected-packages
  [names]
  (if (empty? names)
    packages
    (do
      (doseq [name names]
        (when-not (contains? packages name)
          (throw (ex-info "Unknown Pi package" {:name name
                                                  :available (sort (keys packages))}))))
      (select-keys packages names))))

(defn -main
  [& names]
  (let [root (repo-root)]
    (doseq [[_ package] (selected-packages names)]
      (update-package! root package))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
