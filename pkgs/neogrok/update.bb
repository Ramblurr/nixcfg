#!/usr/bin/env bb

(ns neogrok.update
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn command-output [& args]
  (-> (apply process/shell {:out :string} args) :out str/trim))

(defn replace-value [source pattern value]
  (when-not (= 1 (count (re-seq pattern source)))
    (throw (ex-info "Expected one matching Nix binding" {:pattern (str pattern)})))
  (str/replace source pattern (fn [[_ prefix suffix]] (str prefix (pr-str value) suffix))))

(defn updated-package [source {:keys [version revision source-hash yarn-hash]}]
  (-> source
      (replace-value #"(version = )\"[^\"]*\"(;)" version)
      (replace-value #"(rev = )\"[^\"]*\"(;)" revision)
      (replace-value #"(?s)(src = fetchFromGitHub \{.*?hash = )\"[^\"]*\"(;)" source-hash)
      (replace-value #"(?s)(offlineCache = yarn.fetchYarnBerryDeps \{.*?hash = )\"[^\"]*\"(;)" yarn-hash)))

(defn -main [& args]
  (if (= ["--help"] (vec args))
    (println "Usage: bb pkgs/neogrok/update.bb [TAG-OR-COMMIT]\nDefaults to the latest GitHub release. Updates source and Yarn hashes; builds with tests.\nRequires git, gh and nix. Run from the nixcfg checkout.")
    (do
      (when (> (count args) 1)
        (throw (ex-info "Expected at most one tag or commit" {:args args})))
      (let [root     (command-output "git" "rev-parse" "--show-toplevel")
            package  (str (fs/path root "pkgs/neogrok/package.nix"))
            hashes   (str (fs/path root "pkgs/neogrok/missing-hashes.json"))
            original (slurp package)
            old-hashes (slurp hashes)
            ref      (or (first args)
                         (command-output "gh" "api" "repos/isker/neogrok/releases/latest" "--jq" ".tag_name"))
            revision (command-output "gh" "api" (str "repos/isker/neogrok/commits/" ref) "--jq" ".sha")
            fetcher  (str (command-output "nix" "build" "--no-link" "--print-out-paths"
                                          (str root "#neogrok.yarnFetcher"))
                          "/bin/yarn-berry-fetcher")]
        (when-not (re-matches #"[0-9a-f]{40}" revision)
          (throw (ex-info "Expected a full Git commit" {:revision revision})))
        (fs/with-temp-dir [tmp]
          (let [checkout (str (fs/path tmp "source"))
                missing  (str (fs/path tmp "missing-hashes.json"))]
            (process/shell "git" "init" "-q" checkout)
            (process/shell "git" "-C" checkout "fetch" "--depth=1" "https://github.com/isker/neogrok.git" revision)
            (process/shell "git" "-C" checkout "checkout" "-q" "--detach" "FETCH_HEAD")
            (spit missing (str (command-output fetcher "missing-hashes" (str (fs/path checkout "yarn.lock"))) "\n"))
            (let [version     (:version (json/parse-string (slurp (fs/path checkout "package.json")) true))
                  source-hash (:hash (json/parse-string
                                      (command-output "nix" "store" "prefetch-file" "--json" "--unpack"
                                                      (str "https://github.com/isker/neogrok/archive/" revision ".tar.gz")) true))
                  yarn-hash   (command-output fetcher "prefetch" (str (fs/path checkout "yarn.lock")) missing)
                  updated     (updated-package original {:version version :revision revision
                                                         :source-hash source-hash :yarn-hash yarn-hash})]
              (when-not (re-matches #"sha256-[A-Za-z0-9+/=]+" yarn-hash)
                (throw (ex-info "Fetcher did not return an SRI hash" {:output yarn-hash})))
              (try
                (spit package updated)
                (fs/copy missing hashes {:replace-existing true})
                (process/shell "nix" "build" "--no-link" (str root "#neogrok"))
                (println "Updated Neogrok to" version "at" revision)
                (catch Exception e
                  (spit package original)
                  (spit hashes old-hashes)
                  (throw e))))))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
