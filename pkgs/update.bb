#!/usr/bin/env bb

(ns update
  (:require [babashka.cli :as cli]
            [babashka.process :refer [shell sh]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn log [level msg]
  (println (format "[%s] %s" (str/upper-case (name level)) msg)))

(defn get-all-packages []
  (-> (sh "nix" "eval" ".#packages.x86_64-linux" "--apply" "builtins.attrNames" "--json")
      :out
      (json/parse-string true)))

(defn has-update-script? [pkg]
  (let [result (sh {:continue true}
                   "nix" "eval" "--raw" (str ".#" pkg ".passthru.updateScript.type"))]
    (zero? (:exit result))))

(defn get-packages-with-update-script []
  (->> (get-all-packages)
       (filter has-update-script?)
       vec))

(defn run-update-script [pkg]
  (log :info (str "Updating " pkg))
  (try
    (shell "nix" "run" (str ".#" pkg ".passthru.updateScript"))
    true
    (catch Exception e
      (log :error (str "Failed to run update script for " pkg ": " (.getMessage e)))
      false)))

(defn tracked-file-changes []
  (->> (sh "git" "status" "--short" "--untracked-files=no")
       :out
       str/split-lines
       (remove str/blank?)
       vec))

(defn fetch-package-version [pkg]
  (-> (sh "nix" "eval" "--raw" (str ".#" pkg ".version"))
      :out
      str/trim))

(defn commit-message [pkg old-version new-version]
  (format "%s: %s -> %s"
          pkg
          (str/replace-first old-version #"^v" "")
          (str/replace-first new-version #"^v" "")))

(defn commit-changes [pkg old-version new-version no-commit]
  (shell "git" "add" "-u" "pkgs/")
  (if (zero? (:exit (sh {:continue true} "git" "diff" "--cached" "--quiet")))
    (log :warning "No changes to stage")
    (if no-commit
      (log :info "Changes staged (not committed)")
      (let [message (commit-message pkg old-version new-version)]
        (log :info (str "Committing changes: " message))
        (shell "git" "commit" "-m" message)))))

(defn -main [& args]
  (let [changes (tracked-file-changes)]
    (when (seq changes)
      (log :error "Aborting: tracked files have staged or unstaged changes")
      (doseq [change changes]
        (println (str "  " change)))
      (System/exit 1)))
  (let [{:keys [opts args]} (cli/parse-args args
                                            {:spec {:no-commit {:coerce :boolean
                                                                :desc "Stage changes but don't commit"}}})
        no-commit (:no-commit opts)
        packages-arg (vec args)
        packages-with-updatescript (get-packages-with-update-script)
        packages-to-update (if (empty? packages-arg)
                             packages-with-updatescript
                             (filterv #(some #{%} packages-arg) packages-with-updatescript))]
    (log :info (format "Found %d packages to update" (count packages-to-update)))
    (doseq [pkg packages-to-update]
      (let [old-version (fetch-package-version pkg)]
        (if (run-update-script pkg)
          (commit-changes pkg old-version (fetch-package-version pkg) no-commit)
          (do
            (log :error (str "Aborting: failed to update " pkg))
            (System/exit 1)))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
