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
    (catch Exception e
      (log :warning (str "Failed to run update script for " pkg ": " (.getMessage e))))))

(defn commit-changes [no-commit]
  (try
    (shell "git" "add" "-u" "pkgs/")
    (if no-commit
      (log :info "Changes staged (not committed)")
      (do
        (log :info "Committing changes")
        (shell "git" "commit" "-m" "update(pkgs): Update sources of all downstream packages")))
    (catch Exception _
      (log :warning "No changes to stage"))))

(defn -main [& args]
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
      (run-update-script pkg))
    (commit-changes no-commit)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
