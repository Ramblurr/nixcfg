#!/usr/bin/env bb

(ns update
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
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

(defn package-file [pkg]
  (let [provider (str/replace-first (name pkg) #"^terraform-provider-" "")]
    (if (str/starts-with? (name pkg) "terraform-provider-")
      (str "pkgs/terraform-providers/" provider ".nix")
      (str "pkgs/" (name pkg) "/package.nix"))))

(defn get-packages-with-update-script []
  (->> (get-all-packages)
       (filter #(fs/exists? (package-file %)))
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
  (let [old (str/replace-first old-version #"^v" "")
        new (str/replace-first new-version #"^v" "")]
    (if (or (str/blank? old) (str/blank? new) (= old new))
      (str pkg ": update package")
      (format "%s: %s -> %s" pkg old new))))

(defn package-path [pkg]
  (if (str/starts-with? (name pkg) "terraform-provider-")
    (package-file pkg)
    (str "pkgs/" (name pkg))))

(defn restore-changes [path]
  (log :info (str "Restoring tracked changes in " path))
  (shell "git" "checkout" "HEAD" "--" path))

(defn commit-changes [path message no-commit]
  (shell "git" "add" "-u" "--" path)
  (if (zero? (:exit (sh {:continue true} "git" "diff" "--cached" "--quiet" "--" path)))
    (log :warning (str "No changes to stage in " path))
    (if no-commit
      (log :info (str "Changes staged in " path " (not committed)"))
      (do
        (log :info (str "Committing changes: " message))
        (shell "git" "commit" "-m" message "--" path)))))

(defn script-package
  "A package with a passthru.updateScript, named by its flake export."
  [pkg]
  {:name   pkg
   :path   (package-path pkg)
   :update #(run-update-script pkg)})

(defn pi-packages
  "Packages that pkgs/pi/update.bb updates, named by their flake exports."
  []
  (->> (json/parse-string (:out (shell {:out :string} "bb" "pkgs/pi/update.bb" "--list")))
       (map (fn [[dir export]]
              {:name   export
               :path   (str "pkgs/pi/" dir)
               :update (fn []
                         (log :info (str "Updating " export))
                         (zero? (:exit (shell {:continue true} "bb" "pkgs/pi/update.bb" dir))))}))
       (sort-by :name)))

(defn update-package
  "Update, then commit or restore one package. Returns its name when it failed."
  [{:keys [name path update]} {:keys [no-commit keep-going]}]
  (let [old-version (fetch-package-version name)]
    (cond
      (update)
      (do (commit-changes path (commit-message name old-version (fetch-package-version name)) no-commit)
          nil)

      keep-going
      (do (restore-changes path)
          name)

      :else
      (do (log :error (str "Aborting: failed to update " name))
          (System/exit 1)))))

(defn -main [& args]
  (let [changes (tracked-file-changes)]
    (when (seq changes)
      (log :error "Aborting: tracked files have staged or unstaged changes")
      (doseq [change changes]
        (println (str "  " change)))
      (System/exit 1)))
  (let [{:keys [opts args]} (cli/parse-args args
                                            {:spec {:no-commit  {:coerce :boolean
                                                                 :desc "Stage changes but don't commit"}
                                                    :keep-going {:coerce :boolean
                                                                 :desc "Restore a failed package and continue, then exit non-zero"}}})
        packages-arg (vec args)
        available (concat (map script-package (get-packages-with-update-script))
                          (pi-packages))
        unmatched (remove (set (map :name available)) packages-arg)
        packages-to-update (if (empty? packages-arg)
                             available
                             (filterv #(some #{(:name %)} packages-arg) available))]
    (when (seq unmatched)
      (log :error (str "No updatable package named: " (str/join " " unmatched)))
      (System/exit 1))
    (log :info (format "Found %d packages to update" (count packages-to-update)))
    (let [failed (filterv some? (mapv #(update-package % opts) packages-to-update))]
      (when (seq failed)
        (log :error (str "Failed packages: " (str/join " " failed)))
        (System/exit 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
