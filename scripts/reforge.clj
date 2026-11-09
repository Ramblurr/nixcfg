#!/usr/bin/env bb
(ns scripts.reforge
  "Creates Forgejo repositories and moves local GitHub origins to upstream."
  (:require [babashka.cli :as cli]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def usage
  "Usage: bb scripts/reforge.clj --dir DIR --source-owner OWNER
       --host HOST --owner ORGANIZATION --branches current|all [OPTIONS]

Dry-run by default: reads local Git and GitHub metadata, then prints commands.
--apply                     Execute the complete preflighted plan; stop on error.
--exclude DIRECTORY         Exclude an immediate child directory; repeatable.
--rename SOURCE=DESTINATION  Override a destination repository name; repeatable.
--unpublished private|public Required for repos with no origin or upstream.
--help                      Show this help.

Requires authenticated gh and fj, plus Git push credentials. HOST is a hostname
(optional port), not a URL. Source is github.com; target uses HTTPS.
Names come from GitHub remotes, not directory names. Duplicate targets, bare
repos, shallow clones, conflicting remotes and custom push settings are refused.
Already-migrated origins are skipped. No commits, fetches, force pushes, remote
branch copies, LFS migration, issues or releases. 'all' pushes LOCAL branches and
tags only; 'current' pushes only the checked-out branch. Uncommitted files stay
local. Unpublished repos get an origin, not a fictitious GitHub upstream.

Creation uses fj org repo create. If creation succeeds but a later step fails,
inspect the partial state before retrying; existing targets are not overwritten.")

(defn- fail [message]
  (throw (ex-info message {})))

(def cli-spec
  {:restrict true
   :spec {:dir {:coerce :string}
          :source-owner {:coerce :string}
          :host {:coerce :string}
          :owner {:coerce :string}
          :branches {:coerce :string}
          :unpublished {:coerce :string}
          :exclude {:coerce []}
          :rename {:coerce []}
          :apply {:coerce :boolean}
          :help {:coerce :boolean}}})

(defn- options [args]
  (let [{:keys [opts args]} (cli/parse-args args cli-spec)]
    (when (seq args) (fail (str "Unexpected arguments: " (pr-str args))))
    (-> opts
        (update :exclude set)
        (update :rename
                (fn [renames]
                  (into {} (map (fn [value]
                                  (let [[_ source target]
                                        (re-matches #"([A-Za-z0-9_.-]+)=([A-Za-z0-9_][A-Za-z0-9_.-]*)" value)]
                                    (when-not source
                                      (fail "--rename requires SOURCE=DESTINATION"))
                                    [source target]))
                                renames)))))))

(defn- capture [argv]
  (let [{:keys [exit out err]}
        (apply process/shell {:out :string :err :string :continue true} argv)]
    (when-not (zero? exit)
      (fail (str (pr-str argv) " exited " exit ": " (str/trim err))))
    (str/trim out)))

(defn- git [dir & args]
  (capture (into ["git" "-C" (str dir)] args)))

(defn- github-repo [url owner]
  (let [[_ actual-owner repo]
        (re-matches #"(?:https://github\.com/|git@github\.com:|ssh://git@github\.com/)([^/]+)/([^/]+?)(?:\.git)?/?" url)]
    (when (= (str/lower-case (or actual-owner "")) (str/lower-case owner))
      repo)))

(defn- remote-url [dir remotes remote]
  (when (contains? remotes remote)
    (let [urls (str/split-lines (git dir "remote" "get-url" "--all" remote))
          pushes (str/split-lines (git dir "remote" "get-url" "--push" "--all" remote))]
      (when-not (and (= 1 (count urls)) (= urls pushes))
        (fail (str dir ": multiple URLs or custom push URL for " remote)))
      (first urls))))

(defn- repo-plan [opts dir]
  (let [{:keys [host owner source-owner branches unpublished]} opts
        remotes (set (str/split-lines (git dir "remote")))
        origin (remote-url dir remotes "origin")
        upstream (remote-url dir remotes "upstream")
        source (or origin upstream)
        repo (if source (github-repo source source-owner) (.getName dir))
        target-prefix (str "https://" host "/" owner "/")]
    (if (and origin (str/starts-with? origin target-prefix))
      {:directory (str dir) :skip "already has a destination origin"}
      (do
        (when-not repo (fail (str dir ": not a GitHub remote for " source-owner)))
        (when (and origin upstream)
          (fail (str dir ": upstream already exists; rename it first")))
        (when (= "true" (git dir "rev-parse" "--is-shallow-repository"))
          (fail (str dir ": shallow clone; complete its history first")))
        (when (seq (filter #(re-find #"(?i)^remote\..*\.(push|mirror|receivepack)=" %)
                           (str/split-lines (git dir "config" "--list"))))
          (fail (str dir ": custom push configuration requires manual review")))
        (let [metadata (when source
                         (json/parse-string
                          (capture ["gh" "repo" "view" (str source-owner "/" repo)
                                    "--json" "name,isPrivate,defaultBranchRef"]) true))
              _ (when (and source (not (boolean? (:isPrivate metadata))))
                  (fail (str dir ": missing GitHub visibility")))
              _ (when (and (nil? source) (nil? unpublished))
                  (fail (str dir ": no GitHub remote; choose --unpublished private|public")))
              private? (if source (:isPrivate metadata) (= "private" unpublished))
              branch (git dir "symbolic-ref" "--short" "HEAD")
              _ (git dir "rev-parse" "--verify" "HEAD")
              target-name (get (:rename opts) repo repo)
              target (str target-prefix target-name ".git")
              full-name (str owner "/" target-name)
              fj ["fj" "-H" host]
              git-cmd ["git" "-C" (str dir)]
              refs (if (= branches "all")
                     ["refs/heads/*:refs/heads/*" "refs/tags/*:refs/tags/*"]
                     [(str "refs/heads/" branch ":refs/heads/" branch)])
              default-branch (if (= branches "current") branch
                                 (or (get-in metadata [:defaultBranchRef :name]) branch))]
          (git dir "show-ref" "--verify" (str "refs/heads/" default-branch))
          {:directory (str dir)
           :repo target-name
           :visibility (if private? "private" "public")
           :commands
           (vec (concat
                 [(cond-> (into fj ["org" "repo" "create" owner target-name])
                    private? (conj "--private"))]
                 [(into git-cmd (concat ["push" "--atomic" target] refs))
                  (into fj ["repo" "edit" full-name "--default-branch" default-branch])]
                 (when origin [(into git-cmd ["remote" "rename" "origin" "upstream"])])
                 [(into git-cmd ["remote" "add" "origin" target])
                  (into git-cmd ["ls-remote" "origin"])]))})))))

(defn- plan [opts]
  (doseq [key [:dir :source-owner :host :owner :branches]]
    (when (str/blank? (get opts key)) (fail (str "Required: --" (name key)))))
  (doseq [key [:source-owner :owner]]
    (when-not (re-matches #"[A-Za-z0-9_.-]+" (get opts key))
      (fail (str "Invalid --" (name key)))))
  (when-not (re-matches #"[A-Za-z0-9.-]+(?::[0-9]+)?" (:host opts))
    (fail "--host must be a hostname, optionally with a port"))
  (when-not (contains? #{"current" "all"} (:branches opts))
    (fail "--branches must be current or all"))
  (when-not (contains? #{nil "private" "public"} (:unpublished opts))
    (fail "--unpublished must be private or public"))
  (let [root (.getCanonicalFile (io/file (:dir opts)))]
    (when-not (.isDirectory root) (fail "--dir is not a directory"))
    (let [entries
          (mapv (fn [dir]
                  (cond
                    ((:exclude opts) (.getName dir))
                    {:directory (str dir) :skip "explicit exclusion"}
                    (.exists (io/file dir ".git")) (repo-plan opts dir)
                    (and (.exists (io/file dir "HEAD"))
                         (.isDirectory (io/file dir "objects")))
                    (fail (str dir ": bare repository; exclude or handle manually"))
                    :else {:directory (str dir) :skip "not a working repository"}))
                (sort-by str (filter #(.isDirectory %) (.listFiles root))))
          duplicates (->> entries (keep :repo) frequencies
                          (filter (fn [[_ n]] (> n 1))) (map first))]
      (when (seq duplicates)
        (fail (str "Duplicate destination repositories; exclude extra copies: "
                   (str/join ", " duplicates))))
      entries)))

(defn -main
  "Prints a migration plan, or executes it with `--apply`. Stops on any failure."
  [& args]
  (let [opts (options args)]
    (if (:help opts)
      (println usage)
      (let [entries (plan opts)]
        (pprint/pprint entries)
        (when (:apply opts)
          (process/shell "fj" "-H" (:host opts) "whoami")
          (doseq [{:keys [directory commands]} entries
                  command commands]
            (println "RUN" directory (pr-str command))
            (flush)
            (let [result (apply process/shell {:continue true} command)]
              (println "EXIT" (:exit result))
              (when-not (zero? (:exit result))
                (fail (str "Stopped at " directory "; inspect partial state before retrying"))))))))))

(when (= *file* (System/getProperty "babashka.file"))
  (try
    (apply -main *command-line-args*)
    (catch Exception e
      (binding [*out* *err*] (println "reforge:" (ex-message e)))
      (System/exit 1))))