#!/usr/bin/env bb
(ns scripts.reforge-move
  "Relocates existing Git checkouts from an explicit JSON manifest. Dry-run by default."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def usage
  "Usage: bb scripts/reforge_move.clj --manifest FILE [--dry-run]
       bb scripts/reforge_move.clj --manifest FILE --apply --journal NEW-FILE

Manifest: {\"moves\":[{\"source\":\"/absolute/path\",\"destination\":\"/absolute/path\",
                     \"remote\":\"origin\",\"expected_remote\":\"https://host/owner/repo.git\"}]}
The remote fields are optional but must be supplied together.

Dry-run prints JSON and changes no repositories. Apply skips active process cwd
paths, rejects other preflight errors, and requires a new durable JSONL journal.
Same-filesystem renames only; never overwrite, copy, clean, prune, fetch or push.
Repairs linked worktrees, preserves missing registrations and checks submodules.
No compatibility symlinks or external application configuration edits are made.
Quiesce background users without a cwd in the checkout before apply. /proc checks
cannot find every consumer. After interruption inspect the journal and filesystem;
there is no automatic resume. Roll back by renaming back and repairing worktrees.")

(defn- capture [argv]
  (let [{:keys [exit out]} (apply process/shell {:out :string :err :string :continue true
                                                 :extra-env {"GIT_OPTIONAL_LOCKS" "0"}}
                                  argv)]
    (when-not (zero? exit)
      (throw (ex-info "Command failed" {:program (first argv) :exit exit})))
    out))

(defn- git [directory & args]
  (capture (into ["git" "-C" (str directory)] args)))

(defn- under? [root path]
  (or (= root path) (str/starts-with? path (str root "/"))))

(defn- relocate [source destination path]
  (if (under? source path)
    (str destination (subs path (count source)))
    path))

(defn- present? [path]
  (or (fs/exists? path) (fs/sym-link? path)))

(defn- digest [text]
  (->> (.digest (java.security.MessageDigest/getInstance "SHA-256")
                (.getBytes text "UTF-8"))
       (map #(format "%02x" (bit-and 255 %)))
       (apply str)))

(defn- worktrees [source]
  (for [record (str/split (git source "worktree" "list" "--porcelain" "-z") #"\u0000\u0000")
        :when (seq record)]
    (into {} (for [field (str/split record #"\u0000")
                   :let [[key value] (str/split field #" " 2)]]
               [(keyword key) (or value true)]))))

(defn- checkout-state [path]
  {:head (str/trim (git path "rev-parse" "HEAD"))
   :status (git path "status" "--porcelain=v1" "-z" "--untracked-files=all")
   :tracked-diff (digest (str (git path "diff" "--binary" "--no-ext-diff" "--no-textconv")
                              (git path "diff" "--cached" "--binary" "--no-ext-diff" "--no-textconv")))})

(defn- submodule-paths [path]
  (remove str/blank?
          (str/split (git path "submodule" "foreach" "--quiet" "--recursive"
                          "printf '%s\\0' \"$displaypath\"") #"\u0000")))

(defn- snapshot [source]
  (let [trees (vec (worktrees source))
        live (filter #(fs/directory? (:worktree %)) trees)]
    {:inode (str/trim (capture ["stat" "-c" "%d:%i" "--" source]))
     :refs (git source "for-each-ref" "--format=%(refname) %(objectname) %(symref)")
     :config (digest (slurp (fs/file source ".git/config")))
     :worktrees trees
     :checkouts (into {} (for [{:keys [worktree]} live]
                           [worktree (checkout-state worktree)]))
     :submodules (into {} (for [{:keys [worktree]} live
                                submodule (submodule-paths worktree)
                                :let [path (str (fs/path worktree submodule))]]
                            [path (checkout-state path)]))}))

(defn- process-cwds []
  (reduce (fn [result path]
            (try
              (let [cwd (str (fs/read-link (fs/path path "cwd")))]
                (update result :processes conj {:pid (str (fs/file-name path)) :cwd cwd}))
              (catch Exception error
                (case (.getSimpleName (class error))
                  "AccessDeniedException" (update result :unreadable inc)
                  "NoSuchFileException" result
                  (throw error)))))
          {:processes [] :unreadable 0}
          (filter #(re-matches #"[0-9]+" (str (fs/file-name %))) (fs/list-dir "/proc"))))

(defn- errors [entry]
  (let [{:keys [source destination remote expected_remote]} entry
        ancestor (first (filter fs/exists? (iterate fs/parent (fs/path destination))))]
    (cond-> []
      (or (not (fs/absolute? source)) (not (fs/absolute? destination))
          (not= source (str (fs/normalize source)))
          (not= destination (str (fs/normalize destination))))
      (conj "Paths must be absolute and normalized")
      (or (not (fs/directory? (fs/path source ".git")))
          (fs/sym-link? (fs/path source ".git")))
      (conj "Source must be a main checkout with an embedded .git directory")
      (not= source (.getCanonicalPath (io/file source)))
      (conj "Source contains a symlink")
      (present? destination) (conj "Destination already exists")
      (not= (str ancestor) (.getCanonicalPath (io/file (str ancestor))))
      (conj "Destination ancestor contains a symlink")
      (not= (str/trim (capture ["stat" "-c" "%d" "--" source]))
            (str/trim (capture ["stat" "-c" "%d" "--" (str ancestor)])))
      (conj "Cross-filesystem move is forbidden")
      (not= source (str/trim (git source "rev-parse" "--show-toplevel")))
      (conj "Source is not the checkout root")
      (or (and remote (not expected_remote)) (and expected_remote (not remote)))
      (conj "Both remote and expected_remote are required")
      (and remote expected_remote
           (not= expected_remote (str/trim (git source "remote" "get-url" remote))))
      (conj "Expected remote does not match; complete the approved remote update first")
      (seq (concat (fs/glob (fs/path source ".git") "*.lock")
                   (mapcat #(when (fs/directory? (fs/path source ".git" %))
                              (fs/glob (fs/path source ".git" %) "**/*.lock"))
                           ["refs" "worktrees" "modules"])))
      (conj "Git lock files exist")
      (fs/exists? (fs/path source ".git/objects/info/alternates"))
      (conj "Object alternates require a separate path audit"))))

(defn- inspect-entry [processes entry]
  (try
    (let [problems (errors entry)
          state (when (empty? problems) (snapshot (:source entry)))
          roots (cons (:source entry) (map :worktree (:worktrees state)))
          active (filterv (fn [{:keys [cwd]}] (some #(under? % cwd) roots)) processes)]
      (assoc entry :errors problems :active active :snapshot state))
    (catch Exception error
      (assoc entry :errors [(ex-message error)] :active []))))

(defn plan
  "Inspects manifest `moves` and process working directories without changing checkouts."
  [moves]
  (when-not (seq moves)
    (throw (ex-info "Manifest has no moves" {})))
  (let [paths (mapcat (juxt :source :destination) moves)]
    (when-not (every? #(and (string? %) (not (str/blank? %))) paths)
      (throw (ex-info "Every move needs source and destination paths" {})))
    (doseq [[i path] (map-indexed vector paths)
            other (drop (inc i) paths)]
      (when (or (under? path other) (under? other path))
        (throw (ex-info "Manifest paths collide or overlap" {:paths [path other]})))))
  (let [{:keys [processes unreadable]} (process-cwds)]
    {:mode "dry-run"
     :unreadable-processes unreadable
     :moves (mapv #(inspect-entry processes %) moves)}))

(defn- relocated-snapshot [state source destination]
  (-> state
      (update :worktrees (fn [trees]
                           (mapv #(if (contains? (:checkouts state) (:worktree %))
                                    (update % :worktree (partial relocate source destination))
                                    %) trees)))
      (update :checkouts (fn [states]
                           (into {} (map (fn [[path value]]
                                           [(relocate source destination path) value]) states))))
      (update :submodules (fn [states]
                            (into {} (map (fn [[path value]]
                                            [(relocate source destination path) value]) states))))))

(defn- record! [journal event]
  (spit (str journal) (str (json/generate-string
                            (assoc event :time (str (java.time.Instant/now)))) "\n") :append true)
  (capture ["sync" "--" (str journal) (str (fs/parent (fs/absolutize journal)))]))

(defn- move-one! [journal {:keys [source destination] before :snapshot :as entry}]
  (let [fresh (inspect-entry (:processes (process-cwds)) (select-keys entry [:source :destination :remote :expected_remote]))]
    (when (or (seq (:errors fresh)) (seq (:active fresh)) (not= before (:snapshot fresh)))
      (throw (ex-info "Source changed or became active; nothing moved for this entry" {:source source}))))
  (record! journal {:event "intent" :source source :destination destination :before before})
  (fs/create-dirs (fs/parent destination))
  (capture ["mv" "--no-copy" "--update=none-fail" "--no-target-directory" "--" source destination])
  (when (or (present? source) (not= (:inode before)
                                    (str/trim (capture ["stat" "-c" "%d:%i" "--" destination]))))
    (throw (ex-info "Rename did not produce the expected inode" {:source source})))
  (record! journal {:event "renamed" :source source :destination destination})
  (let [linked (remove #(= source (:worktree %)) (:worktrees before))
        live (filter #(contains? (:checkouts before) (:worktree %)) linked)]
    (when (seq live)
      (apply git destination "worktree" "repair"
             (map #(relocate source destination (:worktree %)) live))))
  (when-not (= (relocated-snapshot before source destination) (snapshot destination))
    (throw (ex-info "Post-move validation failed; inspect journal before recovery" {:destination destination})))
  (record! journal {:event "verified" :source source :destination destination}))

(defn apply-plan!
  "Applies an inspected plan with a new `journal` file. Skips active checkouts.

  Stops on errors or drift. Does not automatically resume or roll back."
  [inspected journal]
  (when (some #(seq (:errors %)) (:moves inspected))
    (throw (ex-info "Resolve preflight errors before apply" {})))
  (when (present? journal)
    (throw (ex-info "Journal already exists; inspect it before retrying" {})))
  (java.nio.file.Files/createFile (fs/path journal)
                                  (make-array java.nio.file.attribute.FileAttribute 0))
  (record! journal {:event "start" :moves (mapv #(select-keys % [:source :destination]) (:moves inspected))})
  (doseq [entry (:moves inspected)]
    (if (seq (:active entry))
      (record! journal {:event "skipped-active" :source (:source entry) :active (:active entry)})
      (move-one! journal entry))))

(defn -main
  "Prints a dry-run report, or applies the manifest only with explicit `--apply`."
  [& args]
  (let [{:keys [opts args]} (cli/parse-args args {:restrict true
                                                  :spec {:manifest {:coerce :string}
                                                         :journal {:coerce :string}
                                                         :apply {:coerce :boolean}
                                                         :dry-run {:coerce :boolean}
                                                         :help {:coerce :boolean}}})]
    (when (seq args) (throw (ex-info "Unexpected positional arguments" {})))
    (if (:help opts)
      (println usage)
      (do
        (when (or (not (:manifest opts)) (and (:apply opts) (:dry-run opts))
                  (and (:apply opts) (not (:journal opts))))
          (throw (ex-info usage {})))
        (let [inspected (plan (:moves (json/parse-string (slurp (:manifest opts)) true)))]
          (if (:apply opts)
            (apply-plan! inspected (:journal opts))
            (println (json/generate-string
                      (update inspected :moves
                              #(mapv (fn [entry]
                                       (-> entry
                                           (assoc :worktrees (get-in entry [:snapshot :worktrees]))
                                           (assoc :initialized-submodules (keys (get-in entry [:snapshot :submodules])))
                                           (dissoc :snapshot))) %))
                      {:pretty true}))))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))