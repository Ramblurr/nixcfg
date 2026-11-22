(ns harness-plugins-sync.packs
  "Fetches Git plugin packs into immutable snapshots and promotes active links.

  Each pack lives under `<state-dir>/packs/<name>/`:
  - `repo.git/` bare Git repository
  - `revs/<sha>/` immutable unpacked archive of each revision
  - `staged` symlink updated by [[pull-pack!]]
  - `active` symlink updated atomically by [[promote-pack!]]"
  (:require
   [babashka.fs :as fs]
   [babashka.process :as proc]
   [clojure.string :as str])
  (:import
   java.nio.channels.FileChannel
   [java.nio.file Files LinkOption Path StandardCopyOption StandardOpenOption]
   java.security.MessageDigest))

(def ^:private ignored-digest-dirs
  #{".git" ".scratch-org" "node_modules" ".worktrees" ".direnv"})

(defn pack-dir
  "Returns the root state directory for `pack-name` under `state-dir`."
  [state-dir pack-name]
  (str (fs/path state-dir "packs" pack-name)))

(defn active-path
  "Returns the `active` symlink path for `pack-name`."
  [state-dir pack-name]
  (str (fs/path (pack-dir state-dir pack-name) "active")))

(defn staged-path
  "Returns the `staged` symlink path for `pack-name`."
  [state-dir pack-name]
  (str (fs/path (pack-dir state-dir pack-name) "staged")))

(defn with-pack-lock
  "Runs `f` while holding an advisory file lock on `pack-name`."
  [state-dir pack-name f]
  (let [dir       (pack-dir state-dir pack-name)
        _         (fs/create-dirs dir)
        lock-path (fs/path dir ".lock")]
    (with-open [ch (FileChannel/open lock-path
                                     (into-array StandardOpenOption
                                                 [StandardOpenOption/CREATE
                                                  StandardOpenOption/WRITE]))
                _lock (.lock ch)]
      (f))))

(defn atomic-symlink!
  "Atomically points symlink `link-path` at `target-str`."
  [link-path target-str]
  (let [parent (fs/parent link-path)
        _      (fs/create-dirs parent)
        tmp    (fs/path parent (str "." (fs/file-name link-path) ".tmp."
                                    (System/nanoTime)))]
    (try
      (Files/createSymbolicLink tmp (fs/path target-str)
                                (make-array java.nio.file.attribute.FileAttribute 0))
      (Files/move tmp (fs/path link-path)
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (catch Exception e
        (fs/delete-if-exists tmp)
        (throw e)))))

(defn read-symlink-target
  "Returns the raw symlink target string of `p`, or nil when `p` is not a symlink."
  [p]
  (let [path (fs/path p)]
    (when (Files/isSymbolicLink path)
      (str (Files/readSymbolicLink path)))))

(defn- rev-from-target [target]
  (when (and target (str/starts-with? target "revs/"))
    (subs target (count "revs/"))))

(defn- resolve-link-dir [link-path]
  (when-let [target (read-symlink-target link-path)]
    (let [tpath (fs/path target)]
      (str (fs/normalize (if (.isAbsolute tpath)
                           tpath
                           (fs/path (fs/parent link-path) tpath)))))))

(defn- run-cmd! [args opts]
  (let [res (proc/sh args (merge {:out :string :err :string :continue true} opts))]
    (when-not (zero? (:exit res))
      (throw (ex-info (str "command failed: " (str/join " " args)
                           (when-not (str/blank? (:err res))
                             (str ": " (str/trim (:err res)))))
                      {:code    :pack/git-failed
                       :details {:cmd args :exit (:exit res) :stderr (str/trim (:err res))}})))
    res))

(defn- prune-revs! [revs-dir keep-set retain-revs]
  (when (fs/exists? revs-dir)
    (let [now-ms (System/currentTimeMillis)
          entries (fs/list-dir revs-dir)]
      (doseq [e entries
              :let [nm (str (fs/file-name e))]
              :when (str/starts-with? nm ".tmp-")]
        (let [age-ms (- now-ms (.toMillis (fs/last-modified-time e)))]
          (when (> age-ms (* 60 60 1000))
            (fs/delete-tree e))))
      (let [old-revs (->> entries
                          (remove #(let [nm (str (fs/file-name %))]
                                     (or (str/starts-with? nm ".tmp-")
                                         (contains? keep-set nm))))
                          (sort-by #(.toMillis (fs/last-modified-time %)) >))]
        (doseq [e (drop retain-revs old-revs)]
          (fs/delete-tree e))))))

(defn pull-pack!
  "Fetches `pack-name` (`{:url :branch}`) into `state-dir`, unpacks `revs/<sha>`,
  and updates the `staged` symlink without modifying `active`.

  Options:
  | key           | description |
  |---------------|-------------|
  | `:retain-revs`| Number of old revisions to keep (default `2`) |
  | `:dry-run`    | When true, inspects without mutating disk |"
  [state-dir pack-name {:keys [url branch] :or {branch "main"}} {:keys [retain-revs dry-run] :or {retain-revs 2}}]
  (if dry-run
    {:pack    pack-name
     :url     url
     :branch  branch
     :status  :dry-run
     :staged  (rev-from-target (read-symlink-target (staged-path state-dir pack-name)))
     :active  (rev-from-target (read-symlink-target (active-path state-dir pack-name)))}
    (with-pack-lock state-dir pack-name
      (fn []
        (let [root     (pack-dir state-dir pack-name)
              repo-dir (str (fs/path root "repo.git"))
              revs-dir (str (fs/path root "revs"))
              prev-rev (rev-from-target (read-symlink-target (staged-path state-dir pack-name)))
              act-rev  (rev-from-target (read-symlink-target (active-path state-dir pack-name)))]
          (fs/create-dirs revs-dir)
          (when-not (fs/exists? repo-dir)
            (run-cmd! ["git" "init" "--quiet" "--bare" repo-dir] {}))
          (run-cmd! ["git" "-C" repo-dir "fetch" "--quiet" "--prune"
                     url (str "+refs/heads/" branch ":refs/heads/" branch)]
                    {})
          (let [rev     (str/trim (:out (run-cmd! ["git" "-C" repo-dir "rev-parse" (str "refs/heads/" branch)] {})))
                rev-dir (str (fs/path revs-dir rev))]
            (when-not (fs/exists? rev-dir)
              (let [tmp (str (fs/create-temp-dir {:dir revs-dir :prefix ".tmp-"}))]
                (try
                  (let [git-p (proc/process ["git" "-C" repo-dir "archive" rev] {:err :string})
                        tar-p (proc/process ["tar" "-x" "-C" tmp] {:in (:out git-p) :err :string})
                        tar-r @tar-p
                        git-r @git-p]
                    (when-not (and (zero? (:exit git-r)) (zero? (:exit tar-r)))
                      (throw (ex-info (str "failed to unpack " pack-name "@" rev)
                                      {:code    :pack/git-failed
                                       :details {:pack pack-name :rev rev
                                                 :git-err (:err git-r) :tar-err (:err tar-r)}}))))
                  (Files/move (fs/path tmp) (fs/path rev-dir)
                              (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE]))
                  (catch Exception e
                    (fs/delete-tree tmp)
                    (throw e)))))
            (when (not= prev-rev rev)
              (atomic-symlink! (staged-path state-dir pack-name) (str "revs/" rev)))
            (prune-revs! revs-dir (set (remove nil? [rev act-rev])) retain-revs)
            {:pack         pack-name
             :url          url
             :branch       branch
             :rev          rev
             :previous-rev prev-rev
             :changed?     (not= prev-rev rev)
             :status       (if (= prev-rev rev) :unchanged :fetched)}))))))

(defn content-digest
  "Computes a deterministic SHA-256 digest of files under `root-dir`, ignoring
  VCS and build directories."
  [root-dir]
  (let [root   (fs/path root-dir)
        digest (MessageDigest/getInstance "SHA-256")
        files  (atom [])]
    (letfn [(walk-dir [^Path dir]
              (doseq [entry (sort-by #(str (fs/file-name %)) (fs/list-dir dir))
                      :let [nm (str (fs/file-name entry))]]
                (cond
                  (Files/isSymbolicLink entry)
                  (swap! files conj [:link (str (.relativize root entry))
                                     (str (Files/readSymbolicLink entry))])

                  (Files/isDirectory entry (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                  (when-not (contains? ignored-digest-dirs nm)
                    (walk-dir entry))

                  (Files/isRegularFile entry (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                  (swap! files conj [:file (str (.relativize root entry)) entry]))))]
      (when (fs/exists? root)
        (walk-dir root))
      (doseq [[kind rel target] @files]
        (.update digest (.getBytes (str (name kind) "\0" rel "\0") "UTF-8"))
        (if (= :link kind)
          (.update digest (.getBytes (str target "\0") "UTF-8"))
          (let [bs (Files/readAllBytes ^Path target)]
            (.update digest bs)
            (.update digest (byte-array [0])))))
      (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest digest))))))

(defn promote-pack!
  "Promotes `pack-name`'s `active` symlink to `local-path` (when provided) or
  its `staged` snapshot, and keeps an existing `active` symlink when neither is
  set. `check`, when given, receives the candidate directory and throws to
  reject it: a rejected candidate leaves `active` alone and the result carries
  `:error`, or, with no `active` to keep, throws `:pack/rejected`. Choosing,
  checking, and switching hold the pack lock, so a concurrent pull cannot swap
  the candidate between check and switch.

  Returns a map describing the active pack target, source (`:git` or `:local`),
  revision or digest, the previous active revision, and whether `active` changed."
  [state-dir pack-name {:keys [local-path dry-run check]}]
  (when (and local-path (not (fs/directory? local-path)))
    (throw (ex-info (str "local pack directory does not exist: " local-path)
                    {:code :pack/not-found :details {:pack pack-name :path local-path}})))
  (let [act-link (active-path state-dir pack-name)
        stg-link (staged-path state-dir pack-name)
        promote
        (fn []
          (let [prev-target  (read-symlink-target act-link)
                local-abs    (when local-path (str (fs/absolutize (fs/normalize local-path))))
                [target dir] (cond
                               local-abs                    [local-abs local-abs]
                               (read-symlink-target stg-link) [(read-symlink-target stg-link)
                                                               (resolve-link-dir stg-link)])
                error        (when (and target check)
                               (try
                                 (check dir)
                                 nil
                                 (catch Exception e
                                   (or (ex-message e) (str e)))))
                [target dir] (if (and target (not error))
                               [target dir]
                               [prev-target (when prev-target (resolve-link-dir act-link))])
                changed?     (and (some? target) (not= prev-target target))
                rev          (rev-from-target target)]
            (cond
              (nil? target)
              (if error
                (throw (ex-info error {:code :pack/rejected :details {:pack pack-name}}))
                (throw (ex-info (str "pack '" pack-name "' has no fetched snapshot; run 'harness-plugins-sync sync' or 'pull' first")
                                {:code :pack/not-fetched :details {:pack pack-name}})))

              :else
              (do
                (when (and changed? (not dry-run))
                  (atomic-symlink! act-link target))
                (cond-> {:pack         pack-name
                         :source       (if rev :git :local)
                         :path         dir
                         :active       act-link
                         :rev          rev
                         :digest       (or rev (content-digest dir))
                         :previous-rev (rev-from-target prev-target)
                         :changed?     changed?
                         :status       (cond dry-run :dry-run changed? :promoted :else :unchanged)}
                  error (assoc :error error))))))]
    (if dry-run
      (promote)
      (with-pack-lock state-dir pack-name promote))))

(defn pack-status
  "Inspects `pack-name` under `state-dir` without making changes."
  [state-dir pack-name {:keys [url branch] :or {branch "main"}}]
  (let [act-link   (active-path state-dir pack-name)
        stg-link   (staged-path state-dir pack-name)
        act-target (read-symlink-target act-link)
        stg-target (read-symlink-target stg-link)
        act-rev    (rev-from-target act-target)
        stg-rev    (rev-from-target stg-target)
        act-dir    (resolve-link-dir act-link)
        source     (cond
                     (nil? act-target) :missing
                     act-rev           :git
                     :else             :local)]
    {:pack            pack-name
     :url             url
     :branch          branch
     :source          source
     :active-path     act-link
     :resolved-path   act-dir
     :active-rev      act-rev
     :staged-rev      stg-rev
     :pending-promote (boolean (and stg-target (not= act-target stg-target)))}))
