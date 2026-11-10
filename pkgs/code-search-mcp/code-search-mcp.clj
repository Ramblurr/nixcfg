#!/usr/bin/env bb

(require '[babashka.deps :as deps]
         '[babashka.fs :as fs]
         '[clojure.edn :as edn])

(when-not (System/getenv "CODE_SEARCH_MCP_PACKAGED")
  (deps/add-deps
   (edn/read-string
    (slurp (str (fs/path (fs/parent *file*) "deps.edn"))))))

#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns ^{:clj-kondo/config '{:lint-as {babashka.sqlite/with-conn clojure.core/let}}}
 code-search.mcp
  "Read-only Streamable HTTP MCP over the existing Neogrok search service."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as client]
            [babashka.process :as process]
            [babashka.sqlite :as sqlite]
            [cheshire.core :as json]
            [clojure.string :as str]
            [mcp-toolkit.json-rpc :as rpc]
            [mcp-toolkit.protocol :as protocol]
            [mcp-toolkit.server :as mcp]
            [org.httpkit.server :as http]
            [promesa.core :as p])
  (:import [java.net URI]
           [java.util.concurrent CompletableFuture ExecutionException Future Semaphore TimeUnit TimeoutException]))

(def protocol-versions ["2025-06-18" "2025-11-25"])
(def max-sessions 64)
(def session-idle-ms (* 30 60 1000))
(def max-request-bytes 65536)
(def max-response-bytes (* 2 1024 1024))
(def upstream-timeout-ms 10000)

(def ^:dynamic *cancelled* nil)
(def ^:dynamic *deadline* nil)

(defn await-result
  "Wait within the request deadline, checking its cancellation flag."
  [pending deadline]
  (loop []
    (when (and *cancelled* @*cancelled*)
      (throw (ex-info "Request cancelled" {})))
    (when (>= (System/nanoTime) deadline)
      (throw (ex-info "Operation deadline exceeded" {})))
    (let [result (try
                   [:done (.get ^Future pending 20 TimeUnit/MILLISECONDS)]
                   (catch TimeoutException _ [:waiting])
                   (catch ExecutionException e
                     (let [cause (.getCause e)]
                       (throw (if (instance? clojure.lang.ExceptionInfo cause)
                                cause
                                (ex-info (if (instance? java.net.http.HttpTimeoutException cause)
                                           "Operation deadline exceeded"
                                           "Operation unavailable") {}))))))]
      (if (= :waiting (first result)) (recur) (second result)))))

(defn- utf8-text [bytes]
  (try
    (str (.decode (.newDecoder (java.nio.charset.Charset/forName "UTF-8"))
                  (java.nio.ByteBuffer/wrap bytes)))
    (catch java.nio.charset.CharacterCodingException _
      (throw (ex-info "Unsupported UTF-8 data" {})))))

(defn- indexed-file-provenance [file]
  (let [revision (try
                   (some (fn [pair]
                           (let [[name value] (str/split pair #"=" 2)]
                             (when (and (= "id" name) value (re-matches #"(?:[0-9a-f]{40}|[0-9a-f]{64})" value)) value)))
                         (str/split (or (.getRawQuery (URI. (or (get file "fileUrl") ""))) "") #"&"))
                   (catch Exception _ nil))]
    (if revision (assoc file "revision" revision)
        (assoc file "revisionWarning" "Backend did not supply an immutable revision"))))

(defn neogrok-request
  "Call Neogrok with one deadline for headers/body, cancellation and a byte cap."
  [base endpoint payload]
  (let [deadline (min (or *deadline* Long/MAX_VALUE) (+ (System/nanoTime) (* upstream-timeout-ms 1000000)))
        ;; raw preserves the cancellable sendAsync future, not a dependent future.
        pending (client/request {:uri (str (str/replace base #"/$" "") "/api/" endpoint)
                                 :method :post :async true :raw true
                                 :headers {"content-type" "application/json" "accept-encoding" "identity"}
                                 :body (json/generate-string payload)
                                 :timeout upstream-timeout-ms :follow-redirects :never})]
    (try
      (let [response (await-result pending deadline)]
        (with-open [body (.body response)]
          (when-not (= 200 (.statusCode response))
            (throw (ex-info "Search backend returned an HTTP error" {:status (.statusCode response)})))
          (let [reading (future (.readNBytes ^java.io.InputStream body (inc max-response-bytes)))
                bytes (try (await-result reading deadline)
                           (finally
                             (.close body)
                             (future-cancel reading)))]
            (when (> (alength bytes) max-response-bytes)
              (throw (ex-info "Search backend response limit exceeded" {})))
            ;; Keep application keys verbatim, separate from the MCP wire codec.
            (let [result (json/parse-string (utf8-text bytes))]
              (when-not (= "success" (get result "kind"))
                (throw (ex-info (or (get result "error") "Invalid search backend response") {})))
              (let [results (get result "results")
                    field (if (= endpoint "list") "repositories" "files")]
                (when-not (and (map? results) (vector? (get results field)))
                  (throw (ex-info "Invalid search backend result shape" {})))
                (if (= endpoint "search")
                  (update results "files" #(mapv indexed-file-provenance %))
                  results))))))
      (finally
        (.cancel ^CompletableFuture pending true)
        ;; Cancellation can race successful headers before await-result returns.
        (when (and (.isDone ^CompletableFuture pending)
                   (not (.isCompletedExceptionally ^CompletableFuture pending)))
          (.close (.body (.join ^CompletableFuture pending))))))))

;; Git and catalog operations use only discovered repositories and immutable IDs.
(def git-timeout-ms 10000)
(def max-git-bytes (* 8 1024 1024))

(defn- kill-git! [child]
  (let [proc ^Process (:proc child)]
    (with-open [descendants (.descendants (.toHandle proc))]
      (doseq [handle (reverse (vec (iterator-seq (.iterator descendants))))]
        (.destroyForcibly ^java.lang.ProcessHandle handle)))
    (.destroyForcibly proc)
    (.waitFor proc 1000 TimeUnit/MILLISECONDS)))

(defn run-git
  "Run a fixed read-only Git command in `repo`, with bounded output and lifetime.

  `args` is internal operation code, never a client-supplied command vector.
  Return stdout bytes; throw on cancellation, timeout, overflow or Git failure."
  [repo args]
  (when-not (contains? #{"rev-parse" "for-each-ref" "ls-tree" "cat-file" "log" "diff"} (first args))
    (throw (ex-info "Unsupported Git operation" {})))
  (let [deadline (min (or *deadline* Long/MAX_VALUE) (+ (System/nanoTime) (* git-timeout-ms 1000000)))
        command (into ["git" "--no-pager" "--literal-pathspecs" (str "--git-dir=" repo)
                       "-c" "core.bare=true" "-c" "core.fsmonitor=false"
                       "-c" "core.hooksPath=/dev/null" "-c" "log.showSignature=false"
                       "-c" "protocol.allow=never"] args)
        child (process/process command
                               {:env {"PATH" (System/getenv "PATH") "LANG" "C.UTF-8"
                                      "HOME" "/nonexistent" "GIT_CONFIG_NOSYSTEM" "1"
                                      "GIT_CONFIG_GLOBAL" "/dev/null" "GIT_ATTR_NOSYSTEM" "1"
                                      "GIT_NO_REPLACE_OBJECTS" "1" "GIT_NO_LAZY_FETCH" "1"
                                      "GIT_GRAFT_FILE" "/dev/null" "GIT_OPTIONAL_LOCKS" "0"
                                      "GIT_TERMINAL_PROMPT" "0"}
                                :in :inherit :out :stream :err :stream})
        read-bounded (fn [stream cap]
                       (future
                         (let [bytes (.readNBytes ^java.io.InputStream stream (inc cap))]
                           (when (> (alength bytes) cap)
                             (kill-git! child)
                             (throw (ex-info "Git output limit exceeded" {})))
                           bytes)))
        stdout (read-bounded (:out child) max-git-bytes)
        stderr (read-bounded (:err child) 8192)]
    (try
      (let [bytes (await-result stdout deadline)]
        (await-result stderr deadline)
        (when-not (.waitFor ^Process (:proc child) (max 1 (- deadline (System/nanoTime))) TimeUnit/NANOSECONDS)
          (throw (ex-info "Git deadline exceeded" {})))
        (when-not (zero? (.exitValue ^Process (:proc child)))
          (throw (ex-info "Git operation failed; object may be unavailable or stale" {})))
        bytes)
      (finally
        (try (kill-git! child)
             (finally
               (.close ^java.io.InputStream (:out child))
               (.close ^java.io.InputStream (:err child))
               (future-cancel stdout)
               (future-cancel stderr)))))))

(defn git-text
  "Run [[run-git]] and decode its bounded output as UTF-8."
  [repo args]
  (utf8-text (run-git repo args)))

(defn open-catalog!
  "Initialize derived SQLite state for a configured mirror root.

  Return a catalog handle with private per-operation connections and a refresh
  semaphore. `state-dir` must be dedicated to this service."
  [mirror-root state-dir]
  (let [root (fs/real-path mirror-root)
        _ (fs/create-dirs state-dir)
        db (str (fs/path state-dir "catalog.sqlite"))]
    (sqlite/query db "PRAGMA journal_mode=WAL")
    (doseq [sql ["CREATE TABLE IF NOT EXISTS repositories (id TEXT PRIMARY KEY, head TEXT, tree_oid TEXT, refreshed INTEGER, attempted INTEGER, warning TEXT, indexed TEXT, index_warning TEXT)"
                 "CREATE TABLE IF NOT EXISTS refs (repo TEXT, name TEXT, revision TEXT, PRIMARY KEY(repo,name))"
                 "CREATE TABLE IF NOT EXISTS tree (repo TEXT, path TEXT, mode TEXT, kind TEXT, oid TEXT, PRIMARY KEY(repo,path))"
                 "CREATE TABLE IF NOT EXISTS cache (key TEXT PRIMARY KEY, bytes INTEGER, touched INTEGER)"]]
      (sqlite/execute! db sql))
    (when-not (some #(= "tree_oid" (:name %)) (sqlite/query db "PRAGMA table_info(repositories)"))
      (sqlite/execute! db "ALTER TABLE repositories ADD COLUMN tree_oid TEXT"))
    (fs/create-dirs (fs/path state-dir "cache"))
    (let [known (set (map #(str (:key %) ".json") (sqlite/query db "SELECT key FROM cache")))]
      (doseq [path (fs/list-dir (fs/path state-dir "cache"))
              :let [name (str (fs/file-name path))]
              :when (or (str/starts-with? name "pending-")
                        (and (re-matches #"[0-9a-f]{64}\.json" name) (not (contains? known name))))]
        (fs/delete-if-exists path)))
    {:root root :db db :state-dir (fs/real-path state-dir)
     :refresh-slot (Semaphore. 1) :cache-lock (Object.)}))

(defn mirror-path
  "Resolve a discovered repository ID without following symlinks or alternates."
  [catalog id]
  (when-not (and (string? id) (seq (sqlite/query (:db catalog) ["SELECT id FROM repositories WHERE id=?" id])))
    (throw (ex-info "Unknown repository" {})))
  (let [root (:root catalog)
        relative (fs/path (str id ".git"))
        parts (iterator-seq (.iterator relative))
        candidate (reduce (fn [parent part]
                            (let [path (fs/path parent part)]
                              (when (or (= ".." (str part)) (fs/sym-link? path))
                                (throw (ex-info "Unsafe mirror path" {})))
                              path)) root parts)]
    (when-not (and (.startsWith (fs/real-path candidate) root)
                   (not-any? #(fs/sym-link? (fs/path candidate %))
                             ["HEAD" "config" "objects" "refs" "packed-refs"])
                   (not (fs/exists? (fs/path candidate "commondir")))
                   (not (fs/exists? (fs/path candidate "objects/info/alternates"))))
      (throw (ex-info "Unsafe mirror layout" {})))
    ;; Bare Git metadata can redirect object/ref reads too, not just the repo path.
    ;; Inspect on Git access, not on hot catalog/cache reads; never follow links.
    (let [deadline (or *deadline* (+ (System/nanoTime) (* git-timeout-ms 1000000)))
          visited (atom 0)
          inspect (fn [_ attrs]
                    (when (or (.isSymbolicLink ^java.nio.file.attribute.BasicFileAttributes attrs)
                              (> (swap! visited inc) 1000000))
                      (throw (ex-info "Unsafe or excessive mirror layout" {})))
                    (when (or (>= (System/nanoTime) deadline) (and *cancelled* @*cancelled*))
                      (throw (ex-info "Mirror inspection cancelled or deadline exceeded" {})))
                    :continue)]
      (fs/walk-file-tree candidate
                         {:follow-links false :pre-visit-dir inspect :visit-file inspect
                          :visit-file-failed (fn [_ _] (throw (ex-info "Cannot inspect mirror layout" {})))
                          :post-visit-dir (fn [_ error]
                                            (when error (throw (ex-info "Cannot inspect mirror layout" {})))
                                            :continue)}))
    (str candidate)))

(defn discover-mirrors
  "Find bare mirror directories, without traversing symlinks or object stores."
  [root]
  (let [found (atom [])]
    (fs/walk-file-tree root
                       {:follow-links false :max-depth 8
                        :pre-visit-dir (fn [dir _]
                                         (if (str/ends-with? (str (fs/file-name dir)) ".git")
                                           (do
                                             (when (>= (count @found) 10000)
                                               (throw (ex-info "Repository discovery limit exceeded" {})))
                                             (swap! found conj (str/replace (str (fs/relativize root dir)) #"\.git$" ""))
                                             :skip-subtree)
                                           :continue))})
    (sort @found)))

(defn- assert-commit! [repo revision]
  (when-not (= "commit" (str/trim (git-text repo ["cat-file" "-t" revision])))
    (throw (ex-info "Revision is not a commit" {}))))

(defn- parse-tree-entries [text]
  (mapv (fn [record]
          (let [[metadata path] (str/split record #"\t" 2)
                [mode kind oid] (str/split metadata #" ")]
            (when (or (nil? path) (> (count path) 4096))
              (throw (ex-info "Unsupported Git path or length" {})))
            {:path path :mode mode :kind kind :oid oid}))
        (remove str/blank? (str/split text #"\u0000"))))

(defn read-git-tree
  "Read recursive tree entries at an immutable commit; preserve literal paths."
  [repo revision]
  (assert-commit! repo revision)
  (parse-tree-entries (git-text repo ["ls-tree" "-r" "-t" "-z" "--full-tree" revision])))

(defn refresh-catalog!
  "Atomically publish each mirror's refs and default tree; preserve failed snapshots.

  `indexed` maps repository IDs to Neogrok metadata. Nil means index metadata is
  unavailable, so retain its last observation and report a separate warning.
  Return false when another refresh is active, otherwise true."
  [catalog indexed]
  (if-not (.tryAcquire ^Semaphore (:refresh-slot catalog))
    false
    (try
      (let [ids (discover-mirrors (:root catalog))
            db (:db catalog)
            now (System/currentTimeMillis)]
        (doseq [id ids]
          (when (and *cancelled* @*cancelled*) (throw (ex-info "Refresh cancelled" {})))
          (sqlite/execute! db ["INSERT OR IGNORE INTO repositories(id,warning) VALUES (?,?)" id "Not yet refreshed"])
          (try
            (let [repo (mirror-path catalog id)
                  head (str/trim (git-text repo ["rev-parse" "--verify" "HEAD^{commit}"]))
                  refs (mapv #(str/split % #"\u0000" 2)
                             (remove str/blank? (str/split-lines (git-text repo ["for-each-ref" "--format=%(refname)%00%(objectname)" "refs/heads/"]))))
                  previous (first (sqlite/query db ["SELECT head,tree_oid,indexed FROM repositories WHERE id=?" id]))
                  tree-oid (if (and (= head (:head previous)) (:tree_oid previous))
                             (:tree_oid previous)
                             (str/trim (git-text repo ["rev-parse" "--verify" (str head "^{tree}")])))
                  entries (when (not= tree-oid (:tree_oid previous)) (read-git-tree repo head))
                  index-data (if (nil? indexed) (:indexed previous) (some-> (get indexed id) json/generate-string))]
              ;; Git work has finished before the write transaction begins.
              (sqlite/with-conn [conn db]
                (sqlite/with-transaction conn
                  (sqlite/execute! conn ["DELETE FROM refs WHERE repo=?" id])
                  (doseq [[name revision] refs]
                    (sqlite/execute! conn ["INSERT INTO refs VALUES (?,?,?)" id name revision]))
                  (when entries
                    (sqlite/execute! conn ["DELETE FROM tree WHERE repo=?" id])
                    (doseq [{:keys [path mode kind oid]} entries]
                      (when (and *cancelled* @*cancelled*) (throw (ex-info "Refresh cancelled" {})))
                      (sqlite/execute! conn ["INSERT INTO tree VALUES (?,?,?,?,?)" id path mode kind oid])))
                  (sqlite/execute! conn ["UPDATE repositories SET head=?,tree_oid=?,refreshed=?,attempted=?,warning=NULL,indexed=?,index_warning=? WHERE id=?"
                                         head tree-oid now now index-data (when (nil? indexed) "Index metadata unavailable") id]))))
            (catch Exception _
              (sqlite/execute! db ["UPDATE repositories SET attempted=?,warning=? WHERE id=?"
                                   now "Refresh failed; previous snapshot retained" id]))))
        (doseq [{:keys [id]} (sqlite/query db "SELECT id FROM repositories")
                :when (not (contains? (set ids) id))]
          (sqlite/execute! db ["UPDATE repositories SET attempted=?,warning=? WHERE id=?" now "Mirror missing" id]))
        true)
      (finally (.release ^Semaphore (:refresh-slot catalog))))))

;; Cache files hold complete operation results; SQLite stores bounded LRU metadata.
(def max-cache-bytes (* 16 1024 1024))
(def max-cache-entry-bytes (* 1024 1024))
(def max-cache-entries 1024)

(defn- cache-path [catalog key]
  (fs/path (:state-dir catalog) "cache" (str key ".json")))

(defn- discard-cache! [catalog key]
  (fs/delete-if-exists (cache-path catalog key))
  (sqlite/execute! (:db catalog) ["DELETE FROM cache WHERE key=?" key]))

(defn cached-result
  "Cache immutable `compute` results by a versioned, normalized identity vector.

  Bound stored result bytes, entry size and entry count. Evict before writing
  temporary bytes. Failed computations are never inserted; metadata is separate
  from this payload budget. Concurrent misses may compute twice, but writes and
  LRU eviction are serialized."
  [catalog identity compute]
  (let [key (format "%064x" (java.math.BigInteger. 1 (.digest (java.security.MessageDigest/getInstance "SHA-256")
                                                              (.getBytes (pr-str (into [1] identity)) "UTF-8"))))
        db (:db catalog)
        cache-lock (:cache-lock catalog)
        hit (locking cache-lock
              (when-let [entry (first (sqlite/query db ["SELECT bytes FROM cache WHERE key=?" key]))]
                (try
                  (let [path (cache-path catalog key)]
                    (when (or (fs/sym-link? path) (not= (:bytes entry) (fs/size path)))
                      (throw (ex-info "Invalid cache entry" {})))
                    (let [value (json/parse-string (slurp (str path) :encoding "UTF-8") true)]
                      (sqlite/execute! db ["UPDATE cache SET touched=(SELECT COALESCE(MAX(touched),0)+1 FROM cache) WHERE key=?" key])
                      {:value value}))
                  (catch Exception _ (discard-cache! catalog key) nil))))]
    (if hit
      (:value hit)
      (let [value (compute)
            text (json/generate-string value)
            size (alength (.getBytes text "UTF-8"))]
        (when (<= size (min max-cache-entry-bytes max-cache-bytes))
          (locking cache-lock
            (discard-cache! catalog key)
            (loop [entries (sqlite/query db "SELECT key,bytes FROM cache ORDER BY touched,key")]
              (when (or (> (+ size (reduce + 0 (map :bytes entries))) max-cache-bytes)
                        (>= (count entries) max-cache-entries))
                (discard-cache! catalog (:key (first entries)))
                (recur (rest entries))))
            (let [temp (fs/create-temp-file {:dir (fs/path (:state-dir catalog) "cache") :prefix "pending-"})]
              (try
                (spit (str temp) text :encoding "UTF-8")
                (fs/move temp (cache-path catalog key) {:atomic-move true :replace-existing true})
                (sqlite/execute! db ["INSERT INTO cache VALUES (?,?,(SELECT COALESCE(MAX(touched),0)+1 FROM cache))" key size])
                (catch Exception e
                  (fs/delete-if-exists (cache-path catalog key))
                  (throw e))
                (finally (fs/delete-if-exists temp))))))
        value))))

(defn resolve-revision
  "Resolve HEAD or a published branch to an immutable commit, never arbitrary Git syntax."
  [catalog repository revision]
  (when-not (or (nil? revision) (and (string? revision) (<= 1 (count revision) 512)))
    (throw (ex-info "Invalid revision" {})))
  (let [row (first (sqlite/query (:db catalog) ["SELECT * FROM repositories WHERE id=?" repository]))
        revision (or revision "HEAD")
        resolved (cond
                   (= "HEAD" revision) (:head row)
                   (re-matches #"(?:[0-9a-f]{40}|[0-9a-f]{64})" revision) revision
                   :else (:revision (first (sqlite/query (:db catalog)
                                                         ["SELECT revision FROM refs WHERE repo=? AND name=?"
                                                          repository (if (str/starts-with? revision "refs/heads/")
                                                                       revision (str "refs/heads/" revision))]))))]
    (when-not row (throw (ex-info "Unknown repository" {})))
    (when-not resolved (throw (ex-info (str "Unknown revision: " revision " (or incomplete mirror snapshot)") {})))
    (assoc row :revision resolved)))

(defn- safe-path [path]
  (when-not (and (string? path) (<= (count path) 4096)
                 (not (str/starts-with? path "/")) (not (str/includes? path "\u0000"))
                 (not (str/includes? path "//"))
                 (not-any? #{"." ".."} (str/split path #"/")))
    (throw (ex-info "Invalid repository-relative path" {})))
  path)

(defn- tree-at [catalog repository revision]
  ;; A single joined read cannot mix a newly published tree with an old HEAD.
  (let [rows (sqlite/query (:db catalog)
                           ["SELECT tree.path,tree.mode,tree.kind,tree.oid FROM repositories LEFT JOIN tree ON repositories.id=tree.repo WHERE repositories.id=? AND repositories.head=? ORDER BY tree.path"
                            repository revision])]
    (if (seq rows)
      (vec (filter :path rows))
      (cached-result catalog ["tree" repository revision]
                     #(read-git-tree (mirror-path catalog repository) revision)))))

(defn list-catalog-repos
  "List published mirror and index observations, with distinct freshness warnings."
  [catalog query]
  (->> (sqlite/query (:db catalog) "SELECT * FROM repositories ORDER BY id")
       (filter #(or (nil? query) (str/includes? (str/lower-case (:id %)) (str/lower-case query))))
       (mapv #(update % :indexed (fn [value] (when value (json/parse-string value)))))))

(defn list-catalog-branches
  "Return a coherent published branch snapshot and its refresh timestamp."
  ([catalog repository] (list-catalog-branches catalog repository 1000))
  ([catalog repository limit]
   (let [rows (sqlite/query (:db catalog)
                            ["SELECT repositories.refreshed,repositories.warning,refs.name,refs.revision FROM repositories LEFT JOIN refs ON repositories.id=refs.repo WHERE repositories.id=? ORDER BY refs.name LIMIT ?"
                             repository (inc limit)])]
     (when (empty? rows) (throw (ex-info "Unknown repository" {})))
     {:repository repository :refreshed (:refreshed (first rows)) :warning (:warning (first rows))
      :branches (mapv #(select-keys % [:name :revision]) (take limit (filter :name rows)))
      :truncated (> (count rows) limit)})))

(defn list-repository-files
  "List entries below a literal directory at an exact resolved revision.

  | Key | Meaning |
  |-----|---------|
  | `:repository` | Discovered repository ID. |
  | `:revision` | HEAD, published branch or full commit ID; defaults to HEAD. |
  | `:path` | Literal directory path; defaults to the root. |
  | `:depth` | Levels below `:path`; defaults to 1. |
  | `:kind` | \"all\", \"files\" or \"dirs\"; defaults to all. |
  | `:limit` | Maximum entries; defaults to 200. |

  Entries are paths relative to `:path`; directories end in `/`."
  [catalog {:keys [repository revision path limit depth kind] :or {path "" limit 200 depth 1 kind "all"}}]
  (safe-path path)
  (let [{commit :revision :as snapshot} (resolve-revision catalog repository revision)
        directory (str/replace path #"/$" "")
        prefix (if (empty? directory) "" (str directory "/"))
        tree (tree-at catalog repository commit)
        _ (when (and (seq directory)
                     (not-any? #(and (= directory (:path %)) (= "tree" (:kind %))) tree))
            (throw (ex-info "Path is not a directory at this revision" {})))
        entries (->> tree
                     (keep (fn [{entry-path :path entry-kind :kind}]
                             (when (str/starts-with? entry-path prefix)
                               (let [relative (subs entry-path (count prefix))
                                     dir? (= "tree" entry-kind)]
                                 (when (and (seq relative)
                                            (< (count (re-seq #"/" relative)) depth)
                                            (case kind "files" (not dir?) "dirs" dir? true))
                                   (if dir? (str relative "/") relative))))))
                     vec)]
    {:repository repository :revision commit :refreshed (:refreshed snapshot)
     :entries (vec (take limit entries)) :total (count entries) :truncated (> (count entries) limit)}))

(defn read-repository-file
  "Read UTF-8 text from an immutable Git blob; Git symlinks return link text, not targets.

  | Key | Meaning |
  |-----|---------|
  | `:repository` | Discovered repository ID. |
  | `:revision` | HEAD, published branch or full commit ID; defaults to HEAD. |
  | `:path` | Literal file path. |
  | `:start-line` | First one-based line; defaults to 1. |
  | `:end-line` | Last inclusive line; defaults to start plus 199. |"
  [catalog {:keys [repository revision path start-line end-line]
            :or {start-line 1}}]
  (safe-path path)
  (let [commit (:revision (resolve-revision catalog repository revision))
        repo (mirror-path catalog repository)
        _ (assert-commit! repo commit)
        entry (or (first (sqlite/query (:db catalog)
                                       ["SELECT tree.* FROM tree JOIN repositories ON tree.repo=repositories.id WHERE tree.repo=? AND repositories.head=? AND tree.path=?"
                                        repository commit path]))
                  (first (parse-tree-entries (git-text repo ["ls-tree" "-z" "--full-tree" commit "--" path]))))]
    (when-not (= "blob" (:kind entry)) (throw (ex-info "Path is not a file at this revision" {})))
    (let [bytes (run-git repo ["cat-file" "blob" (:oid entry)])
          text (utf8-text bytes)
          end-line (or end-line (+ start-line 199))]
      (when (or (> (alength bytes) max-response-bytes) (str/includes? text "\u0000"))
        (throw (ex-info "File exceeds the text limit or is not supported UTF-8 text" {})))
      (when-not (and (pos-int? start-line) (>= end-line start-line) (<= (- end-line start-line) 499))
        (throw (ex-info "Invalid line range; maximum 500 lines" {})))
      (let [lines (if (empty? text) []
                      (let [parts (str/split text #"\r\n|\n|\r" -1)]
                        (if (re-find #"[\r\n]$" text) (pop parts) parts)))]
        {:repository repository :revision commit :path path :blob (:oid entry) :mode (:mode entry)
         :start-line start-line :end-line (min end-line (count lines))
         :total-lines (count lines) :lines (vec (take (inc (- end-line start-line)) (drop (dec start-line) lines)))}))))

(defn- match-parts? [pattern values star? equal?]
  (boolean (peek
            (reduce
             (fn [previous token]
               (reduce (fn [row [i value]]
                         (conj row (if (star? token)
                                     (or (peek row) (nth previous (inc i)))
                                     (and (nth previous i) (equal? token value)))))
                       [(and (star? token) (first previous))]
                       (map-indexed vector values)))
             (into [true] (repeat (count values) false)) pattern))))

(defn glob-matches?
  "Match a case-sensitive path glob without regex backtracking.

  `*` and `?` match within one path component; `?` matches one Unicode character.
  A whole `**` component matches zero or more components. Other punctuation is
  literal; brackets, brace expansion and backslash escapes have no special role."
  [pattern path]
  (let [parts (str/split pattern #"/")]
    (when (some #(and (str/includes? % "**") (not= "**" %)) parts)
      (throw (ex-info "Globstar must occupy a whole path component" {})))
    (match-parts? parts (str/split path #"/") #{"**"}
                  (fn [part value]
                    (match-parts? (re-seq #"(?s)." part) (re-seq #"(?s)." value) #{"*"}
                                  #(or (= "?" %1) (= %1 %2)))))))

(defn glob-repository
  "Match a bounded set of file paths in a prepared or cached immutable tree.

  | Key | Meaning |
  |-----|---------|
  | `:repository` | Discovered repository ID. |
  | `:revision` | HEAD, published branch or full commit ID; defaults to HEAD. |
  | `:pattern` | Glob using the grammar in [[glob-matches?]]. |
  | `:limit` | Maximum paths; defaults to 100. |"
  [catalog {:keys [repository revision pattern limit] :or {limit 100}}]
  (safe-path pattern)
  (let [commit (:revision (resolve-revision catalog repository revision))
        deadline (or *deadline* (+ (System/nanoTime) (* git-timeout-ms 1000000)))
        matches (->> (tree-at catalog repository commit)
                     (filter (fn [{:keys [path kind]}]
                               (when (or (>= (System/nanoTime) deadline)
                                         (and *cancelled* @*cancelled*))
                                 (throw (ex-info "Glob cancelled or deadline exceeded" {})))
                               (and (= "blob" kind) (glob-matches? pattern path))))
                     (map :path)
                     (take (inc limit)) vec)]
    {:repository repository :revision commit
     :matches (vec (take limit matches)) :truncated (> (count matches) limit)}))

(defn list-repository-commits
  "Return bounded commit metadata, cached by immutable start commit and options.

  | Key | Meaning |
  |-----|---------|
  | `:repository` | Discovered repository ID. |
  | `:revision` | HEAD, published branch or full commit ID; defaults to HEAD. |
  | `:path` | Optional literal file path filter. |
  | `:limit` | Maximum commits; defaults to 20. |"
  [catalog {:keys [repository revision path limit] :or {limit 20}}]
  (when path (safe-path path))
  (let [commit (:revision (resolve-revision catalog repository revision))
        value (cached-result
               catalog ["commits" repository commit path limit]
               (fn []
                 (let [repo (mirror-path catalog repository)
                       _ (assert-commit! repo commit)
                       ids (str/split-lines (git-text repo (cond-> ["log" "--no-color" "--no-notes" "--no-patch"
                                                                    "--format=%H" (str "--max-count=" (inc limit)) commit "--"]
                                                             (seq path) (conj path))))]
                   {:truncated (> (count ids) limit)
                    :commits (mapv (fn [id]
                                     (when-not (re-matches #"(?:[0-9a-f]{40}|[0-9a-f]{64})" id)
                                       (throw (ex-info "Invalid Git history output" {})))
                                     (let [[headers message] (str/split (git-text repo ["cat-file" "commit" id]) #"\n\n" 2)
                                           message (or message "")
                                           parents (vec (take 257 (map second (re-seq #"(?m)^parent ([0-9a-f]+)$" headers))))
                                           author (or (second (re-find #"(?m)^author (.*)$" headers)) "")]
                                       {:revision id
                                        :parents (vec (take 256 parents))
                                        :parents-truncated (> (count parents) 256)
                                        :author (subs author 0 (min 1024 (count author)))
                                        :author-truncated (> (count author) 1024)
                                        :message (subs message 0 (min 4096 (count message)))
                                        :message-truncated (> (count message) 4096)}))
                                   (take limit ids))})))]
    (assoc value :repository repository :revision commit)))

(defn diff-repository
  "Return a bounded, literal-path Git diff with external drivers and textconv disabled.

  | Key | Meaning |
  |-----|---------|
  | `:repository` | Discovered repository ID. |
  | `:from` | Original branch or full commit ID. |
  | `:to` | Target branch or full commit ID. |
  | `:path` | Optional literal path filter. |
  | `:context-lines` | Context lines; defaults to 3. |"
  [catalog {:keys [repository from to path context-lines] :or {context-lines 3}}]
  (when path (safe-path path))
  (let [[before after]
        (sqlite/with-conn [conn (:db catalog)]
          ;; A deferred read transaction pins both names without blocking WAL writers.
          (sqlite/execute! conn "BEGIN")
          (try
            (let [snapshot (assoc catalog :db conn)]
              [(:revision (resolve-revision snapshot repository from))
               (:revision (resolve-revision snapshot repository to))])
            ;; Nothing was written. Release the snapshot before cache or Git work.
            (finally (sqlite/execute! conn "ROLLBACK"))))
        diff (cached-result catalog ["diff" repository before after path context-lines]
                            (fn []
                              (let [repo (mirror-path catalog repository)]
                                (assert-commit! repo before)
                                (assert-commit! repo after)
                                {:patch (git-text repo (cond-> ["diff" "--no-color" "--no-ext-diff" "--no-textconv"
                                                                "--no-renames" (str "--unified=" context-lines) before after "--"]
                                                         (seq path) (conj path)))})))]
    (assoc diff :repository repository :from before :to after)))

(defn search-symbol
  "Search exact Ctags definitions or textual name occurrences, never semantic references.

  | Key | Meaning |
  |-----|---------|
  | `:repository` | Discovered repository ID, matched literally. |
  | `:symbol` | Exact, case-sensitive symbol name. |
  | `:limit` | Maximum matching files; defaults to 20. |"
  [base catalog tool {:keys [repository symbol limit] :or {limit 20}}]
  (when catalog (mirror-path catalog repository))
  (let [quoted (java.util.regex.Pattern/quote symbol)
        expression (if (= tool "find_symbol_definitions") (str "^" quoted "$")
                       (str "(^|[^[:alnum:]_])" quoted "($|[^[:alnum:]_])"))
        query (str "case:yes repo:" (json/generate-string (str "^" (java.util.regex.Pattern/quote repository) "$"))
                   " " (when (= tool "find_symbol_definitions") "sym:") (json/generate-string expression))]
    {:kind (if (= tool "find_symbol_definitions") "ctags-definitions" "textual-occurrences-not-semantic")
     :repository repository :symbol symbol
     :results (neogrok-request base "search" {"query" query "files" limit "matches" 100 "contextLines" 2})}))

;; Result presentation: lean structured data, encoded in the caller's format.
(def default-max-chars 20000)
(def max-line-chars 2000)
(def budget-hint "Output trimmed to maxChars; narrow the request or raise maxChars.")

(defn- cut-line [text]
  (if (> (count text) max-line-chars) (str (subs text 0 max-line-chars) " [truncated]") text))

(defn compact
  "Drop nil, false and empty-collection values from maps, recursively."
  [value]
  (cond
    (map? value) (into {} (keep (fn [[k v]]
                                  (let [v (compact v)]
                                    (when-not (or (nil? v) (false? v) (and (coll? v) (empty? v)))
                                      [k v]))))
                       value)
    (vector? value) (mapv compact value)
    :else value))

(defn- edn-text [value]
  (cond
    (map? value) (str "{" (str/join " " (mapcat (fn [[k v]] [(edn-text k) (edn-text v)]) value)) "}")
    (sequential? value) (str "[" (str/join " " (map edn-text value)) "]")
    :else (pr-str value)))

(defn encode-result
  "Encode a lean result as comma-free EDN (default) or JSON with camelCase keys."
  [value fmt]
  (if (= "json" fmt)
    (json/generate-string value {:key-fn protocol/encode-key})
    (edn-text value)))

(defn- take-fraction [items ratio]
  (subvec (vec items) 0 (max 1 (min (dec (count items)) (int (* (count items) ratio 0.9))))))

(defn- retarget-cite [cite lines]
  (if-let [[a] (first lines)]
    (str/replace cite #":\d+-\d+$" (str ":" a "-" (min (first (peek lines)) (+ a 199))))
    cite))

(defn- read-hint [{:keys [end total-lines]}]
  (when (< end total-lines)
    (str "Lines shown end at " end " of " total-lines "; continue with startLine=" (inc end) ".")))

(defn- shrink
  "Return a smaller result scaled by `ratio`, or nil when nothing can be dropped."
  [value ratio]
  (let [rows (:results value)
        lines (:lines value)]
    (cond
      (> (count rows) 1)
      (-> value (update :results take-fraction ratio) (assoc :has-more true :hint budget-hint))
      (and (= 1 (count rows)) (> (count (:lines (first rows))) 1))
      (let [kept (take-fraction (:lines (first rows)) ratio)]
        (-> value
            (assoc-in [:results 0 :lines] kept)
            (update-in [:results 0] #(cond-> % (:cite %) (update :cite retarget-cite kept)))
            (assoc :has-more true :hint budget-hint)))
      (> (count lines) 1)
      (let [kept (take-fraction lines ratio)
            trimmed (assoc value :lines kept :end (+ (:start value) (count kept) -1))]
        (assoc trimmed :has-more true :hint (read-hint trimmed)))
      :else nil)))

(defn fit-budget
  "Encode `value`, dropping trailing rows or lines until it fits `max-chars`."
  [value fmt max-chars]
  (loop [value (compact value)]
    (let [text (encode-result value fmt)]
      (if (<= (count text) max-chars)
        text
        (if-let [smaller (shrink value (/ max-chars (count text)))]
          (recur (compact smaller))
          text)))))

(defn- file-row
  "Lean row for one Neogrok file result; `repo?` keeps the repository field.

  Lines are in file order. The cite range starts at the first shown line and
  spans at most 200 lines, so read_file returns the matches in context."
  [file context? repo?]
  (let [lines (->> (for [chunk (get file "chunks")
                         [i line] (map-indexed vector (get chunk "lines"))]
                     {:n (+ (get chunk "startLineNumber" 1) i)
                      :text (cut-line (get line "text" ""))
                      :match? (boolean (seq (get line "matchRanges")))})
                   (sort-by :n) (partition-by :n) (map first) vec)
        repo (get file "repository")
        path (get-in file ["fileName" "text"])
        revision (get file "revision")]
    {:repo (when repo? repo)
     :path path
     :lang (not-empty (get file "language"))
     :cite (when revision
             (str repo ":" path "@" revision
                  (when (seq lines)
                    (str ":" (:n (first lines)) "-" (min (:n (peek lines)) (+ (:n (first lines)) 199))))))
     :lines (mapv (juxt :n :text) lines)
     :matched (when context? (mapv :n (filter :match? lines)))}))

(defn present-search
  "Map Neogrok search results to lean rows with totals, truncation and hints."
  [results {:keys [context? repo?]}]
  (let [files (get results "files")
        stats (get results "zoektStats")
        total-files (get stats "fileCount")
        total-matches (get stats "matchCount")
        shown (reduce + 0 (map #(get % "matchCount" 0) files))
        more? (boolean (or (and total-files (< (count files) total-files))
                           (and total-matches (< shown total-matches))))]
    {:total-files total-files
     :total-matches total-matches
     :results (mapv #(file-row % context? repo?) files)
     :has-more more?
     :hint (when more? "More matches exist; raise files/matches or narrow the query with repo:, f: or lang:.")
     :warnings (when (some #(get % "revisionWarning") files)
                 ["Some results lack an immutable revision; they have no cite."])}))

(defn- repo-row [{:keys [id head warning index_warning indexed]}]
  (let [indexed (if (string? indexed) (json/parse-string indexed) indexed)
        branch (some #(when (= "HEAD" (get % "name")) %) (get indexed "branches"))
        last-commit (get indexed "lastCommit")]
    {:id id
     :last-commit (when (string? last-commit) (subs last-commit 0 (min 10 (count last-commit))))
     :files (get indexed "fileCount")
     :warnings (cond-> []
                 warning (conj warning)
                 index_warning (conj index_warning)
                 (nil? indexed) (conj "Not indexed")
                 (and branch head (not= head (get branch "version")))
                 (conj "Index revision differs from mirror HEAD"))}))

(defn present-repos
  "Filter, sort and bound lean repository rows."
  [rows {:keys [limit sort problems] :or {limit 50 sort "name"}}]
  (let [rows (cond->> (map compact (map repo-row rows))
               problems (filter :warnings)
               (= "recent" sort) (sort-by #(or (:last-commit %) "") #(compare %2 %1)))
        rows (vec rows)
        more? (> (count rows) limit)]
    {:total (count rows)
     :results (vec (take limit rows))
     :has-more more?
     :hint (when more? "More repositories match; refine query or raise limit.")}))

(defn parse-cite
  "Parse `repo:path@revision[:start-end]` into read_file arguments."
  [cite]
  (if-let [[_ repo path revision start end]
           (re-matches #"([^:@\s]+):(.+)@([^@:\s]+)(?::(\d{1,7})-(\d{1,7}))?" cite)]
    (cond-> {:repository repo :path path :revision revision}
      start (assoc :start-line (parse-long start)
                   :end-line (min (parse-long end) (+ (parse-long start) 499))))
    (throw (ex-info "Invalid cite; expected repo:path@revision:start-end" {}))))

(defn expand-cite
  "Replace a read_file `:cite` with the arguments it names."
  [args]
  (if-let [cite (:cite args)]
    (do (when (some #(contains? args %) [:repository :path :revision :start-line :end-line])
          (throw (ex-info "Pass either cite or repository/path arguments, not both" {})))
        (merge (dissoc args :cite) (parse-cite cite)))
    args))

;; Tool definitions. :core marks tools served on /mcp; every tool is on /mcp/all.
(def instructions
  (str "Code search over mirrored dependency repositories. Workflow: list_repos(query) to find "
       "repository IDs, then grep (Zoekt syntax) or list_files/glob, then read_file. "
       "Results are EDN by default; pass format=\"json\" for JSON. When a result has has-more, "
       "its hint names the argument to change. Pass a result's cite to read_file to read those lines. "
       "Zoekt queries are regex: escape ( [ . + * ? to match them literally."))

(def tool-specs
  (let [text {:type "string" :minLength 1 :maxLength 512}
        path {:type "string" :maxLength 4096}
        common {"repository" text "revision" text}
        limit {:type "integer" :minimum 1 :maximum 1000}
        context {:type "integer" :minimum 0 :maximum 20}
        read-only {:read-only-hint true :destructive-hint false :idempotent-hint true}
        spec (fn [name description properties required & {:as options}]
               (merge {:name name :description description :annotations read-only
                       :properties (assoc properties "format" {:type "string" :enum ["edn" "json"]})
                       :required required}
                      options))]
    [(spec "grep"
           (str "Search indexed source with Zoekt. Everything is regex; space-separated terms must all "
                "match in a file; `or`, parentheses and leading `-` negation work; quote phrases. Filters: "
                "repo: (r:), file: (f:), content: (c:), lang:, sym:, branch: (b:), "
                "case:yes|no (default: case-sensitive only if the query has uppercase). "
                "Indexed revisions may lag mirror HEAD.")
           {"query" (assoc text :maxLength 4096) "contextLines" context
            "files" (assoc limit :maximum 100) "matches" limit
            "maxChars" {:type "integer" :minimum 1000 :maximum 200000}}
           ["query"] :core true)
     (spec "list_repos"
           "Find repositories by ID substring; query \"\" or \"*\" lists all. Default limit 50."
           {"query" {:type "string" :minLength 0 :maxLength 512} "limit" limit
            "sort" {:type "string" :enum ["name" "recent"]}
            "problems" {:type "boolean"}}
           ["query"] :core true)
     (spec "glob"
           "Case-sensitive file glob: *, ? within components; whole-component ** spans directories. Other punctuation is literal."
           (merge common {"pattern" (assoc text :maxLength 256) "limit" limit}) ["repository" "pattern"])
     (spec "read_file"
           (str "Read UTF-8 text at HEAD, a published branch or full commit ID; or pass cite from a search "
                "result. Default 200 lines, maximum 500. Symlinks return link text.")
           (merge common {"path" (assoc path :minLength 1) "cite" {:type "string" :minLength 3 :maxLength 4800}
                          "startLine" {:type "integer" :minimum 1 :maximum 1000000}
                          "endLine" {:type "integer" :minimum 1 :maximum 1000500}
                          "maxChars" {:type "integer" :minimum 1000 :maximum 200000}})
           [] :core true)
     (spec "list_files"
           "List files and directories under path (default root) at a revision. Directories end in /. Default depth 1."
           (merge common {"path" path "limit" {:type "integer" :minimum 1 :maximum 2000}
                          "depth" {:type "integer" :minimum 1 :maximum 10}
                          "kind" {:type "string" :enum ["all" "files" "dirs"]}})
           ["repository"] :core true)
     (spec "list_branches" "List the published branch snapshot with its refresh time."
           {"repository" text "limit" limit} ["repository"])
     (spec "list_commits" "List bounded history at a resolved revision; optional literal file path. Default 20, maximum 100."
           (merge common {"path" path "limit" (assoc limit :maximum 100)}) ["repository"])
     (spec "get_diff" "Compare two resolved commits with optional literal path and 0-20 context lines."
           {"repository" text "from" text "to" text "path" path "contextLines" context} ["repository" "from" "to"])
     (spec "find_symbol_definitions" "Find exact case-sensitive Ctags definitions in the indexed revision."
           {"repository" text "symbol" (assoc text :maxLength 256) "limit" (assoc limit :maximum 100)
            "maxChars" {:type "integer" :minimum 1000 :maximum 200000}}
           ["repository" "symbol"])
     (spec "find_symbol_references" "Find case-sensitive textual name occurrences with identifier boundaries. NOT semantic references."
           {"repository" text "symbol" (assoc text :maxLength 256) "limit" (assoc limit :maximum 100)
            "maxChars" {:type "integer" :minimum 1000 :maximum 200000}}
           ["repository" "symbol"])]))

(defn- tool-spec [tool] (first (filter #(= tool (:name %)) tool-specs)))

(defn- valid-value? [{:keys [type minimum maximum minLength maxLength enum]} value]
  (case type
    "string" (and (string? value) (<= (or minLength 0) (count value) (or maxLength Integer/MAX_VALUE))
                  (or (nil? enum) (boolean (some #{value} enum))))
    "integer" (and (integer? value) (<= (or minimum Long/MIN_VALUE) value (or maximum Long/MAX_VALUE)))
    "boolean" (boolean? value)
    false))

(defn validate-arguments
  "Validate arguments against the constraints advertised to MCP clients; name the offender."
  [tool args]
  (let [{:keys [properties required] :as spec} (tool-spec tool)
        properties (into {} (map (fn [[k v]] [(protocol/decode-key k) v])) properties)]
    (when-not (and spec (map? args))
      (throw (ex-info "Tool arguments must be an object" {})))
    (when-let [unknown (seq (remove #(contains? properties %) (keys args)))]
      (throw (ex-info (str "Unknown argument: " (str/join ", " (map protocol/encode-key unknown))) {})))
    (when-let [missing (seq (remove #(contains? args (protocol/decode-key %)) required))]
      (throw (ex-info (str "Missing required argument: " (str/join ", " missing)) {})))
    (doseq [[key value] args]
      (when-not (valid-value? (get properties key) value)
        (throw (ex-info (str "Invalid argument: " (protocol/encode-key key)) {}))))
    (when (contains? args :path) (safe-path (:path args)))
    (when (= tool "glob")
      (safe-path (:pattern args))
      (glob-matches? (:pattern args) ""))
    (when (= tool "read_file")
      (let [args (expand-cite args)
            start (get args :start-line 1) end (get args :end-line (+ start 199))]
        (when-not (and (:repository args) (:path args))
          (throw (ex-info "Missing required argument: repository and path, or cite" {})))
        (safe-path (:path args))
        (when-not (<= start end (+ start 499))
          (throw (ex-info "Invalid line range; maximum 500 lines" {})))))
    args))

(defn execute-search
  "Execute a raw Neogrok-backed operation; preserve backend revision evidence."
  [base tool args]
  (validate-arguments tool args)
  (case tool
    "grep" (neogrok-request base "search"
                            {"query" (:query args)
                             "contextLines" (get args :context-lines 0)
                             "files" (get args :files 10)
                             "matches" (get args :matches 50)})
    "list_repos" (neogrok-request base "list"
                                  (if (#{"" "*"} (:query args)) {} (select-keys args [:query])))))

(defn- present-read [{:keys [repository path revision mode start-line end-line total-lines lines]}]
  (let [value {:repo repository :path path :rev revision :symlink (= "120000" mode)
               :start start-line :end end-line :total-lines total-lines :lines (mapv cut-line lines)}]
    (assoc value :has-more (< end-line total-lines) :hint (read-hint value))))

(defn execute-tool
  "Execute a validated tool and return its lean result, independent of MCP transport."
  [base catalog tool args]
  (validate-arguments tool args)
  (case tool
    "grep" (present-search (execute-search base tool args)
                           {:context? (pos? (get args :context-lines 0)) :repo? true})
    "list_repos" (present-repos
                  (if catalog
                    (list-catalog-repos catalog (when-not (#{"" "*"} (:query args)) (:query args)))
                    (map (fn [repo] {:id (get repo "name") :indexed repo})
                         (get (execute-search base tool args) "repositories")))
                  args)
    "glob" (let [{:keys [repository revision matches truncated]} (glob-repository catalog args)]
             {:repo repository :rev revision :results matches :has-more truncated
              :hint (when truncated "More files match; raise limit or narrow the pattern.")})
    "read_file" (present-read (read-repository-file catalog (expand-cite args)))
    "list_files" (let [{:keys [repository revision entries total truncated]} (list-repository-files catalog args)]
                   {:repo repository :rev revision :path (not-empty (:path args)) :entries entries
                    :total total :has-more truncated
                    :hint (when truncated "More entries exist; raise limit or narrow path/depth.")})
    "list_branches" (let [{:keys [repository refreshed warning branches truncated]}
                          (list-catalog-branches catalog (:repository args) (get args :limit 100))]
                      {:repo repository :refreshed refreshed :warnings (when warning [warning])
                       :results (mapv (fn [{:keys [name revision]}] {:name (str/replace name #"^refs/heads/" "") :rev revision}) branches)
                       :has-more truncated})
    "list_commits" (let [{:keys [repository commits truncated]} (list-repository-commits catalog args)]
                     {:repo repository
                      :results (mapv (fn [{:keys [revision author message]}]
                                       {:rev revision :author (str/replace author #"\s+<[^>]*>.*$" "")
                                        :subject (first (str/split-lines message))})
                                     commits)
                      :has-more truncated})
    "get_diff" (let [{:keys [repository from to patch]} (diff-repository catalog args)]
                 {:repo repository :from from :to to :patch patch})
    ("find_symbol_definitions" "find_symbol_references")
    (let [{:keys [kind repository symbol results]} (search-symbol base catalog tool args)]
      (merge {:kind kind :repo repository :symbol symbol}
             (present-search results {:context? true :repo? false})))))

(defn tool-result
  "Return an MCP text result holding `value` in the requested format and budget."
  [tool args value]
  (let [fmt (get args :format "edn")
        budget (when (contains? (:properties (tool-spec tool)) "maxChars")
                 (get args :max-chars default-max-chars))
        text (if budget (fit-budget value fmt budget) (encode-result (compact value) fmt))]
    (when (> (alength (.getBytes ^String text "UTF-8")) max-response-bytes)
      (throw (ex-info "Result exceeds response limit; narrow the request" {})))
    {:content [{:type "text" :text text}]}))

(defn tool-error
  "Return an MCP tool-execution failure containing `message`."
  [message]
  {:is-error true :content [{:type "text" :text (or message "Operation failed")}]})

(defn search-tools
  "Return implemented tools sharing a work semaphore; Git tools require a catalog."
  ([base slots] (search-tools base slots nil))
  ([base slots catalog]
   (mapv
    (fn [{:keys [name description properties required annotations]}]
      {:name name
       :description description
       :input-schema {:type "object" :properties properties :required required
                      :additionalProperties false}
       :annotations annotations
       :tool-fn (fn [context args]
                  (let [args (if (and (nil? args) (not (contains? (get-in context [:message :params]) :arguments))) {} args)]
                    (try
                      (validate-arguments name args)
                      (if-not (.tryAcquire ^Semaphore slots)
                        (tool-error "Server busy; retry later")
                        (p/future
                          (try
                            (binding [*cancelled* (:is-cancelled context)
                                      *deadline* (+ (System/nanoTime) (* git-timeout-ms 1000000))]
                              (tool-result name args (execute-tool base catalog name args)))
                            (catch Exception e (tool-error (ex-message e)))
                            (finally (.release ^Semaphore slots)))))
                      (catch clojure.lang.ExceptionInfo e
                        (rpc/invalid-params-response (get-in context [:message :id]) (ex-message e))))))})
    (if catalog tool-specs (filter #(#{"grep" "list_repos"} (:name %)) tool-specs)))))

(defn response
  "Return a plain-text Ring response with `status` and `body`."
  [status body]
  {:status status :headers {"content-type" "text/plain; charset=utf-8"} :body body})

(defn cancel-session!
  "Signal cancellation for all active requests in the SDK `session` atom."
  [session]
  (doseq [flag (vals (:is-cancelled-by-request-id @session))]
    (reset! flag true)))

(defn expire-sessions!
  "Evict expired sessions before allocating or accessing a session."
  [sessions now]
  (locking sessions
    (doseq [[id {:keys [last-seen session]}] @sessions
            :when (>= (- now last-seen) session-idle-ms)]
      (cancel-session! session)
      (swap! sessions dissoc id))))

(defn handle-supported-message
  "Dispatch `message` through the SDK, exposing only this service's methods.

  `context` uses the SDK context shape, including `:session` and `:send-message`.
  Return a promise that settles after message handling, including cancellation."
  [context message]
  (let [send-message (:send-message context)
        supported #{"initialize" "notifications/initialized" "notifications/cancelled"
                    "ping" "tools/list" "tools/call"}]
    (if (contains? supported (:method message))
      (rpc/handle-message context message)
      (p/resolved (when (contains? message :id)
                    (send-message (rpc/method-not-found-response (:id message))))))))

(defn dispatch-message
  "Return HTTP 202 for notifications or an SSE channel for an MCP request.

  Close the request stream when SDK handling settles, even without a response.
  Disconnects signal cancellation to the SDK request."
  [request session session-id message]
  (if-not (contains? message :id)
    (do
      (handle-supported-message {:session session :send-message (constantly nil)} message)
      (response 202 ""))
    (let [disconnected (atom false)]
      (http/as-channel
       request
       {:on-close (fn [_ _]
                    (reset! disconnected true)
                    (when-let [flag (get (:is-cancelled-by-request-id @session) (:id message))]
                      (reset! flag true)))
        :on-open
        (fn [channel]
          (http/send! channel
                      {:status 200
                       :headers {"content-type" "text/event-stream"
                                 "cache-control" "no-cache"
                                 "mcp-session-id" session-id}}
                      false)
          (let [pending (handle-supported-message
                         {:session session
                          :send-message
                          (fn [outbound]
                            (http/send! channel
                                        (str "event: message\ndata: "
                                             (json/generate-string outbound {:key-fn protocol/encode-key})
                                             "\n\n")
                                        (and (= (:id message) (:id outbound))
                                             (or (contains? outbound :result) (contains? outbound :error)))))}
                         message)]
          ;; A disconnect can precede the SDK's cancellation-flag registration.
            (when @disconnected
              (when-let [flag (get (:is-cancelled-by-request-id @session) (:id message))]
                (reset! flag true)))
            (p/handle pending (fn [_ _] (http/close channel)))))}))))

(defn post-message
  "Validate a JSON-RPC POST, select or create its session, and dispatch it.

  `sessions` is the server's session atom; `tools` contains SDK tool definitions.
  Return a Ring response or an asynchronous channel."
  [request sessions tools]
  (let [message (try
                  (json/parse-string (slurp (:body request)) protocol/decode-key)
                  (catch Exception _ ::invalid))
        session-id (get-in request [:headers "mcp-session-id"])]
    (cond
      (= ::invalid message)
      {:status 400 :headers {"content-type" "application/json"}
       :body (json/generate-string rpc/parse-error-response)}
      (not (and (map? message) (= "2.0" (:jsonrpc message)) (string? (:method message))
                (or (not (contains? message :id))
                    (string? (:id message)) (number? (:id message)))))
      (response 400 "Expected one JSON-RPC request or notification")
      :else
      (let [selection
            (locking sessions
              (expire-sessions! sessions (System/currentTimeMillis))
              (cond
                (= "initialize" (:method message))
                (cond
                  session-id {:error (response 400 "Initialize without a session ID")}
                  (not (contains? message :id)) {:error (response 400 "initialize requires an ID")}
                  (>= (count @sessions) max-sessions) {:error (response 503 "Session capacity reached")}
                  :else
                  (let [id (str (random-uuid))
                        session (atom (assoc (mcp/create-session
                                              {:server-info {:name "code-search" :version "0.2.0"}
                                               :server-instructions instructions
                                               :tools tools :server-capabilities {:tools {}}
                                               :on-initialized nil :on-client-root-list-changed nil})
                                             :server-supported-protocol-versions protocol-versions))]
                    (swap! sessions assoc id {:session session :last-seen (System/currentTimeMillis)})
                    {:id id :session session}))
                (nil? session-id) {:error (response 400 "Missing Mcp-Session-Id")}
                (nil? (get @sessions session-id)) {:error (response 404 "Unknown or expired session")}
                :else
                (let [session (get-in @sessions [session-id :session])]
                  (if-not (= (:protocol-version @session)
                             (get-in request [:headers "mcp-protocol-version"]))
                    {:error (response 400 "MCP-Protocol-Version must match the negotiated version")}
                    (do
                      (swap! sessions assoc-in [session-id :last-seen] (System/currentTimeMillis))
                      {:id session-id :session session})))))]
        (if-let [error (:error selection)]
          error
          (dispatch-message request (:session selection) (:id selection) message))))))

(defn media-types
  "Return the lowercase media types in `header`, without parameters."
  [header]
  (set (map #(-> % (str/split #";") first str/trim str/lower-case)
            (str/split (or header "") #","))))

(defn handler
  "Return a Ring handler for `/mcp` with exact Host and Origin validation.

  `origin` is the allowed public origin. `sessions` and `tools` are passed to
  [[post-message]]. Only POST and DELETE are supported."
  [origin sessions tools]
  (let [host (.getRawAuthority (URI. origin))]
    (fn [{:keys [uri request-method headers] :as request}]
      (cond
        (not= host (get headers "host")) (response 403 "Forbidden Host")
        (and (get headers "origin") (not= origin (get headers "origin")))
        (response 403 "Forbidden Origin")
        (not= "/mcp" uri) (response 404 "Not found")
        (= :post request-method)
        (cond
          (not (contains? (media-types (get headers "content-type")) "application/json"))
          (response 415 "Expected application/json")
          (not (every? (media-types (get headers "accept")) ["application/json" "text/event-stream"]))
          (response 406 "Accept must include application/json and text/event-stream")
          :else (post-message request sessions tools))
        (= :delete request-method)
        (locking sessions
          (expire-sessions! sessions (System/currentTimeMillis))
          (if-let [session (get-in @sessions [(get headers "mcp-session-id") :session])]
            (if-not (= (:protocol-version @session) (get headers "mcp-protocol-version"))
              (response 400 "MCP-Protocol-Version must match the negotiated version")
              (do
                (cancel-session! session)
                (swap! sessions dissoc (get headers "mcp-session-id"))
                (response 204 "")))
            (response 404 "Unknown or expired session")))
        :else (assoc-in (response 405 "Use POST or DELETE") [:headers "allow"] "POST, DELETE")))))

(defn start!
  "Start a loopback MCP server. Return its stop function.

  Options:
  | Key | Meaning |
  |-----|---------|
  | `:port` | Loopback listening port. |
  | `:origin` | Exact allowed public origin, without a trailing slash. |
  | `:neogrok-url` | Trusted Neogrok base URL, including its path prefix. |
  | `:mirror-root` | Optional bare mirror root; enables catalog and Git tools. |
  | `:state-dir` | Dedicated writable state directory; required with mirror root. |"
  [{:keys [port origin neogrok-url mirror-root state-dir]}]
  (let [uri (URI. origin)
        _ (when-not (and (integer? port) (<= 1 port 65535)
                         (#{"http" "https"} (.getScheme uri)) (.getHost uri)
                         (nil? (.getUserInfo uri)) (nil? (.getQuery uri)) (nil? (.getFragment uri))
                         (empty? (.getPath uri)))
            (throw (ex-info "Invalid listening port or public origin" {})))
        _ (when (and mirror-root (not state-dir))
            (throw (ex-info "state-dir is required with mirror-root" {})))
        cancelled (atom false)
        wake (java.util.concurrent.CountDownLatch. 1)
        catalog (when mirror-root (open-catalog! mirror-root state-dir))
        refresh (fn []
                  (binding [*cancelled* cancelled]
                    (refresh-catalog! catalog
                                      (try
                                        (into {} (map (juxt #(get % "name") identity))
                                              (get (neogrok-request neogrok-url "list" {}) "repositories"))
                                        (catch Exception _ nil)))))
        _ (when catalog (refresh))
        sessions (atom {})
        tools (search-tools neogrok-url (Semaphore. 4) catalog)
        stop (http/run-server (handler origin sessions tools)
                              {:ip "127.0.0.1" :port port :max-body max-request-bytes})
        worker (when catalog
                 (future
                   (while (not (.await wake 300000 TimeUnit/MILLISECONDS))
                     (try (refresh)
                          (catch Exception _
                            (binding [*out* *err*] (println "Catalog refresh failed; retaining previous snapshots")))))))]
    (fn []
      (reset! cancelled true)
      (.countDown wake)
      (when worker (future-cancel worker))
      (doseq [{:keys [session]} (vals @sessions)] (cancel-session! session))
      (reset! sessions {})
      (stop :timeout 1000))))

(defn -main
  "Start the server using environment configuration and wait until shutdown."
  [& _]
  (let [port (parse-long (or (System/getenv "PORT") "3082"))
        origin (or (System/getenv "CODE_SEARCH_ORIGIN") (str "http://127.0.0.1:" port))
        backend (or (System/getenv "NEOGROK_URL") "http://127.0.0.1:3081/search")
        stop (start! {:port port :origin origin :neogrok-url backend
                      :mirror-root (System/getenv "MIRROR_ROOT")
                      :state-dir (System/getenv "STATE_DIRECTORY")})]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop))
    (binding [*out* *err*] (println "Code-search MCP listening on loopback port" port))
    @(promise)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
