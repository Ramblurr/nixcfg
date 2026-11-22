(ns ts-broker.store
  "SQLite store with one writer thread and a pool of reader connections.

  Writes are command maps that [[submit!]] queues. The writer drains the
  queue, runs each batch in one transaction with a savepoint per command,
  then prints the committed audit rows as JSON lines and calls the
  listeners with the batch's events. Every command that changes state
  appends its audit rows in its own savepoint.

  Reads take a connection from the pool with [[read]] and pass it to the
  query functions in this namespace."
  (:refer-clojure :exclude [read])
  (:require
   [babashka.sqlite :as sq]
   [cheshire.core :as json]
   [clojure.string :as str]
   [ts-broker.domain :as domain])
  (:import
   [java.time Instant]
   [java.util ArrayList]
   [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def schema-version
  "The `user_version` of a database with [[schema]]."
  2)

(def schema
  ["create table credential (
      name text primary key,
      vault text not null,
      item text not null,
      field text not null,
      field_label text not null,
      header text not null,
      hosts text not null,
      revision text not null,
      created text not null,
      updated text not null,
      editor text not null)"
   "create table assignment (
      subject text not null,
      credential text not null,
      mode text not null,
      approval text not null,
      editor text not null,
      created text not null,
      primary key (subject, credential, mode))"
   "create table approval (
      id integer primary key,
      identity text not null,
      credential text not null,
      mode text not null,
      revision text not null,
      host text not null,
      state text not null,
      requested_at text not null,
      decided_at text,
      approver text,
      expires_at text,
      uses_left integer)"
   "create unique index approval_open
      on approval (identity, credential, mode, host)
      where state in ('pending', 'approved', 'blocked')"
   "create table audit (
      id integer primary key,
      at text not null,
      kind text not null,
      identity text,
      actor text,
      credential text,
      detail text not null)"
   "create index audit_identity on audit (kind, identity)"])

(def ^:private instant-columns #{:at :created :updated :requested-at :decided-at :expires-at})

(defn- column-key [k]
  (keyword (str/replace (name k) "_" "-")))

(defn- column-value [k v]
  (cond
    (nil? v) nil
    (instant-columns k) (Instant/parse v)
    (= k :state) (keyword v)
    (#{:hosts :detail} k) (json/parse-string v true)
    :else v))

(defn- row [m]
  (into {} (map (fn [[k v]] (let [k (column-key k)] [k (column-value k v)]))) m))

(defn- rows [db q]
  (mapv row (sq/query db q)))

(defn- text [x]
  (cond
    (instance? Instant x) (str x)
    (keyword? x) (name x)
    :else x))

(defn credentials
  "Returns all credentials ordered by name."
  [db]
  (rows db "select * from credential order by name"))

(defn credential
  "Returns the credential named `name`, or nil."
  [db name]
  (first (rows db ["select * from credential where name = ?" name])))

(defn assignments
  "Returns all assignments ordered by credential and subject."
  [db]
  (rows db "select * from assignment order by credential, subject, mode"))

(defn credential-assignments
  "Returns the assignments of credential `name`."
  [db name]
  (rows db ["select * from assignment where credential = ? order by subject, mode" name]))

(defn approval
  "Returns the approval with `id`, or nil."
  [db id]
  (first (rows db ["select * from approval where id = ?" id])))

(defn open-approval
  "Returns the open approval of `identity`, `credential` and `mode` for
  `host`, or nil. A blocked approval covers every host."
  [db identity credential mode host]
  (first (rows db ["select * from approval
                    where identity = ? and credential = ? and mode = ?
                      and (state = 'blocked' or (host = ? and state in ('pending', 'approved')))
                    order by state = 'blocked' desc
                    limit 1"
                   identity credential mode host])))

(defn open-approvals
  "Returns the open approvals, newest first."
  [db]
  (rows db "select * from approval
            where state in ('pending', 'approved', 'blocked')
            order by id desc"))

(defn audit
  "Returns up to `limit` audit rows, newest first. `filters` may hold
  `:identity`, `:credential` and `:kind` to match exactly."
  [db filters limit]
  (let [filters (select-keys filters [:identity :credential :kind])
        where (str/join " and " (map #(str (name %) " = ?") (keys filters)))]
    (rows db (into [(str "select * from audit"
                         (when (seq filters) (str " where " where))
                         " order by id desc limit ?")]
                   (concat (vals filters) [limit])))))

(defn identities
  "Returns each identity that sent a decide request, with the node, groups
  and modes of its latest request, ordered by identity."
  [db]
  (->> (rows db "select identity, max(id) as id, at, detail from audit
                 where kind = 'request' and identity is not null
                 group by identity order by identity")
       (mapv (fn [{:keys [identity at detail]}]
               (merge {:identity identity :seen at}
                      (select-keys detail [:node :groups :modes]))))))

(defn- audit! [tx {:keys [detail] :as entry}]
  (let [entry (assoc entry :at ((:clock tx)))
        {:keys [last-insert-rowid]}
        (sq/execute! (:conn tx)
                     ["insert into audit (at, kind, identity, actor, credential, detail)
                       values (?, ?, ?, ?, ?, ?)"
                      (str (:at entry)) (:kind entry) (:identity entry) (:actor entry)
                      (:credential entry) (json/generate-string (or detail {}))])]
    (vswap! (:audits tx) conj (assoc entry :id last-insert-rowid :detail (or detail {})))))

(defn- event! [tx event]
  (vswap! (:events tx) conj event))

(defn- invalid! [errors]
  (throw (ex-info (str/join " " errors) {:type ::invalid :errors errors})))

(defn- approval-detail [a]
  (-> (select-keys a [:id :state :host :revision :expires-at :uses-left])
      (update-vals text)))

(defn- save-approval! [tx a actor]
  (sq/execute! (:conn tx)
               ["update approval set state = ?, decided_at = ?, approver = ?,
                   expires_at = ?, uses_left = ? where id = ?"
                (text (:state a)) (text (:decided-at a)) (:approver a)
                (text (:expires-at a)) (:uses-left a) (:id a)])
  (audit! tx {:kind       "approval"
              :identity   (:identity a)
              :credential (:credential a)
              :actor      actor
              :detail     (approval-detail a)}))

(defn- create-approval! [tx a]
  (let [now ((:clock tx))
        {:keys [last-insert-rowid]}
        (sq/execute! (:conn tx)
                     ["insert into approval (identity, credential, mode, revision, host, state, requested_at)
                       values (?, ?, ?, ?, ?, 'pending', ?)"
                      (:identity a) (:credential a) (:mode a) (:revision a) (:host a) (str now)])
        a (assoc a :id last-insert-rowid :state :pending :requested-at now)]
    (audit! tx {:kind       "approval"
                :identity   (:identity a)
                :credential (:credential a)
                :detail     (approval-detail a)})
    (event! tx {:event :approval-needed :approval a})
    last-insert-rowid))

(defn- run-writes! [tx writes]
  (reduce (fn [created {:keys [op approval]}]
            (case op
              :approval/create (create-approval! tx approval)
              :approval/save (do (save-approval! tx approval nil) created)))
          nil
          writes))

(defmulti command
  "Runs write command `cmd` in transaction context `tx` and returns its
  result. Dispatches on `:command`."
  (fn [_tx cmd] (:command cmd)))

(defn- request-detail [request]
  (-> (select-keys request [:node :groups :modes :host :header :mode])
      (update :groups #(vec (sort %)))
      (update :modes #(vec (sort %)))))

(defmethod command :decide [tx {:keys [request]}]
  (let [db (:conn tx)
        {:keys [identity mode] cred-name :credential} request
        _ (audit! tx {:kind "request" :identity identity :credential cred-name
                      :detail (request-detail request)})
        {:keys [decision writes]} (domain/decide request
                                                 (credential db cred-name)
                                                 (credential-assignments db cred-name)
                                                 (open-approval db identity cred-name mode (:host request))
                                                 ((:clock tx)))
        created (run-writes! tx writes)
        decision (cond-> decision
                   (and (= "pending" (:decision decision)) (nil? (:approval decision)))
                   (assoc :approval (str created)))]
    (audit! tx {:kind "decision" :identity identity :credential cred-name
                :detail (select-keys decision [:decision :reason :approval])})
    (when (= "allow" (:decision decision))
      (audit! tx {:kind "use" :identity identity :credential cred-name
                  :detail {:approval (:approval decision) :host (:host request)}}))
    decision))

(defn- host-patterns [hosts]
  (vec (distinct (map domain/host-pattern hosts))))

(defn- credential-audit! [tx actor c action]
  (audit! tx {:kind       "credential"
              :actor      actor
              :credential (:name c)
              :detail     (assoc (select-keys c [:vault :item :field :field-label :header :hosts])
                                 :action action)}))

(defmethod command :credential/create [tx {c :credential :keys [actor]}]
  (let [db (:conn tx)
        errors (domain/credential-errors c)
        now (str ((:clock tx)))
        c (update c :hosts host-patterns)]
    (cond
      (seq errors) (invalid! errors)
      (credential db (:name c)) (invalid! [(str "A credential named " (:name c) " exists.")]))
    (sq/execute! db ["insert into credential
                        (name, vault, item, field, field_label, header, hosts, revision, created, updated, editor)
                      values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                     (:name c) (:vault c) (:item c) (:field c) (:field-label c) (:header c)
                     (json/generate-string (:hosts c)) (str (random-uuid)) now now actor])
    (credential-audit! tx actor c "create")
    (:name c)))

(defn- close-approvals!
  "Moves each open approval of credential `name` by the event that
  `events` maps its state to. Leaves approvals in other states open."
  [tx name events actor]
  (doseq [a (rows (:conn tx) ["select * from approval where credential = ?
                               and state in ('pending', 'approved', 'blocked')" name])
          :let [event (events (:state a))]
          :when event]
    (save-approval! tx (domain/transition a event {:actor actor :now ((:clock tx))}) actor)))

(defmethod command :credential/update [tx {:keys [name revision header hosts actor]}]
  (let [db (:conn tx)
        existing (credential db name)
        c (assoc existing :header header :hosts hosts)]
    (when (or (nil? existing) (not= revision (:revision existing)))
      (invalid! ["This credential changed or was deleted since the form was shown. Reload the page."]))
    (when-let [errors (seq (domain/credential-errors c))]
      (invalid! errors))
    (let [c (update c :hosts host-patterns)]
      (when-not (= (select-keys c [:header :hosts]) (select-keys existing [:header :hosts]))
        (sq/execute! db ["update credential set header = ?, hosts = ?, revision = ?, updated = ?, editor = ?
                          where name = ?"
                         header (json/generate-string (:hosts c)) (str (random-uuid))
                         (str ((:clock tx))) actor name])
        (credential-audit! tx actor c "update")
        (close-approvals! tx name {:pending :supersede :approved :supersede} actor))
      name)))

(defmethod command :credential/delete [tx {:keys [name revision actor]}]
  (let [db (:conn tx)]
    (when-not (some-> (credential db name) :revision (= revision))
      (invalid! ["This credential changed or was deleted since the form was shown. Reload the page."]))
    (close-approvals! tx name {:pending :supersede :approved :supersede :blocked :unblock} actor)
    (doseq [a (credential-assignments db name)]
      (audit! tx {:kind "assignment" :actor actor :identity (:subject a) :credential name
                  :detail {:action "remove" :subject (:subject a) :mode (:mode a)}}))
    (sq/execute! db ["delete from assignment where credential = ?" name])
    (sq/execute! db ["delete from credential where name = ?" name])
    (audit! tx {:kind "credential" :actor actor :credential name :detail {:action "delete"}})
    name))

(defmethod command :assignment/assign [tx {:keys [credentials subjects mode approval actor]}]
  (let [db (:conn tx)
        missing (remove #(credential db %) credentials)
        errors (cond-> (vec (domain/subject-errors subjects))
                 (empty? credentials) (conj "Select at least one credential.")
                 (empty? subjects) (conj "Select at least one subject.")
                 (seq missing) (conj (str "Unknown credentials: " (str/join ", " missing)))
                 (not (domain/modes mode)) (conj "Unknown mode.")
                 (not (domain/approval-policies approval)) (conj "Approval must be auto or ask."))
        now (str ((:clock tx)))]
    (when (seq errors)
      (invalid! errors))
    (doseq [c (distinct credentials)
            s (distinct subjects)]
      (sq/execute! db ["insert into assignment (subject, credential, mode, approval, editor, created)
                        values (?, ?, ?, ?, ?, ?)
                        on conflict (subject, credential, mode) do update set
                          approval = excluded.approval, editor = excluded.editor"
                       s c mode approval actor now])
      (audit! tx {:kind "assignment" :actor actor :identity s :credential c
                  :detail {:action "assign" :subject s :mode mode :approval approval}}))
    (* (count (distinct credentials)) (count (distinct subjects)))))

(defmethod command :assignment/remove [tx {:keys [subject credential mode actor]}]
  (let [{:keys [rows-changed]}
        (sq/execute! (:conn tx) ["delete from assignment where subject = ? and credential = ? and mode = ?"
                                 subject credential mode])]
    (when (zero? rows-changed)
      (invalid! ["No such assignment."]))
    (audit! tx {:kind "assignment" :actor actor :identity subject :credential credential
                :detail {:action "remove" :subject subject :mode mode}})
    rows-changed))

(defmethod command :approval/event [tx {:keys [id event grant actor]}]
  (let [a (approval (:conn tx) id)
        next-a (some-> a (domain/transition event {:grant grant :actor actor :now ((:clock tx))}))]
    (when-not next-a
      (invalid! [(cond
                   (= :superseded (:state a))
                   (str "Approval " id " was superseded by a change to its credential. The next request asks again.")

                   event
                   (str "Approval " id " cannot take " (name event) ".")

                   :else
                   "No such approval or event.")]))
    (save-approval! tx next-a actor)
    (when (= :unblock event)
      (doseq [b (rows (:conn tx) ["select * from approval
                                   where identity = ? and credential = ? and mode = ? and state = 'blocked'"
                                  (:identity a) (:credential a) (:mode a)])]
        (save-approval! tx (domain/transition b :unblock {:actor actor :now ((:clock tx))}) actor)))
    (assoc (approval-detail next-a) :event (name event) :grant grant)))

(defn- run-command [conn clock {:keys [cmd]}]
  (let [tx {:conn conn :clock clock :audits (volatile! []) :events (volatile! [])}]
    (sq/execute! conn "savepoint command")
    (try
      (let [result (command tx cmd)]
        (sq/execute! conn "release command")
        {:result result :audits @(:audits tx) :events @(:events tx)})
      (catch Throwable e
        (sq/execute! conn "rollback to command")
        (sq/execute! conn "release command")
        {:error e}))))

(defn- run-batch! [{:keys [writer clock audit-out listeners]} batch]
  (let [outcomes (try
                   (sq/with-transaction writer
                     (mapv #(run-command writer clock %) batch))
                   (catch Throwable e
                     (mapv (constantly {:error e}) batch)))]
    (doseq [[{:keys [promise]} {:keys [result error]}] (map vector batch outcomes)]
      (deliver promise (if error {:error error} {:result result})))
    (doseq [entry (mapcat :audits outcomes)]
      (audit-out entry))
    (when (some :result outcomes)
      (let [events (into [{:event :changed}] (mapcat :events) outcomes)]
        (doseq [f (vals @listeners)]
          (f events))))))

(defn- writer-loop [{:keys [^LinkedBlockingQueue queue] :as store}]
  (loop []
    (let [first-item (.take queue)]
      (when-not (= ::stop first-item)
        (let [items (ArrayList.)]
          (.drainTo queue items)
          (let [batch (into [first-item] (remove #(= ::stop %)) items)]
            (run-batch! store batch)
            (when-not (some #(= ::stop %) items)
              (recur))))))))

(defn- create-schema! [conn]
  (sq/with-transaction conn
    (doseq [stmt schema]
      (sq/execute! conn stmt))
    (sq/execute! conn (str "pragma user_version = " schema-version))))

(defn- open-writer
  "Opens the writer connection to `path`. Creates the schema in an empty
  database. Throws when the database has another schema version."
  [path]
  (let [conn (sq/open path)
        [{version :user_version}] (sq/query conn "pragma user_version")
        [{tables :n}] (sq/query conn "select count(*) as n from sqlite_master")]
    (cond
      (and (zero? version) (zero? tables))
      (create-schema! conn)

      (not= schema-version version)
      (do (sq/close! conn)
          (throw (ex-info (str "The database " path " has schema version " version
                               " and ts-broker needs " schema-version
                               ". ts-broker does not migrate databases.")
                          {:type ::schema :path path :version version}))))
    (sq/query conn "pragma journal_mode = wal")
    (sq/execute! conn "pragma synchronous = full")
    conn))

(defn print-audit
  "Prints audit `entry` as one JSON line on stdout."
  [entry]
  (println (json/generate-string (update-vals entry text))))

(defn start!
  "Opens the database at `path` and starts the writer. Creates the schema
  in a new database and throws when an existing one has another
  [[schema-version]].

  Options:

  | key          | description |
  |--------------|-------------|
  | `:clock`     | Function that returns the current `Instant` (default `Instant/now`) |
  | `:readers`   | Number of reader connections (default 4) |
  | `:audit-out` | Function called with each committed audit row (default [[print-audit]]) |"
  [path {:keys [clock readers audit-out]
         :or   {clock #(Instant/now) readers 4 audit-out print-audit}}]
  (let [writer (open-writer path)
        pool (LinkedBlockingQueue.)
        _ (dotimes [_ readers] (.put pool (sq/open path {:read-only true})))
        store {:path      path
               :writer    writer
               :pool      pool
               :readers   readers
               :queue     (LinkedBlockingQueue.)
               :clock     clock
               :audit-out audit-out
               :listeners (atom {})}
        thread (doto (Thread. #(writer-loop store) "ts-broker-writer")
                 (.setDaemon true)
                 (.start))]
    (assoc store :thread thread)))

(defn stop!
  "Stops the writer after the queued commands and closes all connections."
  [{:keys [^LinkedBlockingQueue queue ^Thread thread ^LinkedBlockingQueue pool readers writer]}]
  (.put queue ::stop)
  (.join thread 10000)
  (dotimes [_ readers]
    (sq/close! (.poll pool 10 TimeUnit/SECONDS)))
  (sq/close! writer)
  nil)

(defn submit!
  "Queues write command `cmd` and returns a promise of `{:result r}` or
  `{:error e}`."
  [store cmd]
  (let [p (promise)]
    (.put ^LinkedBlockingQueue (:queue store) {:cmd cmd :promise p})
    p))

(defn write!
  "Runs write command `cmd` and returns its result. Throws the command's
  exception, or when the writer does not answer within 10 seconds."
  [store cmd]
  (let [{:keys [result error] :as outcome} (deref (submit! store cmd) 10000 ::timeout)]
    (cond
      (= ::timeout outcome) (throw (ex-info "Store writer timed out." {:type ::timeout}))
      error (throw error)
      :else result)))

(defn invalid-errors
  "Returns the validation messages of exception `e` from a command, or nil."
  [e]
  (when (= ::invalid (:type (ex-data e)))
    (:errors (ex-data e))))

(defn read
  "Calls `f` with a reader connection and returns its result."
  [{:keys [^LinkedBlockingQueue pool]} f]
  (let [conn (.take pool)]
    (try
      (f conn)
      (finally
        (.put pool conn)))))

(defn listen!
  "Calls `f` with the events of each batch that committed a command, under
  `k`. Events are maps with `:event` of `:changed` or `:approval-needed`.
  `f` runs on the writer thread and must return quickly."
  [store k f]
  (swap! (:listeners store) assoc k f))

(defn unlisten!
  "Removes the listener under `k`."
  [store k]
  (swap! (:listeners store) dissoc k))
