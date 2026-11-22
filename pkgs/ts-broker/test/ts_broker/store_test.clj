(ns ts-broker.store-test
  (:require
   [babashka.fs :as fs]
   [babashka.sqlite :as sq]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [ts-broker.domain :as domain]
   [ts-broker.socket :as socket]
   [ts-broker.store :as store])
  (:import
   [java.io BufferedReader InputStreamReader]
   [java.net StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio.channels Channels SocketChannel]
   [java.time Duration Instant]))

(def ^:dynamic *store* nil)
(def ^:dynamic *dir* nil)
(def ^:dynamic *clock* nil)

(use-fixtures :each
  (fn [f]
    (let [dir (fs/create-temp-dir {:prefix "ts-broker-test"})
          clock (atom (Instant/parse "2026-11-22T10:00:00Z"))
          st (store/start! (str (fs/path dir "broker.db")) {:clock #(deref clock) :audit-out (fn [_])})]
      (try
        (binding [*store* st *dir* dir *clock* clock]
          (f))
        (finally
          (store/stop! st)
          (fs/delete-tree dir))))))

(defn- write! [cmd]
  (store/write! *store* cmd))

(defn- seed! [approval]
  (write! {:command :credential/create :actor "casey"
           :credential {:name "GITHUB_TOKEN" :vault "vlt1" :item "itm1" :field "fld1"
                        :vault-title "Agents" :item-title "GitHub token" :field-label "credential" :header "Authorization"
                        :hosts ["API.github.com"]}})
  (write! {:command :assignment/assign :credentials ["GITHUB_TOKEN"] :subjects ["group:agents"]
           :mode "inject" :approval approval :actor "casey"}))

(def request
  {:op "decide" :identity "tag:t3-alice" :node "t3-alice" :groups ["agents"] :modes ["inject"]
   :credential "GITHUB_TOKEN" :host "api.github.com" :header "Authorization" :mode "inject"})

(defn- decide!
  ([]
   (decide! {}))
  ([override]
   (write! {:command :decide :request (domain/parse-request (merge request override))})))

(defn- audit-kinds []
  (mapv (juxt :kind :identity :actor :credential)
        (reverse (store/read *store* #(store/audit % {} 100)))))

(def allow
  {:decision "allow"
   :approval nil
   :secret   {:vault "vlt1" :item "itm1" :field "fld1"}
   :inject   {:header "Authorization"}})

(deftest auto-assignment-allows-and-audits
  (seed! "auto")
  (is (= allow (decide!)))
  (is (= [["credential" nil "casey" "GITHUB_TOKEN"]
          ["assignment" "group:agents" "casey" "GITHUB_TOKEN"]
          ["request" "tag:t3-alice" nil "GITHUB_TOKEN"]
          ["decision" "tag:t3-alice" nil "GITHUB_TOKEN"]
          ["use" "tag:t3-alice" nil "GITHUB_TOKEN"]]
         (audit-kinds))))

(deftest ask-approval-lifecycle
  (seed! "ask")
  (let [events (atom [])]
    (store/listen! *store* ::t #(swap! events into (remove (comp #{:changed} :event)) %))
    (is (= {:decision "pending" :approval "1"} (decide!)))
    (is (= {:decision "pending" :approval "1"} (decide!)))
    (is (= [:approval-needed] (map :event @events)) "one event per new pending approval")
    (write! {:command :approval/event :id 1 :event :approve :grant "1h" :actor "casey"})
    (is (= (assoc allow :approval "1") (decide!)))
    (testing "revoke applies on the next request"
      (write! {:command :approval/event :id 1 :event :revoke :actor "casey"})
      (is (= {:decision "pending" :approval "2"} (decide!))))
    (testing "deny closes the approval and the next request asks again"
      (write! {:command :approval/event :id 2 :event :deny :actor "casey"})
      (is (= {:decision "pending" :approval "3"} (decide!))))
    (testing "block denies until unblocked"
      (write! {:command :approval/event :id 3 :event :block :actor "casey"})
      (is (= {:decision "deny" :reason "blocked"} (decide!)))
      (write! {:command :approval/event :id 3 :event :unblock :actor "casey"})
      (is (= {:decision "pending" :approval "4"} (decide!))))
    (testing "a timed approval expires"
      (write! {:command :approval/event :id 4 :event :approve :grant "1h" :actor "casey"})
      (is (= "allow" (:decision (decide!))))
      (swap! *clock* #(.plus ^Instant % (Duration/ofMinutes 61)))
      (is (= {:decision "pending" :approval "5"} (decide!))))
    (is (= [[1 :revoked "casey"] [2 :denied "casey"] [3 :closed "casey"] [4 :expired "casey"] [5 :pending nil]]
           (store/read *store* (fn [db] (mapv #((juxt :id :state :approver) (store/approval db %)) (range 1 6))))))))

(deftest a-once-approval-survives-a-wrong-header
  (seed! "ask")
  (decide!)
  (write! {:command :approval/event :id 1 :event :approve :grant "once" :actor "casey"})
  (is (= [{:decision "deny" :reason "header"} (assoc allow :approval "1") {:decision "pending" :approval "2"}]
         [(decide! {:header "X-Other"}) (decide!) (decide!)])))

(deftest invalid-commands-roll-back-alone
  (seed! "ask")
  (let [before (audit-kinds)
        bad (store/submit! *store* {:command :approval/event :id 99 :event :approve :grant "once" :actor "casey"})
        good (store/submit! *store* {:command :decide :request (domain/parse-request request)})]
    (is (= ["Approval 99 cannot take approve."] (store/invalid-errors (:error @bad))))
    (is (= {:decision "pending" :approval "1"} (:result @good)))
    (is (= (into before [["request" "tag:t3-alice" nil "GITHUB_TOKEN"]
                         ["approval" "tag:t3-alice" nil "GITHUB_TOKEN"]
                         ["decision" "tag:t3-alice" nil "GITHUB_TOKEN"]])
           (audit-kinds)))))

(deftest concurrent-requests-share-one-pending-approval
  (seed! "ask")
  (let [answers (->> (range 20) (mapv (fn [_] (future (decide!)))) (mapv deref))]
    (is (= #{{:decision "pending" :approval "1"}} (set answers)))))

(defn- approval-states []
  (store/read *store* (fn [db] (mapv #((juxt :id :state) (store/approval db %)) (range 1 4)))))

(defn- approval-audits []
  (->> (store/read *store* #(store/audit % {:kind "approval"} 100))
       reverse
       (mapv (juxt :actor (comp :state :detail)))))

(deftest an-approval-covers-only-the-host-it-was-requested-for
  (write! {:command :credential/create :actor "casey"
           :credential {:name "GITHUB_TOKEN" :vault "vlt1" :item "itm1" :field "fld1"
                        :vault-title "Agents" :item-title "GitHub token" :field-label "credential" :header "Authorization"
                        :hosts ["api.github.com" "uploads.github.com"]}})
  (write! {:command :assignment/assign :credentials ["GITHUB_TOKEN"] :subjects ["group:agents"]
           :mode "inject" :approval "ask" :actor "casey"})
  (decide!)
  (write! {:command :approval/event :id 1 :event :approve :grant "once" :actor "casey"})
  (is (= {:decision "pending" :approval "2"} (decide! {:host "uploads.github.com"})))
  (is (= "1" (:approval (decide! {:host "API.github.com."}))))
  (testing "a block covers every host"
    (write! {:command :approval/event :id 2 :event :block :actor "casey"})
    (is (= {:decision "deny" :reason "blocked"} (decide!)))))

(deftest unblocking-lifts-the-block-on-every-host
  (write! {:command :credential/create :actor "casey"
           :credential {:name "GITHUB_TOKEN" :vault "vlt1" :item "itm1" :field "fld1"
                        :vault-title "Agents" :item-title "GitHub token" :field-label "credential" :header "Authorization"
                        :hosts ["api.github.com" "uploads.github.com"]}})
  (write! {:command :assignment/assign :credentials ["GITHUB_TOKEN"] :subjects ["group:agents"]
           :mode "inject" :approval "ask" :actor "casey"})
  (decide!)
  (decide! {:host "uploads.github.com"})
  (write! {:command :approval/event :id 1 :event :block :actor "casey"})
  (write! {:command :approval/event :id 2 :event :block :actor "casey"})
  (write! {:command :approval/event :id 2 :event :unblock :actor "casey"})
  (is (= [{:decision "pending" :approval "3"} {:decision "pending" :approval "4"}]
         [(decide!) (decide! {:host "uploads.github.com"})])))

(deftest deleting-a-credential-supersedes-its-approvals
  (seed! "ask")
  (decide!)
  (decide! {:identity "tag:t3-bob"})
  (write! {:command :approval/event :id 2 :event :approve :grant "forever" :actor "casey"})
  (decide! {:identity "tag:t3-carol"})
  (write! {:command :approval/event :id 3 :event :block :actor "casey"})
  (write! {:command :credential/delete :name "GITHUB_TOKEN" :actor "kim"
           :revision (:revision (store/read *store* #(store/credential % "GITHUB_TOKEN")))})
  (is (= [nil [] [[1 :superseded] [2 :superseded] [3 :closed]] {:decision "deny" :reason "unknown"}]
         [(store/read *store* #(store/credential % "GITHUB_TOKEN"))
          (store/read *store* store/assignments)
          (approval-states)
          (decide!)]))
  (is (= [["kim" "superseded"] ["kim" "superseded"] ["kim" "closed"]]
         (take-last 3 (approval-audits)))))

(deftest approvals-record-the-revision-and-host-they-were-requested-under
  (seed! "ask")
  (decide! {:host "API.github.com."})
  (is (= {:revision (:revision (store/read *store* #(store/credential % "GITHUB_TOKEN")))
          :host     "api.github.com"}
         (select-keys (store/read *store* #(store/approval % 1)) [:revision :host]))))

(deftest editing-a-credential-supersedes-its-approvals
  (seed! "ask")
  (decide!)
  (decide! {:identity "tag:t3-bob"})
  (write! {:command :approval/event :id 2 :event :approve :grant "forever" :actor "casey"})
  (decide! {:identity "tag:t3-carol"})
  (write! {:command :approval/event :id 3 :event :block :actor "casey"})
  (let [{:keys [revision]} (store/read *store* #(store/credential % "GITHUB_TOKEN"))]
    (write! {:command :credential/update :name "GITHUB_TOKEN" :revision revision :actor "kim"
             :header "Authorization" :hosts ["*.github.com"]}))
  (is (= [[1 :superseded] [2 :superseded] [3 :blocked]] (approval-states)))
  (is (= [["kim" "superseded"] ["kim" "superseded"]] (take-last 2 (approval-audits))))
  (is (= ["Approval 1 was superseded by a change to its credential. The next request asks again."]
         (store/invalid-errors
          (:error @(store/submit! *store* {:command :approval/event :id 1 :event :approve :grant "once"
                                           :actor "casey"}))))
      "approving a superseded approval is refused")
  (is (= [{:decision "pending" :approval "4"} {:decision "pending" :approval "5"} {:decision "deny" :reason "blocked"}]
         [(decide!) (decide! {:identity "tag:t3-bob"}) (decide! {:identity "tag:t3-carol"})])
      "the next request asks again under the new revision")
  (is (= (:revision (store/read *store* #(store/credential % "GITHUB_TOKEN")))
         (:revision (store/read *store* #(store/approval % 4))))))

(deftest a-save-without-changes-keeps-the-approvals
  (seed! "ask")
  (decide!)
  (let [{:keys [revision]} (store/read *store* #(store/credential % "GITHUB_TOKEN"))]
    (write! {:command :credential/update :name "GITHUB_TOKEN" :revision revision :actor "kim"
             :header "Authorization" :hosts ["api.github.com"]})
    (is (= [revision :pending]
           [(:revision (store/read *store* #(store/credential % "GITHUB_TOKEN")))
            (:state (store/read *store* #(store/approval % 1)))]))))

(deftest writer-syncs-in-full
  (is (= [{:synchronous 2}] (sq/query (:writer *store*) "pragma synchronous"))))

(defn- start-error [path]
  (try
    (store/stop! (store/start! path {:audit-out (fn [_])}))
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-message e))))

(deftest opens-only-a-database-of-the-current-schema
  (is (= [{:user_version store/schema-version}] (sq/query (:writer *store*) "pragma user_version")))
  (is (nil? (start-error (:path *store*))) "a database it created opens again")
  (let [unversioned (str (fs/path *dir* "unversioned.db"))
        newer (str (fs/path *dir* "newer.db"))
        first-release (str (fs/path *dir* "first-release.db"))]
    (doto (sq/open first-release)
      (sq/execute! "pragma user_version = 1")
      (sq/close!))
    (is (= (str "The database " first-release " has schema version 1 and ts-broker needs "
                store/schema-version ". ts-broker does not migrate databases.")
           (start-error first-release))
        "approvals became per host after version 1")
    (doto (sq/open unversioned)
      (sq/execute! "create table credential (name text primary key)")
      (sq/close!))
    (doto (sq/open newer)
      (sq/execute! "pragma user_version = 99")
      (sq/close!))
    (is (= [(str "The database " unversioned " has schema version 0 and ts-broker needs "
                 store/schema-version ". ts-broker does not migrate databases.")
            (str "The database " newer " has schema version 99 and ts-broker needs "
                 store/schema-version ". ts-broker does not migrate databases.")]
           [(start-error unversioned) (start-error newer)]))))

(defn- credential []
  (store/read *store* #(store/credential % "GITHUB_TOKEN")))

(deftest updates-change-only-header-and-hosts
  (seed! "auto")
  (let [{:keys [revision] :as before} (credential)]
    (write! {:command :credential/update :name "GITHUB_TOKEN" :revision revision :actor "kim"
             :header "X-Api-Key" :hosts ["*.GitHub.com"] :vault "evil" :item "evil" :field "evil"})
    (is (= (assoc (select-keys before [:name :vault :item :field :field-label :created])
                  :header "X-Api-Key" :hosts ["*.github.com"] :editor "kim")
           (select-keys (credential) [:name :vault :item :field :field-label :created :header :hosts :editor])))
    (is (not= revision (:revision (credential))))
    (is (= ["This credential changed or was deleted since the form was shown. Reload the page."]
           (store/invalid-errors
            (:error @(store/submit! *store* {:command :credential/update :name "GITHUB_TOKEN" :revision revision
                                             :actor "kim" :header "Authorization" :hosts ["api.github.com"]}))))
        "a second edit from the same form is refused")))

(deftest title-refreshes-change-only-titles-and-keep-the-revision
  (seed! "auto")
  (let [before (credential)
        renamed {:name "GITHUB_TOKEN" :vault-title "Agents" :item-title "GitHub bot" :field-label "token"}]
    (is (= 1 (write! {:command :credential/titles
                      :titles  [renamed
                                {:name "NOPE" :vault-title "A" :item-title "B" :field-label "C"}
                                {:name "GITHUB_TOKEN" :vault-title "" :item-title "x" :field-label "y"}]})))
    (is (= (merge before (dissoc renamed :name)) (credential)))
    (is (= 0 (write! {:command :credential/titles :titles [renamed]})) "an unchanged title is not written again")
    (is (= ["credential" nil "GITHUB_TOKEN" {:action "titles" :item-title "GitHub bot" :field-label "token"}]
           ((juxt :kind :actor :credential #(select-keys (:detail %) [:action :item-title :field-label]))
            (first (store/read *store* #(store/audit % {:kind "credential"} 1))))))))

(deftest updates-refuse-a-recreated-credential
  (seed! "auto")
  (let [{:keys [revision]} (credential)]
    (write! {:command :credential/delete :name "GITHUB_TOKEN" :revision (:revision (credential)) :actor "kim"})
    (write! {:command :credential/create :actor "kim"
             :credential {:name "GITHUB_TOKEN" :vault "vlt2" :item "itm2" :field "fld2"
                          :vault-title "Other" :item-title "Other" :field-label "other" :header "Authorization" :hosts ["api.github.com"]}})
    (is (= ["This credential changed or was deleted since the form was shown. Reload the page."]
           (store/invalid-errors
            (:error @(store/submit! *store* {:command :credential/update :name "GITHUB_TOKEN" :revision revision
                                             :actor "casey" :header "X-Api-Key" :hosts ["api.github.com"]})))))
    (is (= {:item "itm2" :header "Authorization"} (select-keys (credential) [:item :header])))))

(deftest deletes-only-the-revision-the-form-showed
  (seed! "auto")
  (let [{:keys [revision]} (credential)]
    (write! {:command :credential/update :name "GITHUB_TOKEN" :revision revision
             :actor "kim" :header "X-Api-Key" :hosts ["api.github.com"]})
    (is (= ["This credential changed or was deleted since the form was shown. Reload the page."]
           (store/invalid-errors
            (:error @(store/submit! *store* {:command :credential/delete :name "GITHUB_TOKEN"
                                             :revision revision :actor "casey"})))))
    (is (= "X-Api-Key" (:header (credential))))
    (write! {:command :credential/delete :name "GITHUB_TOKEN" :revision (:revision (credential)) :actor "casey"})
    (is (nil? (credential)))))

(deftest deleting-a-missing-credential-changes-nothing
  (let [before (audit-kinds)]
    (is (= ["This credential changed or was deleted since the form was shown. Reload the page."]
           (store/invalid-errors
            (:error @(store/submit! *store* {:command :credential/delete :name "NOPE" :actor "casey"})))))
    (is (= before (audit-kinds)))))

(defn- connect ^SocketChannel [path]
  (doto (SocketChannel/open StandardProtocolFamily/UNIX)
    (.connect (UnixDomainSocketAddress/of (str path)))))

(defn- ask-socket [path lines]
  (with-open [ch (connect path)]
    (let [out (Channels/newOutputStream ch)
          in (BufferedReader. (InputStreamReader. (Channels/newInputStream ch)))]
      (mapv (fn [line]
              (.write out (.getBytes (str line "\n") "UTF-8"))
              (.flush out)
              (some-> (.readLine in) (json/parse-string true)))
            lines))))

(defn- with-socket [opts f]
  (let [path (fs/path *dir* "decide.sock")
        server (socket/start! *store* (str path) opts)]
    (try
      (f path)
      (finally
        (socket/stop! server)))))

(def malformed {:decision "deny" :reason "malformed"})

(deftest socket-answers-ndjson-requests-in-sequence
  (seed! "auto")
  (with-socket {}
    (fn [path]
      (is (= "rw-rw----" (fs/posix->str (fs/posix-file-permissions path))))
      (is (= [allow malformed malformed malformed {:decision "deny" :reason "unknown"}]
             (ask-socket path [(json/generate-string request)
                               "not json"
                               (json/generate-string (dissoc request :mode))
                               (json/generate-string (assoc request :credential "github_token"))
                               (json/generate-string (assoc request :credential "OTHER"))]))))))

(deftest socket-refuses-malformed-requests-before-the-audit
  (seed! "auto")
  (let [before (audit-kinds)]
    (with-socket {:max-line 4096}
      (fn [path]
        (is (= [malformed malformed malformed]
               (ask-socket path [(json/generate-string (assoc request :credential (apply str (repeat 65 "A"))))
                                 (json/generate-string (assoc request :identity (apply str (repeat 2000 "x"))))
                                 (json/generate-string (assoc request :node (apply str (repeat 300 "x"))))])))
        (with-open [ch (connect path)]
          (let [lines (str (json/generate-string (assoc request :groups [(apply str (repeat 5000 "g"))])) "\n"
                           (json/generate-string request) "\n")
                in (BufferedReader. (InputStreamReader. (Channels/newInputStream ch)))]
            (.write ch (java.nio.ByteBuffer/wrap (.getBytes lines "UTF-8")))
            (is (= [malformed nil] [(json/parse-string (.readLine in) true) (.readLine in)])
                "an over-long line is answered and closes the connection")))))
    (is (= before (audit-kinds)))))

(deftest socket-needs-one-object-per-terminated-line
  (seed! "auto")
  (let [before (audit-kinds)
        line (json/generate-string request)]
    (with-socket {}
      (fn [path]
        (is (= [malformed malformed malformed]
               (ask-socket path [(str line line) (str line " x") (str line " {}")])))
        (with-open [ch (connect path)]
          (let [in (BufferedReader. (InputStreamReader. (Channels/newInputStream ch)))]
            (.write ch (java.nio.ByteBuffer/wrap (.getBytes line "UTF-8")))
            (.shutdownOutput ch)
            (is (= [malformed nil] [(json/parse-string (.readLine in) true) (.readLine in)])
                "a request without its newline at the end of the stream is malformed")))))
    (is (= before (audit-kinds)))))

(defn- closed-within? [^SocketChannel ch ms]
  (let [read (future (.read ch (java.nio.ByteBuffer/allocate 1)))]
    (= -1 (deref read ms ::open))))

(deftest socket-closes-idle-connections
  (with-socket {:read-timeout-ms 200}
    (fn [path]
      (with-open [ch (connect path)]
        (.write ch (java.nio.ByteBuffer/wrap (.getBytes "{\"op\":" "UTF-8")))
        (is (closed-within? ch 2000))))))

(deftest socket-caps-open-connections
  (seed! "auto")
  (with-socket {:max-connections 2}
    (fn [path]
      (with-open [a (connect path)
                  b (connect path)
                  c (connect path)]
        (is (closed-within? c 2000) "the third connection is refused")
        (is (not (closed-within? a 200)))
        (.close a)
        (.close b)
        (Thread/sleep 100)
        (is (= [allow] (ask-socket path [(json/generate-string request)]))
            "a closed connection frees its slot")))))
