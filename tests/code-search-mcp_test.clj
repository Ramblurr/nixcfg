(load-file (str (.getParent (java.io.File. *file*)) "/../pkgs/code-search-mcp/code-search-mcp.clj"))

#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns code-search.mcp-test
  (:require [babashka.fs :as fs]
            [babashka.http-client :as jdk-client]
            [babashka.process :as process]
            [babashka.sqlite :as sqlite]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [code-search.mcp :as sut]
            [org.httpkit.client :as client]
            [org.httpkit.server :as http])
  (:import [java.net ServerSocket]
           [java.util.concurrent CompletableFuture Semaphore TimeUnit]))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)] (.getLocalPort socket)))

(def ^:dynamic *endpoint* "/mcp")

(defn- call-http [origin method message headers]
  (let [result @(client/request
                 {:url (str origin *endpoint*) :method method :as :text :timeout 3000
                  :headers (merge {"content-type" "application/json"
                                   "accept" "application/json, text/event-stream"}
                                  headers)
                  :body (when message (if (string? message) message (json/generate-string message)))})
        data (some #(when (str/starts-with? % "data: ") (subs % 6))
                   (str/split-lines (or (:body result) "")))]
    (cond-> result data (assoc :message (json/parse-string data true)))))

(defn- initialize [origin]
  (call-http origin :post
             {:jsonrpc "2.0" :id 1 :method "initialize"
              :params {:protocolVersion "2025-11-25" :capabilities {}
                       :clientInfo {:name "code-search-test" :version "1"}}} {}))

(defn- session-headers [initialized]
  {"mcp-session-id" (get-in initialized [:headers :mcp-session-id])
   "mcp-protocol-version" "2025-11-25"})

(defn- fixture [f]
  (let [backend-port (free-port)
        port (free-port)
        origin (str "http://127.0.0.1:" port)
        backend (str "http://127.0.0.1:" backend-port)
        seen (atom [])
        stop-backend
        (http/run-server
         (fn [request]
           (let [payload (json/parse-string (slurp (:body request)) true)
                 query (:query payload)]
             (swap! seen conj {:path (:uri request) :payload payload})
             (when (= query "slow") (Thread/sleep 300))
             {:status 200 :headers {"content-type" "application/json"}
              :body (json/generate-string
                     (cond
                       (= query "invalid") {:kind "error" :error "invalid regex"}
                       (= query "large") {:kind "success" :results {:value (apply str (repeat 4096 "a"))}}
                       (= (:uri request) "/search/api/list")
                       {:kind "success" :results {:repositories [{:name "example/repo"}]}}
                       :else {:kind "success" :results {:files [{:fileName {:text "main.go"}
                                                                 :fileUrl "https://src.example.test/tree/main.go?id=abc"}]}}))}))
         {:ip "127.0.0.1" :port backend-port})
        stop (sut/start! {:port port :origin origin :neogrok-url (str backend "/search")})]
    (try (f {:origin origin :backend backend :seen seen})
         (finally (stop) (stop-backend :timeout 1000)))))

(deftest real-http-lifecycle-and-search
  (fixture
   (fn [{:keys [origin seen]}]
     (let [init (initialize origin)
           headers (session-headers init)]
       (is (= {:status 200 :protocol-version "2025-11-25"}
              {:status (:status init) :protocol-version (get-in init [:message :result :protocolVersion])}))
       (is (string? (get headers "mcp-session-id")))
       (is (= 202 (:status (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers))))
       (let [listed (call-http origin :post {:jsonrpc "2.0" :id 2 :method "tools/list" :params {}} headers)]
         (is (= #{"grep" "list_repos"} (set (map :name (get-in listed [:message :result :tools]))))))
       (doseq [[tool args] [["list_repos" {:query "" :format "json"}]
                            ["grep" {:query "sym:main" :contextLines 3 :files 4 :matches 5 :format "json"}]]]
         (let [result (call-http origin :post {:jsonrpc "2.0" :id 3 :method "tools/call"
                                               :params {:name tool :arguments args}} headers)
               value (some-> (get-in result [:message :result :content 0 :text]) (json/parse-string true))]
           ;; MCP permits success to omit isError; normalize that omission to false.
           (is (= {:status 200 :is-error false :value (if (= tool "grep") "main.go" "example/repo")}
                  {:status (:status result)
                   :is-error (boolean (get-in result [:message :result :isError]))
                   :value (if (= tool "grep")
                            (get-in value [:results 0 :path])
                            (get-in value [:results 0 :id]))}))))
       (is (= {:path "/search/api/search" :payload {:query "sym:main" :contextLines 3 :files 4 :matches 5}}
              (last @seen)))
       (is (= 204 (:status (call-http origin :delete nil headers))))
       (is (= 404 (:status (call-http origin :post {:jsonrpc "2.0" :id 4 :method "tools/list"} headers))))))))

(deftest june-protocol-and-pre-initialization-rejection
  (fixture
   (fn [{:keys [origin]}]
     (let [init (call-http origin :post
                           {:jsonrpc "2.0" :id 1 :method "initialize"
                            :params {:protocolVersion "2025-06-18" :capabilities {}
                                     :clientInfo {:name "test" :version "1"}}} {})
           headers (assoc (session-headers init) "mcp-protocol-version" "2025-06-18")]
       (is (= "2025-06-18" (get-in init [:message :result :protocolVersion])))
       (is (get-in (call-http origin :post {:jsonrpc "2.0" :id 2 :method "tools/list"} headers)
                   [:message :error]))
       (is (= 202 (:status (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers))))
       (is (= 2 (count (get-in (call-http origin :post {:jsonrpc "2.0" :id 3 :method "tools/list"} headers)
                               [:message :result :tools]))))))))

(deftest advertises-and-accepts-only-supported-methods
  (fixture
   (fn [{:keys [origin]}]
     (doseq [version sut/protocol-versions]
       (let [init (call-http origin :post
                             {:jsonrpc "2.0" :id 1 :method "initialize"
                              :params {:protocolVersion version :capabilities {}
                                       :clientInfo {:name "test" :version "1"}}} {})
             headers (assoc (session-headers init) "mcp-protocol-version" version)]
         (is (= {:tools {}} (get-in init [:message :result :capabilities])))
         (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers)
         (doseq [method ["resources/subscribe" "resources/list" "prompts/list" "logging/setLevel"
                         "completion/complete" "subscriptions/listen" "tasks/list"]]
           (is (= -32601
                  (get-in (call-http origin :post
                                     {:jsonrpc "2.0" :id 2 :method method
                                      :params {:uri "file:///anything" :level "debug"}} headers)
                          [:message :error :code])))))))))

(deftest cancellation-over-http-closes-streams
  (doseq [action [:notification :delete :disconnect]]
    (let [port (free-port)
          backend-port (free-port)
          origin (str "http://127.0.0.1:" port)
          started (promise)
          closed (promise)
          slots (Semaphore. 1)
          sessions (atom {})
          stop-backend (http/run-server
                        (fn [request]
                          (http/as-channel request
                                           {:on-open (fn [channel]
                                                       (http/send! channel {:status 200 :body " "} false)
                                                       (deliver started true))
                                            :on-close (fn [_ _] (deliver closed true))}))
                        {:ip "127.0.0.1" :port backend-port})
          stop (http/run-server (sut/handler origin sessions
                                             (sut/search-tools (str "http://127.0.0.1:" backend-port) slots))
                                {:ip "127.0.0.1" :port port})]
      (try
        (let [headers (session-headers (initialize origin))]
          (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers)
          (let [message {:jsonrpc "2.0" :id 7 :method "tools/call"
                         :params {:name "grep" :arguments {:query "main"}}}
                pending (if (= :disconnect action)
                          (jdk-client/request {:uri (str origin "/mcp") :method :post
                                               :async true :raw true :timeout 2000
                                               :headers (merge {"content-type" "application/json"
                                                                "accept" "application/json, text/event-stream"} headers)
                                               :body (json/generate-string message)})
                          (future (call-http origin :post message headers)))]
            (is (= true (deref started 1000 false)))
            (case action
              :delete (is (= 204 (:status (call-http origin :delete nil headers))))
              :notification (is (= 202 (:status (call-http origin :post
                                                           {:jsonrpc "2.0" :method "notifications/cancelled"
                                                            :params {:requestId 7}} headers))))
              :disconnect (when-not (.cancel ^CompletableFuture pending true)
                            (.close (.body (.join ^CompletableFuture pending)))))
            (when-not (= :disconnect action)
              (is (= {:status 200 :body ""} (select-keys (deref pending 1000 {}) [:status :body]))))
            (is (= true (deref closed 1000 false)))
            (is (.tryAcquire slots 1000 TimeUnit/MILLISECONDS))))
        (finally (stop :timeout 100) (stop-backend :timeout 100))))))

(deftest transport-rejects-untrusted-and-invalid-requests
  (fixture
   (fn [{:keys [origin]}]
     (let [init (initialize origin)
           headers (session-headers init)
           request {:jsonrpc "2.0" :id 2 :method "tools/list"}]
       (is (= 403 (:status (call-http origin :post request (assoc headers "host" "attacker.example")))))
       (is (= 403 (:status (call-http origin :post request (assoc headers "origin" "https://attacker.example")))))
       (is (= 400 (:status (call-http origin :post request {}))))
       (doseq [[status modified] [[400 (dissoc headers "mcp-protocol-version")]
                                  [400 (assoc headers "mcp-protocol-version" "1900-01-01")]
                                  [415 (assoc headers "content-type" "text/plain")]
                                  [406 (assoc headers "accept" "application/json")]]]
         (is (= status (:status (call-http origin :post request modified)))))
       (is (= 405 (:status (call-http origin :get nil headers))))
       (is (= 400 (:status (call-http origin :post "{broken" headers))))
       (is (= 400 (:status (call-http origin :post [] headers))))
       (is (= 413 (:status (call-http origin :post (apply str (repeat 70000 "x")) headers))))))))

(deftest tool-errors-and-invalid-arguments
  (fixture
   (fn [{:keys [origin seen]}]
     (let [headers (session-headers (initialize origin))]
       (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers)
       (let [result (call-http origin :post {:jsonrpc "2.0" :id 2 :method "tools/call"
                                             :params {:name "grep" :arguments {:query "invalid"}}} headers)]
         (is (= {:is-error true :text "invalid regex"}
                {:is-error (get-in result [:message :result :isError])
                 :text (get-in result [:message :result :content 0 :text])})))
       (let [count-before (count @seen)
             result (call-http origin :post {:jsonrpc "2.0" :id 3 :method "tools/call"
                                             :params {:name "grep" :arguments {:query "hello" :files 10000}}} headers)]
         (is (= -32602 (get-in result [:message :error :code])))
         (is (= count-before (count @seen))))))))

(deftest bounded-upstream-and-overload
  (fixture
   (fn [{:keys [backend]}]
     (with-redefs [sut/max-response-bytes 1024]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"limit exceeded"
                             (sut/execute-search (str backend "/search") "grep" {:query "large"}))))
     (with-redefs [sut/upstream-timeout-ms 25]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"deadline"
                             (sut/execute-search (str backend "/search") "grep" {:query "slow"}))))
     (let [tools (sut/search-tools (str backend "/search") (Semaphore. 0))]
       (is (:is-error ((:tool-fn (first tools)) {} {:query "hello"})))))))

(deftest total-deadline-closes-dribbling-upstream
  (let [port (free-port)
        closed (promise)
        stop (http/run-server
              (fn [request]
                (http/as-channel
                 request
                 {:on-close (fn [_ _] (deliver closed true))
                  :on-open (fn [channel]
                             (http/send! channel {:status 200 :headers {"content-type" "application/json"}} false)
                             (future
                               (dotimes [_ 100]
                                 (http/send! channel " " false)
                                 (Thread/sleep 20))
                               (http/close channel)))}))
              {:ip "127.0.0.1" :port port})]
    (try
      (with-redefs [sut/upstream-timeout-ms 100]
        (let [result (future
                       (try (sut/execute-search (str "http://127.0.0.1:" port) "grep" {:query "main"})
                            (catch Exception e (ex-message e))))]
          (is (re-find #"deadline" (str (deref result 1000 ::hung))))
          (is (= true (deref closed 500 false)))))
      (finally (stop :timeout 100)))))

(deftest cancellation-aborts-upstream-and-releases-slot
  (let [port (free-port)
        started (promise)
        closed (promise)
        cancelled (atom false)
        slots (Semaphore. 1)
        stop (http/run-server
              (fn [request]
                (http/as-channel request
                                 {:on-open (fn [channel]
                                             (http/send! channel {:status 200 :body " "} false)
                                             (deliver started true))
                                  :on-close (fn [_ _] (deliver closed true))}))
              {:ip "127.0.0.1" :port port})]
    (try
      (let [tool (first (sut/search-tools (str "http://127.0.0.1:" port) slots))
            result ((:tool-fn tool) {:is-cancelled cancelled} {:query "main"})]
        (is (= true (deref started 1000 false)))
        (reset! cancelled true)
        (is (:is-error (deref result 1000 {})))
        (is (= true (deref closed 1000 false)))
        (is (= 1 (.availablePermits slots))))
      (finally (stop :timeout 100)))))

(deftest session-capacity-and-expiry
  (fixture
   (fn [{:keys [origin]}]
     (with-redefs [sut/max-sessions 1]
       (let [first-session (initialize origin)]
         (is (= 200 (:status first-session)))
         (is (= 503 (:status (initialize origin))))
         (is (= 204 (:status (call-http origin :delete nil (session-headers first-session)))))
         (is (= 200 (:status (initialize origin))))))))
  (let [cancelled (atom false)
        sessions (atom {"old" {:last-seen 0 :session (atom {:is-cancelled-by-request-id {1 cancelled}})}
                        "new" {:last-seen sut/session-idle-ms :session (atom {})}})]
    (sut/expire-sessions! sessions sut/session-idle-ms)
    (is (= #{"new"} (set (keys @sessions))))
    (is @cancelled)))

(defn- fixture-git! [dir & args]
  (:out (apply process/shell {:dir (str dir) :out :string :err :string
                              :extra-env {"GIT_CONFIG_GLOBAL" "/dev/null" "GIT_CONFIG_NOSYSTEM" "1"}}
               "git" "-c" "user.name=Fixture" "-c" "user.email=fixture@example.test" args)))

(defn- git-fixture [f]
  (let [temp (fs/create-temp-dir {:prefix "code-search-git-"})
        work (fs/path temp "work")
        root (fs/path temp "mirrors")
        mirror (fs/path root "example/repo.git")]
    (try
      (fs/create-dirs (fs/path work "src"))
      (fs/create-dirs (fs/parent mirror))
      (fixture-git! work "init" "-b" "main")
      (spit (str (fs/path work "src/core.clj")) "old\nline two\n")
      (spit (str (fs/path work "space name.txt")) "spaces\n")
      (spit (str (fs/path work "line\nbreak.txt")) "newline path\n")
      (fs/create-sym-link (fs/path work "link") "/etc/passwd")
      (fixture-git! work "add" ".")
      (fixture-git! work "commit" "-m" "initial")
      (fixture-git! temp "clone" "--bare" (str work) (str mirror))
      (let [catalog (sut/open-catalog! root (fs/path temp "state"))
            head (str/trim (fixture-git! work "rev-parse" "HEAD"))]
        (f {:catalog catalog :work work :mirror mirror :head head :root root}))
      (finally (fs/delete-tree temp)))))

(deftest catalog-publication-and-failed-refresh
  (git-fixture
   (fn [{:keys [catalog work mirror head root]}]
     (let [index {"example/repo" {"branches" [{"name" "HEAD" "version" head}]}}
           db (:db catalog)]
       (fs/create-sym-link (fs/path root "escape.git") work)
       (is (= ["example/repo"] (sut/discover-mirrors root)))
       (is (true? (sut/refresh-catalog! catalog index)))
       (is (= #{"src" "src/core.clj" "space name.txt" "line\nbreak.txt" "link"}
              (set (map :path (sqlite/query db "SELECT path FROM tree")))))
       (is (= [{:head head :warning nil}] (sqlite/query db "SELECT head,warning FROM repositories")))
       (let [reads (atom 0)
             read-tree sut/read-git-tree]
         (with-redefs [sut/read-git-tree (fn [& args] (swap! reads inc) (apply read-tree args))]
           (sut/refresh-catalog! catalog index)
           (is (zero? @reads))))
       (spit (str (fs/path work "second.txt")) "second\n")
       (fixture-git! work "add" ".")
       (fixture-git! work "commit" "-m" "second")
       (fixture-git! work "push" (str mirror) "main:main")
       (sut/refresh-catalog! catalog index)
       (let [published (sqlite/query db "SELECT head,indexed FROM repositories")
             refs (sqlite/query db "SELECT * FROM refs")
             tree (sqlite/query db "SELECT * FROM tree")]
         (is (not= head (:head (first published))))
         (is (= head (get-in (json/parse-string (:indexed (first published))) ["branches" 0 "version"])))
         (spit (str (fs/path mirror "HEAD")) "ref: refs/heads/missing\n")
         (sut/refresh-catalog! catalog nil)
         (is (= published (sqlite/query db "SELECT head,indexed FROM repositories")))
         (is (= refs (sqlite/query db "SELECT * FROM refs")))
         (is (= tree (sqlite/query db "SELECT * FROM tree")))
         (is (string? (:warning (first (sqlite/query db "SELECT warning FROM repositories"))))))))))

(deftest git-bounds-and-path-policy
  (git-fixture
   (fn [{:keys [catalog mirror head]}]
     (sut/refresh-catalog! catalog {})
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown repository"
                           (sut/mirror-path catalog "../../etc")))
     (doseq [revision ["--help" "HEAD~1" "main:src/core.clj" "HEAD^{tree}"]]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown revision"
                             (sut/read-repository-file catalog {:repository "example/repo" :revision revision :path "src/core.clj"}))))
     (with-redefs [sut/max-git-bytes 1]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"limit exceeded"
                             (sut/run-git (str mirror) ["ls-tree" "-r" head]))))
     (with-redefs [sut/git-timeout-ms 0]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"deadline"
                             (sut/run-git (str mirror) ["ls-tree" "-r" head]))))
     (binding [sut/*cancelled* (atom true)]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cancelled"
                             (sut/run-git (str mirror) ["ls-tree" "-r" head]))))
     (spit (str (fs/path mirror "objects/info/alternates")) "/etc\n")
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsafe mirror layout"
                           (sut/mirror-path catalog "example/repo"))))))

(deftest concurrent-refresh-publishes-whole-generations
  (git-fixture
   (fn [{:keys [catalog work mirror head]}]
     (sut/refresh-catalog! catalog {})
     (.acquire ^Semaphore (:refresh-slot catalog))
     (try (is (false? (sut/refresh-catalog! catalog {})))
          (finally (.release ^Semaphore (:refresh-slot catalog))))
     (dotimes [i 100] (spit (str (fs/path work (str "file-" i))) "x"))
     (fixture-git! work "add" ".")
     (fixture-git! work "commit" "-m" "large generation")
     (fixture-git! work "push" (str mirror) "main:main")
     (let [new-head (str/trim (fixture-git! work "rev-parse" "HEAD"))
           gate (promise)
           refresh (future @gate (sut/refresh-catalog! catalog {}))
           reader (future @gate
                          (doall (repeatedly 100 #(first (sqlite/query (:db catalog)
                                                                       "SELECT head,COUNT(path) files FROM repositories JOIN tree ON repositories.id=tree.repo GROUP BY id")))))]
       (deliver gate true)
       (is (true? (deref refresh 10000 false)))
       (is (every? #{{:head head :files 5} {:head new-head :files 105}} (deref reader 10000 [{}])))
       (is (= new-head (:head (first (sqlite/query (:db catalog) "SELECT head FROM repositories")))))))))

(deftest git-timeout-reaps-child
  (git-fixture
   (fn [{:keys [mirror head]}]
     (let [children (atom []) real-process process/process]
       (with-redefs [process/process (fn [& args]
                                       (let [child (apply real-process args)]
                                         (swap! children conj (:proc child)) child))
                     sut/git-timeout-ms 0]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"deadline"
                               (sut/run-git (str mirror) ["ls-tree" "-r" head]))))
       (is (= 1 (count @children)))
       (is (every? #(not (.isAlive ^Process %)) @children))))))

(deftest text-file-boundaries-and-literal-paths
  (git-fixture
   (fn [{:keys [catalog work mirror]}]
     (spit (str (fs/path work "unicode.txt")) "�\r\n\r\nlast\r\n" :encoding "UTF-8")
     (spit (str (fs/path work ":(glob)*")) "literal path")
     (with-open [out (io/output-stream (str (fs/path work "bad.bin")))]
       (.write out (byte-array [-61 40])))
     (fixture-git! work "add" ".")
     (fixture-git! work "commit" "-m" "text cases")
     (fixture-git! work "push" (str mirror) "main:main")
     (sut/refresh-catalog! catalog {})
     (is (= {:lines ["�" "" "last"] :total-lines 3}
            (select-keys (sut/read-repository-file catalog {:repository "example/repo" :path "unicode.txt"}) [:lines :total-lines])))
     (is (= ["literal path"] (:lines (sut/read-repository-file catalog {:repository "example/repo" :path ":(glob)*"}))))
     (let [old (:head (first (sqlite/query (:db catalog) "SELECT head FROM repositories")))
           traversals (atom 0) original sut/read-git-tree]
       (fixture-git! work "commit" "--allow-empty" "-m" "metadata only")
       (fixture-git! work "push" (str mirror) "main:main")
       (with-redefs [sut/read-git-tree (fn [& args] (swap! traversals inc) (apply original args))]
         (sut/refresh-catalog! catalog {})
         (is (zero? @traversals)))
       (is (= ["literal path"] (:lines (sut/read-repository-file catalog {:repository "example/repo" :revision old :path ":(glob)*"})))))
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"UTF-8"
                           (sut/read-repository-file catalog {:repository "example/repo" :path "bad.bin"}))))))

(deftest rejects-internal-mirror-symlinks
  (git-fixture
   (fn [{:keys [catalog mirror work]}]
     (sut/refresh-catalog! catalog {})
     (fs/create-sym-link (fs/path mirror "objects/pack/outside.pack") (fs/path work "src/core.clj"))
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsafe"
                           (sut/read-repository-file catalog {:repository "example/repo" :path "src/core.clj"}))))))

(deftest concurrent-cache-writes
  (git-fixture
   (fn [{:keys [catalog]}]
     (let [gate (promise)
           workers (doall (repeatedly 8 #(future @gate (sut/cached-result catalog ["same"] (constantly {:value "same"})))))]
       (deliver gate true)
       (is (= (repeat 8 {:value "same"}) (mapv #(deref % 5000 ::timeout) workers)))
       (is (= 1 (:n (first (sqlite/query (:db catalog) "SELECT COUNT(*) n FROM cache")))))
       (is (= 1 (count (fs/list-dir (fs/path (:state-dir catalog) "cache")))))))))

(deftest pruned-revision-does-not-fall-back-to-head
  (git-fixture
   (fn [{:keys [catalog work mirror head]}]
     (sut/refresh-catalog! catalog {})
     (fixture-git! work "checkout" "--orphan" "replacement")
     (fixture-git! work "rm" "-rf" ".")
     (fs/create-dirs (fs/path work "src"))
     (spit (str (fs/path work "src/core.clj")) "replacement\n")
     (fixture-git! work "add" ".")
     (fixture-git! work "commit" "-m" "new root")
     (fixture-git! work "push" "--force" (str mirror) "replacement:main")
     (sut/refresh-catalog! catalog {})
     (sut/list-repository-files catalog {:repository "example/repo" :revision head})
     (fixture-git! mirror "reflog" "expire" "--expire=now" "--all")
     (fixture-git! mirror "gc" "--prune=now")
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"stale"
                           (sut/read-repository-file catalog {:repository "example/repo" :revision head :path "src/core.clj"})))
     (is (= ["replacement"] (:lines (sut/read-repository-file catalog {:repository "example/repo" :path "src/core.clj"}))))
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a directory"
                           (sut/list-repository-files catalog {:repository "example/repo" :path "src/core.clj"}))))))

(deftest cache-entry-controls
  (git-fixture
   (fn [{:keys [catalog]}]
     (with-redefs [sut/max-cache-bytes 256 sut/max-cache-entry-bytes 32 sut/max-cache-entries 1]
       (let [calls (atom 0)
             oversized #(do (swap! calls inc) {:value (apply str (repeat 100 "x"))})]
         (dotimes [_ 2] (sut/cached-result catalog ["oversized"] oversized))
         (is (= 2 @calls))
         (is (zero? (:n (first (sqlite/query (:db catalog) "SELECT COUNT(*) n FROM cache")))))
         (sut/cached-result catalog ["a"] (constantly {:value "a"}))
         (sut/cached-result catalog ["b"] (constantly {:value "b"}))
         (is (= 1 (:n (first (sqlite/query (:db catalog) "SELECT COUNT(*) n FROM cache")))))
         (is (= 1 (count (fs/list-dir (fs/path (:state-dir catalog) "cache"))))))))))

(deftest glob-semantics
  (doseq [[pattern path expected] [["**/*.clj" "core.clj" true]
                                   ["**/*.clj" "src/deep/core.clj" true]
                                   ["src/*.clj" "src/deep/core.clj" false]
                                   ["src/?.clj" "src/é.clj" true]
                                   ["?.txt" "😀.txt" true]
                                   ["[ab].txt" "a.txt" false]
                                   ["[ab].txt" "[ab].txt" true]
                                   ["*.clj" "core.CLJ" false]]]
    (is (= expected (sut/glob-matches? pattern path))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"whole path component"
                        (sut/glob-matches? "**.clj" "core.clj"))))

(deftest immutable-file-history-diff-and-warm-cache
  (git-fixture
   (fn [{:keys [catalog work mirror head]}]
     (sut/refresh-catalog! catalog {})
     (spit (str (fs/path work "src/core.clj")) "new\nline two\n")
     (fixture-git! work "add" ".")
     (fixture-git! work "commit" "-m" "second")
     (fixture-git! work "push" (str mirror) "main:main")
     (sut/refresh-catalog! catalog {})
     (let [args {:repository "example/repo" :path "src/core.clj" :revision head}
           read-old (sut/read-repository-file catalog args)
           read-new (sut/read-repository-file catalog (dissoc args :revision))]
       (is (= ["old" "line two"] (:lines read-old)))
       (is (= ["new" "line two"] (:lines read-new)))
       (is (not= (:revision read-old) (:revision read-new))))
     (is (= {:mode "120000" :lines ["/etc/passwd"]}
            (select-keys (sut/read-repository-file catalog {:repository "example/repo" :path "link"}) [:mode :lines])))
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"stale"
                           (sut/read-repository-file catalog {:repository "example/repo" :path "src/core.clj"
                                                              :revision (apply str (repeat 40 "a"))})))
     (let [calls (atom 0) real-git sut/run-git
           args {:repos ["example/repo"] :count 10}]
       (with-redefs [sut/run-git (fn [& argv] (swap! calls inc) (apply real-git argv))]
         (let [cold (sut/commit-search catalog args)
               count-cold @calls]
           (is (= 2 (count (:results cold))))
           (is (= cold (sut/commit-search catalog args)))
           (is (= count-cold @calls))
           (is (pos? count-cold)))))
     (let [marker (fs/path work "unsafe-command-ran")
           script (fs/path work "unsafe-driver.sh")
           command (str "sh " script)]
       (spit (str script) (str "touch '" marker "'\nprintf 'unsafe driver invoked\\n'\n"))
       (fixture-git! mirror "config" "diff.external" command)
       (fixture-git! mirror "config" "core.fsmonitor" command)
       (fixture-git! mirror "config" "log.showSignature" "true")
       (fixture-git! mirror "config" "gpg.program" command)
       ;; Positive control: the fixture really can execute the configured driver.
       (fixture-git! mirror "diff" head "HEAD")
       (is (fs/exists? marker))
       (fs/delete marker)
       (let [result (sut/compare-revisions catalog {:repo "example/repo" :base head :head "HEAD"})]
         (is (str/includes? (get-in result [:results 0 :patch]) "-old\n+new")))
       (sut/commit-search catalog {:repos ["example/repo"] :count 1})
       (is (not (fs/exists? marker)))))))

(deftest diff-endpoints-share-one-published-generation
  (doseq [[from to] [["HEAD" "HEAD"] ["main" "peer"]]]
    (git-fixture
     (fn [{:keys [catalog work mirror head]}]
       (fixture-git! mirror "branch" "peer" "main")
       (sut/refresh-catalog! catalog {})
       (spit (str (fs/path work "src/core.clj")) "new generation\n")
       (fixture-git! work "add" ".")
       (fixture-git! work "commit" "-m" "new generation")
       (fixture-git! work "push" (str mirror) "main:main")
       (fixture-git! mirror "branch" "-f" "peer" "main")
       (let [new-head (str/trim (fixture-git! work "rev-parse" "HEAD"))
             published? (atom false)
             resolve-original sut/resolve-revision]
         ;; Publish a real new generation after the first endpoint is resolved.
         (with-redefs [sut/resolve-revision
                       (fn [& args]
                         (let [result (apply resolve-original args)]
                           (when (compare-and-set! published? false true)
                             (sut/refresh-catalog! catalog {}))
                           result))]
           (let [result (sut/compare-revisions catalog {:repo "example/repo" :base from :head to})]
             (is (= {:base head :head head :results [] :published-head new-head}
                    (assoc (select-keys result [:base :head :results])
                           :published-head (:head (first (sqlite/query (:db catalog) "SELECT head FROM repositories")))))))))))))

(deftest cache-budget-and-failure-recovery
  (git-fixture
   (fn [{:keys [catalog]}]
     (with-redefs [sut/max-cache-bytes 24 sut/max-cache-entry-bytes 24]
       (let [computations (atom 0)
             compute #(do (swap! computations inc) {:value "123456"})
             call #(sut/cached-result catalog [%] compute)]
         (call 1)
         (call 1)
         (is (= 1 @computations))
         (call 2)
         (call 1)
         (is (= 3 @computations))
         (is (<= (:bytes (first (sqlite/query (:db catalog) "SELECT SUM(bytes) bytes FROM cache"))) 24))
         (is (<= (reduce + (map fs/size (fs/list-dir (fs/path (:state-dir catalog) "cache")))) 24))
         (is (thrown? clojure.lang.ExceptionInfo
                      (sut/cached-result catalog ["failed"] #(throw (ex-info "failed" {})))))
         (let [path (first (fs/list-dir (fs/path (:state-dir catalog) "cache")))]
           (spit (str path) "broken")
           (is (= {:value "123456"} (call 1)))
           (is (= 4 @computations))))))))

(deftest result-formats-budget-and-cite
  (let [value {:results [{:repo "r" :path "p" :lines [[1 "a"]]}] :empty [] :none nil :has-more false}]
    (is (= {:results [{:repo "r" :path "p" :lines [[1 "a"]]}]} (sut/compact value)))
    (is (= (sut/compact value) (edn/read-string (sut/encode-result (sut/compact value) "edn"))))
    (is (= {:hasMore true :totalLines 3} (json/parse-string (sut/encode-result {:has-more true :total-lines 3} "json") true))))
  (let [rows (mapv (fn [i] {:path (str "f" i) :lines [[i (apply str (repeat 100 "x"))]]}) (range 50))
        text (sut/fit-budget {:results rows} "edn" 1000)
        value (edn/read-string text)]
    (is (<= (count text) 1000))
    (is (= [true sut/budget-hint] [(:has-more value) (:hint value)]))
    (is (< 0 (count (:results value)) 50)))
  (let [row {:path "p" :cite "r:p@abc:1-40" :lines (mapv (fn [i] [i (apply str (repeat 80 "y"))]) (range 1 41))}
        value (edn/read-string (sut/fit-budget {:results [row]} "edn" 1500))
        kept (get-in value [:results 0 :lines])]
    (is (< 0 (count kept) 40))
    (is (= (str "r:p@abc:1-" (first (peek kept))) (get-in value [:results 0 :cite]))))
  (let [value (edn/read-string (sut/fit-budget {:start 1 :end 100 :total-lines 300
                                                :lines (vec (repeat 100 (apply str (repeat 50 "z"))))} "edn" 2000))]
    (is (< (:end value) 100))
    (is (= (count (:lines value)) (:end value)))
    (is (str/includes? (:hint value) (str "startLine=" (inc (:end value))))))
  (is (= {:repository "github.com/a/b" :path "dir/x@y.go" :revision "main" :start-line 3 :end-line 9}
         (sut/parse-cite "github.com/a/b:dir/x@y.go@main:3-9")))
  (is (= {:repository "a/b" :path "p" :revision "abc"} (sut/parse-cite "a/b:p@abc")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid cite" (sut/parse-cite "nonsense")))
  (doseq [[pattern tool args] [[#"Unknown argument: bogusArg" "grep" {:query "x" :bogus-arg 1}]
                               [#"Missing required argument: query" "list_repos" {}]
                               [#"Invalid argument: format" "grep" {:query "x" :format "xml"}]
                               [#"either cite" "read_file" {:cite "a:b@c" :path "b"}]
                               [#"repository and path, or cite" "read_file" {:path "b"}]]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo pattern (sut/validate-arguments tool args)))))

(deftest lean-git-tools-and-cite-round-trip
  (git-fixture
   (fn [{:keys [catalog head]}]
     (sut/refresh-catalog! catalog {"example/repo" {"branches" [{"name" "HEAD" "version" "0000"}]
                                                    "fileCount" 5 "lastCommit" "2026-11-03T09:41:16Z"}})
     (is (= {:total 1 :results [{:id "example/repo" :last-commit "2026-11-03" :files 5
                                 :warnings ["Index revision differs from mirror HEAD"]}]}
            (sut/compact (sut/execute-tool nil catalog "list_repos" {:query "EXAMPLE"}))))
     (is (= 1 (:total (sut/execute-tool nil catalog "list_repos" {:query "*" :problems true}))))
     (is (zero? (:total (sut/execute-tool nil catalog "list_repos" {:query "absent"}))))
     (let [files #(set (:entries (sut/execute-tool nil catalog "list_files" (merge {:repository "example/repo"} %))))]
       (is (= #{"line\nbreak.txt" "link" "space name.txt" "src/"} (files {})))
       (is (= #{"line\nbreak.txt" "link" "space name.txt" "src/" "src/core.clj"} (files {:depth 2})))
       (is (= #{"src/"} (files {:kind "dirs" :depth 3})))
       (is (= #{"core.clj"} (files {:path "src"}))))
     (let [read (sut/execute-tool nil catalog "read_file" {:cite (str "example/repo:src/core.clj@" head ":2-2")})]
       (is (= {:rev head :start 2 :end 2 :lines ["line two"]} (select-keys read [:rev :start :end :lines]))))
     (let [read (sut/compact (sut/execute-tool nil catalog "read_file" {:repository "example/repo" :path "src/core.clj" :end-line 1}))]
       (is (= [true "Lines shown end at 1 of 2; continue with startLine=2."] [(:has-more read) (:hint read)]))))))

(deftest refs-history-and-compare
  (git-fixture
   (fn [{:keys [catalog work mirror head]}]
     (fixture-git! work "tag" "v1.0.0")
     (spit (str (fs/path work "src/core.clj")) "new\nline two\n")
     (spit (str (fs/path work "other.txt")) "added\n")
     (fixture-git! work "add" ".")
     (fixture-git! work "commit" "-m" "Fix parser bug")
     (fixture-git! work "tag" "-a" "v1.1.0" "-m" "release")
     (fixture-git! work "push" "--tags" (str mirror) "main:main")
     (fixture-git! mirror "update-ref" "refs/pull/1/head" head)
     (sut/refresh-catalog! catalog {})
     (let [new-head (str/trim (fixture-git! work "rev-parse" "HEAD"))
           refs #(->> (sut/execute-tool nil catalog "list_refs" (merge {:repository "example/repo"} %))
                      :results (map (juxt :name :type :rev)) set)
           names #(set (map first (refs %)))
           search #(sut/commit-search catalog (merge {:repos ["example/repo"]} %))]
       (is (= #{["main" "branch" new-head] ["v1.0.0" "tag" head] ["v1.1.0" "tag" new-head]} (refs {})))
       (is (= #{"v1.0.0" "v1.1.0"} (names {:tags-only true})))
       (is (= #{"main"} (names {:heads-only true})))
       (is (= #{"main" "v1.0.0" "v1.1.0"} (names {:contains-commit head})))
       (is (= #{"main" "v1.1.0"} (names {:contains-commit "v1.1.0"})))
       (is (= ["old" "line two"] (:lines (sut/read-repository-file catalog {:repository "example/repo" :revision "v1.0.0" :path "src/core.clj"}))))
       (is (= ["Fix parser bug"] (map :subject (:results (search {:message-terms ["parser" "FIX"]})))))
       (is (= 2 (count (:results (search {:authors ["FIXTURE"]})))))
       (is (= [new-head] (map :rev (:results (search {:content-terms ["added"]})))))
       (is (= [new-head] (map :rev (:results (search {:files ["other.txt"]})))))
       (is (= [head] (map :rev (:results (search {:revisions ["v1.0.0"]})))))
       (is (empty? (:results (search {:after "2100-01-01"}))))
       (is (= 2 (count (:results (search {:after "3 days ago"})))))
       (is (= 2 (count (:results (search {:after "2001-01-01" :before "2100-01-01T00:00:00+01:00"})))))
       (is (= ["example/repo: Invalid date: 2026-13-45"] (:warnings (search {:after "2026-13-45"}))))
       (is (= {:has-more true :count 1} (let [r (search {:count 1})] {:has-more (:has-more r) :count (count (:results r))})))
       (is (seq (:warnings (search {:after "x;rm -rf"}))))
       (is (= ["missing/repo: Unknown repository"] (:warnings (sut/commit-search catalog {:repos ["missing/repo"]}))))
       (let [page (sut/compare-revisions catalog {:repo "example/repo" :base head :head "main" :first 1})
             rest-page (sut/compare-revisions catalog {:repo "example/repo" :base head :head "main" :first 1 :after (:next page)})]
         (is (= {:files-changed 2 :next "1" :has-more true
                 :results [{:path "other.txt" :status "A" :added 1 :deleted 0 :patch "@@ -0,0 +1 @@\n+added"}]}
                (select-keys page [:files-changed :next :has-more :results])))
         (is (= {:offset 1 :has-more false :paths ["src/core.clj"]}
                {:offset (:offset rest-page) :has-more (:has-more rest-page) :paths (map :path (:results rest-page))})))))))

(deftest all-tools-over-real-http
  (fixture
   (fn [{:keys [backend]}]
     (git-fixture
      (fn [{:keys [catalog root head]}]
        (let [port (free-port)
              origin (str "http://127.0.0.1:" port)
              stop (sut/start! {:port port :origin origin :neogrok-url (str backend "/search")
                                :mirror-root root :state-dir (:state-dir catalog)})]
          (try
            (binding [*endpoint* "/mcp/all"]
             (let [headers (session-headers (initialize origin))]
              (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers)
              (is (= (set (map :name sut/tool-specs))
                     (set (map :name (get-in (call-http origin :post {:jsonrpc "2.0" :id "list" :method "tools/list"} headers)
                                             [:message :result :tools])))))
              (doseq [[tool args key expected]
                      [["grep" {:query "main"} :results :present]
                       ["list_repos" {:query ""} :results :present]
                       ["list_repos" {:query "EXAMPLE/Repo"} :results :present]
                       ["glob" {:repository "example/repo" :pattern "**/*.clj"} :results ["src/core.clj"]]
                       ["read_file" {:repository "example/repo" :path "src/core.clj"} :lines ["old" "line two"]]
                       ["list_files" {:repository "example/repo" :path "src"} :entries ["core.clj"]]
                       ["list_refs" {:repository "example/repo"} :results :present]
                       ["commit_search" {:repos ["example/repo"]} :results :present]
                       ["compare_revisions" {:repo "example/repo" :base head :head "HEAD"} :base head]
                       ["find_symbol_definitions" {:repository "example/repo" :symbol "main"} :kind "ctags-definitions"]
                       ["find_symbol_references" {:repository "example/repo" :symbol "main"} :kind "textual-occurrences-not-semantic"]]
                      fmt ["json" "edn"]]
                (let [response (call-http origin :post {:jsonrpc "2.0" :id (str tool fmt) :method "tools/call"
                                                        :params {:name tool :arguments (assoc args :format fmt)}} headers)
                      text (get-in response [:message :result :content 0 :text])
                      result (when text (if (= fmt "json") (json/parse-string text true) (edn/read-string text)))]
                  (is (= {:status 200 :is-error false :value (if (= expected :present) true expected)}
                         {:status (:status response)
                          :is-error (boolean (get-in response [:message :result :isError]))
                          :value (if (= expected :present) (boolean (seq (get result key))) (get result key))})
                      [tool fmt text])))
              (doseq [args [{:repository "example/repo" :path "../outside"}
                            {:repository "example/repo" :path "src/core.clj" :endLine 501}]]
                (is (= -32602 (get-in (call-http origin :post {:jsonrpc "2.0" :id (str (random-uuid)) :method "tools/call"
                                                               :params {:name "read_file" :arguments args}} headers)
                                      [:message :error :code]))))))
            (finally (stop)))))))))

(deftest endpoints-and-disabled-tools
  (fixture
   (fn [{:keys [backend]}]
     (git-fixture
      (fn [{:keys [catalog root]}]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown disabled tool: nope"
                              (sut/start! {:port (free-port) :origin "http://127.0.0.1:1" :neogrok-url backend
                                           :disabled-tools #{"nope"}})))
        (let [port (free-port)
              origin (str "http://127.0.0.1:" port)
              stop (sut/start! {:port port :origin origin :neogrok-url (str backend "/search")
                                :mirror-root root :state-dir (:state-dir catalog)
                                :disabled-tools #{"glob" "commit_search"}})
              listed (fn [endpoint]
                       (binding [*endpoint* endpoint]
                         (let [headers (session-headers (initialize origin))]
                           (call-http origin :post {:jsonrpc "2.0" :method "notifications/initialized"} headers)
                           [headers (set (map :name (get-in (call-http origin :post {:jsonrpc "2.0" :id 2 :method "tools/list"} headers)
                                                            [:message :result :tools])))])))]
          (try
            (let [[core-headers core] (listed "/mcp")
                  [_ all] (listed "/mcp/all")]
              (is (= (disj sut/core-tools "commit_search") core))
              (is (= (disj (set (map :name sut/tool-specs)) "glob" "commit_search") all))
              (binding [*endpoint* "/mcp/all"]
                (is (= 404 (:status (call-http origin :post {:jsonrpc "2.0" :id 3 :method "tools/list"} core-headers))))
                (is (= 404 (:status (call-http origin :delete nil core-headers)))))
              (binding [*endpoint* "/mcp/other"]
                (is (= 404 (:status (call-http origin :post {:jsonrpc "2.0" :id 3 :method "tools/list"} core-headers))))))
            (finally (stop)))))))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests 'code-search.mcp-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))