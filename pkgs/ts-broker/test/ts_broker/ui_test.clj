(ns ts-broker.ui-test
  (:require
   [babashka.fs :as fs]
   [babashka.http-client :as http-client]
   [clojure.string :as str]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [ts-broker.actions :as actions]
   [ts-broker.connect :as connect]
   [ts-broker.domain :as domain]
   [ts-broker.fake-connect :as fake-connect]
   [ts-broker.http :as http]
   [ts-broker.lib :as lib]
   [ts-broker.pages :as pages]
   [ts-broker.store :as store])
  (:import
   [java.net InetSocketAddress StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio ByteBuffer]
   [java.nio.channels ServerSocketChannel SocketChannel]
   [java.time Instant]))

(def ^:dynamic *store* nil)
(def ^:dynamic *base* nil)
(def ^:dynamic *socket* nil)

(defn- pump
  "Copies `from` to `to` until `from` ends, then half-closes `to`."
  [^SocketChannel from ^SocketChannel to]
  (future
    (let [buf (ByteBuffer/allocate 8192)]
      (try
        (loop []
          (.clear buf)
          (when (<= 0 (.read from buf))
            (.flip buf)
            (while (.hasRemaining buf)
              (.write to buf))
            (recur)))
        (.shutdownOutput to)
        (catch Exception _
          (.close from)
          (.close to))))))

(defn- bridge
  "Forwards connections on a free loopback port to the unix socket at
  `path`, as ts-plug forwards tailnet connections, so the JDK HTTP client
  can reach the UI. Returns `{:port p :stop f}`."
  [path]
  (let [listener (doto (ServerSocketChannel/open)
                   (.bind (InetSocketAddress. "127.0.0.1" 0)))
        upstream (UnixDomainSocketAddress/of ^String path)]
    (future
      (loop []
        (when-let [tcp (try (.accept listener) (catch Exception _ nil))]
          (let [uds (SocketChannel/open StandardProtocolFamily/UNIX)]
            (.connect uds upstream)
            (let [up (pump tcp uds)
                  down (pump uds tcp)]
              (future @up @down (.close tcp) (.close uds))))
          (recur))))
    {:port (.getPort ^InetSocketAddress (.getLocalAddress listener))
     :stop #(.close listener)}))

(use-fixtures :each
  (fn [f]
    (let [dir (fs/create-temp-dir {:prefix "ts-broker-ui-test"})
          socket (str (fs/path dir "ui.sock"))
          fake (fake-connect/start!)
          st (store/start! (str (fs/path dir "broker.db")) {:audit-out (fn [_])})
          server (http/start! (merge (lib/routes 'ts-broker.pages 'ts-broker.actions)
                                     {:socket      socket
                                      :ctx         {:store   st
                                                    :clock   #(Instant/now)
                                                    :connect (connect/client (:url fake) fake-connect/token)}
                                      :css         pages/css
                                      :datastar-js "export {}"}))
          tcp (bridge socket)]
      (try
        (binding [*store* st *socket* socket *base* (str "http://127.0.0.1:" (:port tcp))]
          (f))
        (finally
          ((:stop tcp))
          (http/stop! server)
          ((:stop fake))
          (store/stop! st)
          (fs/delete-tree dir))))))

(def login {"Tailscale-User-Login" "casey@github"})

(def datastar
  "Headers a same-origin Datastar request from the browser carries."
  (assoc login "Datastar-Request" "true" "Sec-Fetch-Site" "same-origin"))

(def client
  "An HTTP/1.1 client. The default client's h2c upgrade stalls on SSE
  responses from http-kit."
  (http-client/client {:version :http1.1}))

(defn- request [method path & {:keys [headers body]}]
  (http-client/request {:client  client
                        :method  method
                        :uri     (str *base* path)
                        :headers (into {} (remove (comp nil? val)) headers)
                        :body    body
                        :throw   false
                        :timeout 5000}))

(defn- form-post [path form]
  (request :post path
           :headers (assoc datastar "Content-Type" "application/x-www-form-urlencoded")
           :body (str/join "&" (for [[k v] form] (str (name k) "=" (java.net.URLEncoder/encode v "UTF-8"))))))

(defn- json-post [path payload & {:keys [headers]}]
  (request :post path
           :headers (merge datastar {"Content-Type" "application/json"} headers)
           :body (json/generate-string payload)))

(defn- stream-post
  "POSTs to the page `path` and returns the response with its body closed."
  [path headers]
  (let [response (http-client/request {:client client :method :post :uri (str *base* path) :headers headers
                                       :body "{}" :as :stream :throw false :timeout 5000})]
    (.close ^java.io.InputStream (:body response))
    response))

(defn- render [path]
  (let [{:keys [body]} (http-client/request {:client client :method :post :uri (str *base* path)
                                             :headers datastar
                                             :body "{}" :as :stream :timeout 5000})]
    (with-open [r (java.io.BufferedReader. (java.io.InputStreamReader. body))]
      (loop [lines []]
        (let [line (.readLine r)]
          (if (or (nil? line) (and (str/blank? line) (seq lines)))
            (str/join "\n" lines)
            (recur (conj lines line))))))))

(def approve {:id 1 :event "approve" :grant "once"})

(deftest requests-need-the-tailscale-login
  (is (= [403 403 403 403 200]
         (mapv :status [(request :get "/")
                        (json-post actions/approval approve :headers {"Tailscale-User-Login" nil})
                        (json-post actions/approval approve :headers {"Tailscale-User-Login" " "})
                        (request :get "/" :headers {"Tailscale-User-Login" "" "Tailscale-Node-Tags" "tag:t3-alice"})
                        (request :get "/" :headers login)]))))

(deftest only-the-owner-may-open-the-ui-socket
  (is (= "rw-------" (fs/posix->str (fs/posix-file-permissions *socket*)))))

(deftest posts-need-a-same-origin-datastar-request
  (is (= [403 403 403 403 403]
         (mapv :status [(json-post actions/delete-credential {:name "X"} :headers {"Sec-Fetch-Site" "cross-site"})
                        (json-post actions/delete-credential {:name "X"} :headers {"Sec-Fetch-Site" "same-site"})
                        (json-post actions/delete-credential {:name "X"} :headers {"Sec-Fetch-Site" nil})
                        (json-post actions/delete-credential {:name "X"} :headers {"Datastar-Request" nil})
                        (stream-post "/" (dissoc datastar "Sec-Fetch-Site"))])))
  (is (= 200 (:status (json-post actions/delete-credential {:name "X"})))))

(deftest no-response-may-be-framed
  (is (= #{{"content-security-policy" "frame-ancestors 'none'" "x-frame-options" "DENY"}}
         (set (map #(select-keys (:headers %) ["content-security-policy" "x-frame-options"])
                   [(request :get "/" :headers login)
                    (request :get "/")
                    (request :get "/nowhere" :headers login)
                    (json-post actions/delete-credential {:name "X"})
                    (stream-post "/" datastar)
                    (request :post actions/assign
                             :headers (assoc datastar "Content-Type" "application/x-www-form-urlencoded")
                             :body "credential=%E0%A4%A")])))))

(deftest malformed-form-encoding-is-a-bad-request
  (is (= 400 (:status (request :post actions/assign
                               :headers (assoc datastar "Content-Type" "application/x-www-form-urlencoded")
                               :body "credential=%E0%A4%A"))))
  (is (= 400 (:status (request :post actions/assign
                               :headers (assoc datastar "Content-Type" "application/x-www-form-urlencoded")
                               :body "credential=%FF")))
      "invalid UTF-8 is refused, not replaced")
  (is (= 400 (:status (request :post actions/assign
                               :headers (assoc datastar "Content-Type" "application/x-www-form-urlencoded")
                               :body (java.io.ByteArrayInputStream. (byte-array (map unchecked-byte (concat (.getBytes "credential=X&junk=") [0xff])))))))
      "raw invalid UTF-8 bytes in the body are refused too"))

(deftest actions-never-retry
  (is (every? #(str/includes? % "retryMaxCount: 0")
              [(lib/post actions/assign {}) (lib/form-post actions/create-credential)])
      "a retried action could send values the human edited after submitting"))

(deftest creates-a-credential-from-the-catalog-by-field-id
  (is (str/includes? (render "/catalog?vault=vlt0000000000000000000000a&item=itm0000000000000000000000a")
                     "value=\"credential\""))
  (let [body (:body (form-post actions/create-credential
                               {:vault  "vlt0000000000000000000000a"
                                :item   "itm0000000000000000000000a"
                                :field  "f2"
                                :name   "GITHUB_NOTE"
                                :header "Authorization"
                                :hosts  "api.github.com\n*.githubusercontent.com"}))
        [_ name] (re-find #"window\.location\.href =\"/credential\?name=(BRK_[A-Z2-7]{8})&created=true\"" body)]
    (is (some? name) "a successful create moves the browser to the new credential, named by the broker")
    (is (= {:name        name
            :field       "f2"
            :vault-title "Agents"
            :item-title  "GitHub token"
            :field-label "note"
            :hosts       ["api.github.com" "*.githubusercontent.com"]
            :editor      "casey@github"}
           (select-keys (store/read *store* #(store/credential % name))
                        [:name :field :vault-title :item-title :field-label :hosts :editor])))
    (let [page (render (str "/credential?name=" name "&created=true"))]
      (is (str/includes? page "<h1>Agents / GitHub token / note</h1>"))
      (is (str/includes? page "Created."))
      (is (str/includes? page (str "<code>GITHUB_TOKEN=TS_EGRESS_PLACEHOLDER_" name "</code>")))))
  (testing "a refused create shows why next to the form, which morphing skips"
    (is (re-find #"data-ignore-morph[^>]*>.*id=\"create-feedback\""
                 (render "/catalog?vault=vlt0000000000000000000000a&item=itm0000000000000000000000a")))
    (let [body (:body (form-post actions/create-credential
                                 {:vault "vlt0000000000000000000000a" :item "itm0000000000000000000000a"
                                  :field "f2" :name "GITHUB_NOTE" :header "Host" :hosts "x.com"}))]
      (is (re-find #"data: mode replace\ndata: elements <div class=\"feedback\" [^>]*id=\"create-feedback\"[^>]*><div class=\"message error\"><p>Host is a reserved header"
                   body)))))

(deftest the-create-button-is-disabled-while-its-request-is-in-flight
  (let [page (render "/catalog?vault=vlt0000000000000000000000a&item=itm0000000000000000000000a")
        [_ signal] (re-find #"<form [^>]*data-indicator=\"(\w+)\"[^>]*data-on:submit__prevent=\"@post\(&apos;/action/create-credential&apos;" page)]
    (is (some? signal) "the create form sets a signal while its request is in flight")
    (is (str/includes? page (str "<button data-attr:disabled=\"$" signal "\" type=\"submit\">Create credential</button>")))))

(defn- seed-credential! []
  (store/write! *store* {:command :credential/create :actor "casey@github"
                         :credential {:name "GITHUB_TOKEN" :vault "v" :item "i" :field "f"
                                      :vault-title "Agents" :item-title "GitHub token" :field-label "credential"
                                      :header "Authorization" :hosts ["api.github.com"]}}))

(deftest approves-through-the-ui-and-records-the-approver
  (seed-credential!)
  (form-post actions/assign {:credential "GITHUB_TOKEN" :subjects "group:agents" :mode "inject" :approval "ask"})
  (let [decide #(store/write! *store* {:command :decide
                                       :request (domain/parse-request
                                                 {:op "decide" :identity "tag:t3-alice" :node "t3-alice"
                                                  :groups ["agents"] :modes ["inject"] :credential "GITHUB_TOKEN"
                                                  :host "api.github.com" :header "Authorization" :mode "inject"})})]
    (is (= {:decision "pending" :approval "1"} (decide)))
    (is (str/includes? (render "/") "{payload: {&quot;id&quot;:1,&quot;event&quot;:&quot;approve&quot;,&quot;grant&quot;:&quot;once&quot;}, retryMaxCount: 0})"))
    (testing "an action ignores parameters in the query string"
      (is (str/includes? (:body (request :post (str actions/approval "?id=1&event=approve&grant=24h") :headers datastar))
                         "No such approval or event."))
      (is (= :pending (:state (store/read *store* #(store/approval % 1))))))
    (is (= 200 (:status (json-post actions/approval {:id 1 :event "approve" :grant "24h"}))))
    (is (= "allow" (:decision (decide))))
    (is (= {:state :approved :approver "casey@github"}
           (select-keys (store/read *store* #(store/approval % 1)) [:state :approver])))
    (is (str/includes? (render "/identity?id=tag%3At3-alice") "h left"))))

(deftest approvals-show-the-requested-host-and-the-current-credential
  (seed-credential!)
  (form-post actions/assign {:credential "GITHUB_TOKEN" :subjects "group:agents" :mode "inject" :approval "ask"})
  (store/write! *store* {:command :decide
                         :request (domain/parse-request
                                   {:op "decide" :identity "tag:t3-alice" :node "t3-alice"
                                    :groups ["agents"] :modes ["inject"] :credential "GITHUB_TOKEN"
                                    :host "api.github.com" :header "Authorization" :mode "inject"})})
  (let [page (render "/")]
    (is (re-find #"<td><code>api\.github\.com</code></td><td><code>Authorization</code> <code class=\"pill\">api\.github\.com</code></td>"
                 page)))
  (let [revision (:revision (store/read *store* #(store/credential % "GITHUB_TOKEN")))]
    (form-post actions/update-credential {:name "GITHUB_TOKEN" :revision revision :header "X-Api-Key"
                                            :hosts "*.github.com"}))
  (is (str/includes? (:body (json-post actions/approval {:id 1 :event "approve" :grant "once"}))
                     "Approval 1 was superseded by a change to its credential."))
  (is (not (str/includes? (render "/") "api.github.com")) "a superseded approval leaves the queue"))

(deftest assigns-one-manual-subject-per-line
  (seed-credential!)
  (let [body (:body (form-post actions/assign {:credential "GITHUB_TOKEN" :subjects "tag:a,tag:b\n group:agents \n"
                                               :subject "tag:t3-bob" :mode "inject" :approval "ask"}))]
    (is (re-find #"data: mode replace\ndata: elements <form [^>]*id=\"assign-form\".*<p>Saved 3 assignments\.</p>" body)
        "a successful assign answers with a fresh form that says what it saved"))
  (is (= ["group:agents" "tag:a,tag:b" "tag:t3-bob"]
         (map :subject (store/read *store* store/assignments))))
  (testing "a refused assign shows why next to the form and keeps the ticked boxes"
    (is (re-find #"data: mode replace\ndata: elements <div class=\"feedback\" [^>]*id=\"assign-feedback\".*Select at least one credential\."
                 (:body (form-post actions/assign {:subject "tag:t3-bob" :mode "inject" :approval "ask"}))))))

(deftest the-assign-button-submits-all-three-groups
  (let [page (render "/assign")
        form (re-find #"(?s)<form [^>]*id=\"assign-form\".*</form>" page)]
    (is (re-find #"</fieldset></div><div class=\"form-footer\"><button type=\"submit\">Assign</button>" form))
    (is (= 3 (count (re-seq #"<fieldset>" form))))))

(defn- options
  "Returns the checkboxes of the assign form in `html`, in page order, as
  maps of their value, id and label id."
  [html]
  (for [[_ label-id input] (re-seq #"<label class=\"option\"(?: id=\"([^\"]+)\")?><(input [^>]*type=\"checkbox\"[^>]*)>" html)
        :let [attr #(second (re-find (re-pattern (str " " % "=\"([^\"]+)\"")) input))]]
    {:value (attr "value") :id (attr "id") :label-id label-id}))

(defn- ids [html]
  (into {} (map (juxt :value (juxt :id :label-id))) (options html)))

(deftest assign-checkboxes-keep-their-ids-when-a-re-render-reorders-them
  (doseq [[name title] [["BRK_AAAAAAAA" "Apple"] ["BRK_BBBBBBBB" "Berry"]]]
    (store/write! *store* {:command :credential/create :actor "casey@github"
                           :credential {:name name :vault "v" :item name :field "f"
                                        :vault-title "Agents" :item-title title :field-label "credential"
                                        :header "Authorization" :hosts ["api.example.com"]}}))
  (form-post actions/assign {:credential "BRK_AAAAAAAA" :subjects "tag:t3-bob" :mode "inject" :approval "ask"})
  (let [before (render "/assign")]
    (store/write! *store* {:command :credential/titles
                           :titles  [{:name "BRK_AAAAAAAA" :vault-title "Agents" :item-title "Zebra"
                                      :field-label "credential"}]})
    (form-post actions/assign {:credential "BRK_AAAAAAAA" :subjects "tag:t3-alice" :mode "inject" :approval "ask"})
    (let [after (render "/assign")]
      (is (= [["BRK_AAAAAAAA" "BRK_BBBBBBBB" "tag:t3-bob"]
              ["BRK_BBBBBBBB" "BRK_AAAAAAAA" "tag:t3-alice" "tag:t3-bob"]]
             (map #(map :value (options %)) [before after]))
          "the rename and the new subject move the boxes")
      (is (= (ids before) (select-keys (ids after) (keys (ids before))))
          "each box and its label keep their ids, so morphing keeps a tick on its own box")
      (is (apply distinct? (mapcat val (ids after)))))))

(defn- open-stream
  "Opens the page stream of `path` and returns a function that returns its
  next event, and closes it on `:close`."
  [path]
  (let [{:keys [body]} (http-client/request {:client client :method :post :uri (str *base* path)
                                             :headers datastar :body "{}" :as :stream :timeout 5000})
        r (java.io.BufferedReader. (java.io.InputStreamReader. body))]
    (fn
      ([] (loop [lines []]
            (let [line (.readLine r)]
              (if (or (nil? line) (and (str/blank? line) (seq lines)))
                (str/join "\n" lines)
                (recur (cond-> lines (not (str/blank? line)) (conj line)))))))
      ([_] (.close r)))))

(defn- form-tag [html]
  (re-find #"<form [^>]*id=\"credential-form\"[^>]*>" html))

(defn- form-revision [html]
  (second (re-find #"name=\"revision\"[^>]*value=\"([^\"]+)\"" html)))

(deftest the-edit-form-keeps-its-draft-and-revision-until-saved
  (seed-credential!)
  (let [next-event (open-stream "/credential?name=GITHUB_TOKEN")
        shown (next-event)
        revision (form-revision shown)]
    (try
      (form-post actions/update-credential {:name "GITHUB_TOKEN" :revision revision :header "X-Other-Admin"
                                              :hosts "api.github.com"})
      (let [rerender (next-event)]
        (is (= [true true]
               (mapv #(str/includes? (str (form-tag %)) "data-ignore-morph") [shown rerender]))
            "the live re-render leaves the form the user is editing as it is")
        (is (not= revision (form-revision rerender))))
      (is (str/includes? (:body (form-post actions/update-credential
                                           {:name "GITHUB_TOKEN" :revision revision :header "X-Api-Key"
                                            :hosts "api.github.com"}))
                         "changed or was deleted since the form was shown")
          "the user's save from the form they saw is refused")
      (is (= "X-Other-Admin" (:header (store/read *store* #(store/credential % "GITHUB_TOKEN")))))
      (finally
        (next-event :close))))
  (testing "a successful save replaces the form with one at the new revision"
    (let [{:keys [revision]} (store/read *store* #(store/credential % "GITHUB_TOKEN"))
          body (:body (form-post actions/update-credential {:name "GITHUB_TOKEN" :revision revision
                                                              :header "X-Api-Key" :hosts "api.github.com"}))
          saved (:revision (store/read *store* #(store/credential % "GITHUB_TOKEN")))]
      (is (= [true saved]
             [(str/includes? body (str "data: mode replace\ndata: elements " (form-tag body)))
              (form-revision body)])))))

(deftest deleting-an-assigned-credential-warns-that-its-placeholder-stops-working
  (seed-credential!)
  (let [warning #"<p class=\"warn-text\" id=\"assignment-warning\"> It has 1 assignment\. Deleting it removes them, and its placeholder stops working"]
    (is (nil? (re-find warning (render "/credential?name=GITHUB_TOKEN"))))
    (form-post actions/assign {:credential "GITHUB_TOKEN" :subjects "group:agents" :mode "inject" :approval "ask"})
    (let [page (render "/credential?name=GITHUB_TOKEN")]
      (is (re-find warning page))
      (is (str/includes? page "data-confirm=\"Delete Agents / GitHub token / credential?\"")))))

(deftest edits-the-credential-the-form-showed
  (seed-credential!)
  (let [page (render "/credential?name=GITHUB_TOKEN")
        revision (second (re-find #"name=\"revision\"[^>]*value=\"([^\"]+)\"" page))
        edit {:name "GITHUB_TOKEN" :revision (str revision) :header "X-Api-Key" :hosts "api.github.com"}]
    (is (some? revision))
    (is (not (str/includes? (:body (form-post actions/update-credential edit)) "class=\"message\"")))
    (is (= "X-Api-Key" (:header (store/read *store* #(store/credential % "GITHUB_TOKEN")))))
    (is (str/includes? (:body (form-post actions/update-credential (assoc edit :header "Authorization")))
                       "changed or was deleted since the form was shown"))))
