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
   [java.time Instant]))

(def ^:dynamic *store* nil)
(def ^:dynamic *base* nil)

(use-fixtures :each
  (fn [f]
    (let [dir (fs/create-temp-dir {:prefix "ts-broker-ui-test"})
          fake (fake-connect/start!)
          st (store/start! (str (fs/path dir "broker.db")) {:audit-out (fn [_])})
          server (http/start! (merge (lib/routes 'ts-broker.pages 'ts-broker.actions)
                                     {:port        0
                                      :ctx         {:store   st
                                                    :clock   #(Instant/now)
                                                    :connect (connect/client (:url fake) fake-connect/token)}
                                      :css         pages/css
                                      :datastar-js "export {}"}))]
      (try
        (binding [*store* st *base* (str "http://127.0.0.1:" (http/port server))]
          (f))
        (finally
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
  (is (= [403 403 403 200]
         (mapv :status [(request :get "/")
                        (json-post actions/approval approve :headers {"Tailscale-User-Login" nil})
                        (json-post actions/approval approve :headers {"Tailscale-User-Login" " "})
                        (request :get "/" :headers login)]))))

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
                    (stream-post "/" datastar)])))))

(deftest creates-a-credential-from-the-catalog-by-field-id
  (is (str/includes? (render "/catalog?vault=vlt0000000000000000000000a&item=itm0000000000000000000000a")
                     "value=\"credential\""))
  (is (str/includes? (:body (form-post actions/create-credential
                                       {:vault  "vlt0000000000000000000000a"
                                        :item   "itm0000000000000000000000a"
                                        :field  "f2"
                                        :name   "GITHUB_NOTE"
                                        :header "Authorization"
                                        :hosts  "api.github.com\n*.githubusercontent.com"}))
                     "window.location.href =\"/credential?name=GITHUB_NOTE&created=true\"")
      "a successful create moves the browser to the new credential")
  (is (= {:name "GITHUB_NOTE" :field "f2" :field-label "note" :hosts ["api.github.com" "*.githubusercontent.com"]
          :editor "casey@github"}
         (select-keys (store/read *store* #(store/credential % "GITHUB_NOTE"))
                      [:name :field :field-label :hosts :editor])))
  (is (str/includes? (render "/credential?name=GITHUB_NOTE&created=true") "Created."))
  (testing "a refused create shows why next to the form, which morphing skips"
    (is (re-find #"data-ignore-morph[^>]*>.*id=\"create-feedback\""
                 (render "/catalog?vault=vlt0000000000000000000000a&item=itm0000000000000000000000a")))
    (let [body (:body (form-post actions/create-credential
                                 {:vault "vlt0000000000000000000000a" :item "itm0000000000000000000000a"
                                  :field "f2" :name "GITHUB_NOTE" :header "Host" :hosts "x.com"}))]
      (is (re-find #"data: mode replace\ndata: elements <div class=\"feedback\" id=\"create-feedback\" role=\"alert\"><div class=\"message\"><p>Host is a reserved header"
                   body)))))

(defn- seed-credential! []
  (store/write! *store* {:command :credential/create :actor "casey@github"
                         :credential {:name "GITHUB_TOKEN" :vault "v" :item "i" :field "f" :field-label "credential"
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
    (is (str/includes? (render "/") "{payload: {&quot;id&quot;:1,&quot;event&quot;:&quot;approve&quot;,&quot;grant&quot;:&quot;once&quot;}}"))
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
  (form-post actions/assign {:credential "GITHUB_TOKEN" :subjects "tag:a,tag:b\n group:agents \n" :subject "tag:t3-bob"
                               :mode "inject" :approval "ask"})
  (is (= ["group:agents" "tag:a,tag:b" "tag:t3-bob"]
         (map :subject (store/read *store* store/assignments)))))

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
