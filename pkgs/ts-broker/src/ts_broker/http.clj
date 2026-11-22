(ns ts-broker.http
  "Datastar HTTP server for the broker UI.

  Pages and actions come from [[ts-broker.lib/routes]]. GET on a page path
  serves a shell page; its `data-init` opens one long-lived SSE stream by
  POST on the same path. The stream renders the page when it opens and
  again after each committed store write. Pages read their parameters from
  the query string. An action is a POST that runs a command and answers
  with the `#flash` element, empty on success. When the action returns a
  map with `:replace`, that element also replaces the element with its id,
  which morphing would skip. With `:redirect`, the browser then loads that
  path. Actions read their parameters from the body
  only, a form or a JSON object.

  Every request needs the `Tailscale-User-Login` header, which tailscale
  serve sets. A POST also needs `Datastar-Request: true` and
  `Sec-Fetch-Site: same-origin`. Without them the answer is 403. The login
  is the request's `:actor`. Malformed URL encoding is a 400. No response
  may be framed, errors included."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [hiccup2.core :as h]
   [org.httpkit.server :as server]
   [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
   [starfederation.datastar.clojure.api :as d*]
   [ts-broker.store :as store])
  (:import
   [java.io ByteArrayOutputStream]
   [java.net URLDecoder]
   [java.nio ByteBuffer]
   [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]
   [java.security MessageDigest]
   [java.util Base64]
   [java.util.zip GZIPOutputStream]))

(defn- digest [^String s]
  (-> (doto (MessageDigest/getInstance "SHA-256") (.update (.getBytes s "UTF-8")))
      (.digest)
      (->> (.encodeToString (.withoutPadding (Base64/getUrlEncoder))))
      (subs 0 16)))

(defn- gzip [^String s]
  (let [out (ByteArrayOutputStream.)]
    (with-open [z (GZIPOutputStream. out)]
      (.write z (.getBytes s "UTF-8")))
    (.toByteArray out)))

(defn- accepts-gzip? [request]
  (boolean (some->> (get-in request [:headers "accept-encoding"])
                    (re-find #"(?i)\bgzip\b"))))

(defn- asset [body content-type]
  {:path      (str "/" (digest body))
   :body      body
   :gzip-body (gzip body)
   :headers   {"Cache-Control" "max-age=31536000, immutable"
               "Content-Type"  content-type
               "Vary"          "Accept-Encoding"}})

(defn- asset-response [{:keys [body gzip-body headers]} request]
  (if (accepts-gzip? request)
    {:status 200 :headers (assoc headers "Content-Encoding" "gzip") :body gzip-body}
    {:status 200 :headers headers :body body}))

(defn- decode
  "Decodes the URL-encoded `s` as strict UTF-8, throwing a bad request for a
  broken escape or an invalid byte sequence instead of substituting U+FFFD."
  [s]
  (try
    (let [latin1 (URLDecoder/decode (str s) "ISO-8859-1")
          bytes  (.getBytes latin1 StandardCharsets/ISO_8859_1)]
      (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                      (.onMalformedInput CodingErrorAction/REPORT)
                      (.onUnmappableCharacter CodingErrorAction/REPORT))
                    (ByteBuffer/wrap bytes))))
    (catch IllegalArgumentException e
      (throw (ex-info "Malformed URL encoding." {:type ::bad-request} e)))
    (catch CharacterCodingException e
      (throw (ex-info "Malformed URL encoding." {:type ::bad-request} e)))))

(defn parse-params
  "Returns the parameters of the URL-encoded `s` as a map of keyword to a
  vector of values."
  [s]
  (reduce (fn [m pair]
            (let [[k v] (str/split pair #"=" 2)]
              (if (str/blank? k)
                m
                (update m (keyword (decode k)) (fnil conj []) (decode (or v ""))))))
          {}
          (str/split (str s) #"&")))

(defn- json-params [s]
  (let [m (try (json/parse-string s) (catch Exception _ nil))]
    (if (map? m)
      (into {}
            (keep (fn [[k v]]
                    (when (some? v)
                      [(keyword k) (if (sequential? v) (mapv str v) [(str v)])])))
            m)
      {})))

(defn- body-params
  "Returns the parameters of the form or JSON object body of `request` as a
  map of keyword to a vector of values."
  [request]
  (let [content-type (str (get-in request [:headers "content-type"]))
        body (some-> (:body request) slurp)]
    (cond
      (str/starts-with? content-type "application/x-www-form-urlencoded") (parse-params body)
      (str/starts-with? content-type "application/json") (json-params body)
      :else {})))

(def ^:private security-headers
  {"Content-Security-Policy" "frame-ancestors 'none'"
   "X-Frame-Options"         "DENY"})

(defn- shell-page [{:keys [css js]} title]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "UTF-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title]
          [:link {:rel "stylesheet" :href (:path css)}]
          [:script {:type "module" :src (:path js)}]]
         [:body
          [:div {:data-init "@post(window.location.pathname + window.location.search, {retryMaxCount: Infinity})"}]
          [:div#flash]
          [:main#morph [:p "Loading"]]
          [:noscript "This page needs JavaScript."]]])))

(defn- html-response [request body]
  (let [gzip? (accepts-gzip? request)]
    {:status  200
     :headers (cond-> {"Content-Type"  "text/html; charset=utf-8"
                       "Cache-Control" "no-store"
                       "Vary"          "Accept-Encoding"}
                gzip? (assoc "Content-Encoding" "gzip"))
     :body    (if gzip? (gzip body) body)}))

(defn- sse-options [request opts]
  (cond-> (assoc opts :headers (assoc security-headers "Cache-Control" "no-store" "Vary" "Accept-Encoding"))
    (accepts-gzip? request) (assoc hk-gen/write-profile hk-gen/gzip-profile)))

(defn- render-stream [{:keys [renders ctx]} request page]
  (hk-gen/->sse-response
   request
   (sse-options request
                {hk-gen/on-open  (fn [sse]
                                   (let [render #(d*/patch-elements! sse (str (h/html (page ctx request))))]
                                     (swap! renders assoc sse render)
                                     (render)))
                 hk-gen/on-close (fn [sse _status]
                                   (swap! renders dissoc sse))})))

(defn- flash [messages]
  [:div#flash {:role "status"}
   (when (seq messages)
     [:div.message (for [m messages] [:p m])])])

(defn- action-response [{:keys [ctx]} request action]
  (let [[elements {:keys [replace redirect]}]
        (try
          [(flash nil) (action ctx request)]
          (catch Exception e
            [(flash (or (store/invalid-errors e)
                        (do (binding [*out* *err*]
                              (println "action failed:" (ex-message e)))
                            ["The action failed. See the broker log."])))]))]
    (hk-gen/->sse-response
     request
     (sse-options request
                  {hk-gen/on-open (fn [sse]
                                    (d*/patch-elements! sse (str (h/html elements)))
                                    (when replace
                                      (d*/patch-elements! sse (str (h/html replace))
                                                          {d*/patch-mode d*/pm-replace}))
                                    (when redirect
                                      (d*/redirect! sse redirect))
                                    (d*/close-sse! sse))}))))

(defn- text-response [status message]
  {:status status :headers {"Content-Type" "text/plain; charset=utf-8"} :body message})

(defn- error-response [e]
  (if (= ::bad-request (:type (ex-data e)))
    (text-response 400 (str "Bad request: " (ex-message e) "\n"))
    (do (binding [*out* *err*]
          (println "request failed:" (ex-message e)))
        (text-response 500 "The request failed. See the broker log.\n"))))

(defn- route [{:keys [pages actions assets] :as app} {:keys [request-method uri] :as request}]
  (let [page (get pages uri)]
    (case request-method
      :get  (cond
              page (html-response request (shell-page assets (:title page)))
              (= uri (get-in assets [:css :path])) (asset-response (:css assets) request)
              (= uri (get-in assets [:js :path])) (asset-response (:js assets) request)
              :else {:status 404})
      :post (cond
              page (render-stream app (assoc request :params (parse-params (:query-string request))) (:fn page))
              (get actions uri) (action-response app (assoc request :params (body-params request)) (get actions uri))
              :else {:status 404})
      {:status 405})))

(defn handler
  "Returns the Ring handler of `app`. See the namespace docstring for the
  rules every request must meet."
  [app]
  (fn [request]
    (let [actor (some-> (get-in request [:headers "tailscale-user-login"]) str/trim not-empty)
          headers (:headers request)
          response (cond
                     (nil? actor)
                     (text-response 403 "Forbidden: the Tailscale-User-Login header is missing.\n")

                     (and (not= :get (:request-method request))
                          (not (and (= "true" (get headers "datastar-request"))
                                    (= "same-origin" (get headers "sec-fetch-site")))))
                     (text-response 403 "Forbidden: not a same-origin Datastar request.\n")

                     :else
                     (try
                       (route app (assoc request :actor actor))
                       (catch Exception e
                         (error-response e))))]
      (update response :headers merge security-headers))))

(defn- refresh-fn
  "Returns a function that re-renders every open stream. Renders run one at
  a time on an agent, and calls that arrive while one is queued share it."
  [renders]
  (let [renderer (agent nil)
        pending (atom false)]
    (fn []
      (when (compare-and-set! pending false true)
        (send-off renderer
                  (fn [_]
                    (reset! pending false)
                    (doseq [render (vals @renders)]
                      (try
                        (render)
                        (catch Exception e
                          (binding [*out* *err*]
                            (println "render failed:" (ex-message e))))))))))))

(defn start!
  "Starts the UI server on 127.0.0.1 and returns it for [[stop!]].

  Options:

  | key            | description |
  |----------------|-------------|
  | `:port`        | TCP port; 0 picks a free one |
  | `:ctx`         | App context passed to pages and actions; must hold `:store` |
  | `:pages`       | Pages from [[ts-broker.lib/routes]] |
  | `:actions`     | Actions from [[ts-broker.lib/routes]] |
  | `:css`         | Stylesheet text |
  | `:datastar-js` | Datastar bundle text |"
  [{:keys [port ctx pages actions css datastar-js]}]
  (let [renders (atom {})
        refresh (refresh-fn renders)
        app {:ctx     ctx
             :pages   pages
             :actions actions
             :renders renders
             :assets  {:css (asset css "text/css; charset=utf-8")
                       :js  (asset datastar-js "text/javascript; charset=utf-8")}}
        stop-server (server/run-server (handler app) {:ip "127.0.0.1" :port port :legacy-return-value? false})]
    (store/listen! (:store ctx) ::ui (fn [_events] (refresh)))
    {:server stop-server
     :ctx    ctx}))

(defn port
  "Returns the TCP port of the UI `server`."
  [{:keys [server]}]
  (server/server-port server))

(defn stop!
  "Stops the UI server."
  [{:keys [server ctx]}]
  (store/unlisten! (:store ctx) ::ui)
  @(server/server-stop! server {:timeout 1000})
  nil)
