#!/usr/bin/env bb

(require '[babashka.deps :as deps])
(when-not (System/getenv "DEPENDENCY_MIRROR_PACKAGED")
  (deps/add-deps
   '{:deps
     {dev.data-star.clojure/sdk
      {:mvn/version "1.0.0-RC13"}

      dev.data-star.clojure/http-kit
      {:mvn/version "1.0.0-RC13"}}}))

(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[hiccup.core :as hp]
         '[org.httpkit.server :as srv]
         '[starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
         '[starfederation.datastar.clojure.api :as d*])

(import '[java.io ByteArrayOutputStream]
        '[java.security MessageDigest SecureRandom]
        '[java.util Base64 Base64$Encoder]
        '[java.util.zip GZIPOutputStream])

;; The Nix wrapper supplies the pinned asset so startup also works offline.
(when-let [asset (System/getenv "DEPENDENCY_MIRROR_DATASTAR")]
  (intern *ns* 'datastar-js-body (slurp asset)))

;;;  -- start framework --

(def datastar-url "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js")

(def ^Base64$Encoder base64-encoder
  (.withoutPadding (Base64/getUrlEncoder)))

(defn bytes->base64 [^bytes bytes]
  (.encodeToString base64-encoder bytes))

(defn digest
  "Returns a short URL-safe SHA-256 digest of `data`."
  [data]
  (let [bytes (if (bytes? data)
                data
                (.getBytes (str data)))]
    (-> (doto (MessageDigest/getInstance "SHA256")
          (.update bytes))
        (.digest)
        bytes->base64
        (subs 10))))

(def ^SecureRandom secure-random
  (SecureRandom.))

(defn random-unguessable-uid []
  (let [buffer (byte-array 20)]
    (.nextBytes secure-random buffer)
    (bytes->base64 buffer)))

(defn gzip [data]
  (let [bytes (if (bytes? data)
                data
                (.getBytes (str data) "UTF-8"))
        out (ByteArrayOutputStream.)]
    (with-open [gzip-stream (GZIPOutputStream. out)]
      (.write gzip-stream bytes))
    (.toByteArray out)))

(defn accepts-gzip? [request]
  (let [items (some-> (get-in request [:headers "accept-encoding"])
                      (str/split #","))
        coding #(-> % (str/split #";") first str/trim str/lower-case)
        enabled? #(not (some (fn [param]
                               (re-matches #"(?i)q\s*=\s*0(?:\.0*)?"
                                           (str/trim param)))
                             (rest (str/split % #";"))))]
    (if-let [item (first (filter #(= "gzip" (coding %)) items))]
      (enabled? item)
      (boolean (some #(and (= "*" (coding %)) (enabled? %)) items)))))

(defmacro defaction
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym args & body]
  (let [path (str "/" (digest (str *ns* "/" sym)))
        handler (symbol (str sym "-fn"))]
    `(do
       (defn ~handler ~args ~@body)
       (def ~sym ~path))))

(defmacro action-map [& actions]
  `(hash-map
    ~@(mapcat (fn [action]
                [action `(var ~(symbol (str action "-fn")))])
              actions)))

(defonce assets_ (atom {}))

(defn static-asset [{:keys [body content-type compress?]}]
  (let [path (str "/" (digest body))
        response {:status 200
                  :headers (cond-> {"Cache-Control" "max-age=31536000, immutable"
                                    "Content-Type" content-type}
                             compress? (assoc "Vary" "Accept-Encoding"))
                  :body body}]
    (swap! assets_ assoc path
           {:response response
            :gzip-body (when compress? (gzip body))})
    path))

(defn to-str [s]
  (cond
    (keyword? s) (name s)
    (vector? s) (->> (map to-str s)
                     (interpose " ")
                     (apply str))
    :else (str s)))

(defn format-rule [[selector declarations]]
  (str
   (to-str selector)
   (if (map? declarations)
     (str "{"
          (reduce (fn [css [property value]]
                    (str css (to-str property) ":" (to-str value) ";"))
                  ""
                  (sort-by (comp to-str key) declarations))
          "}")
     declarations)))

(defn flatten-seq [xs]
  (mapcat #(if (and (seq? %) (vector? (first %))) % [%]) xs))

(defn static-css [css-rules]
  (static-asset
   {:body (if (vector? css-rules)
            (->> (flatten-seq css-rules)
                 (map format-rule)
                 (reduce str ""))
            css-rules)
    :content-type "text/css"
    :compress? true}))

(defn static-asset-handler [{:keys [uri] :as request}]
  (if-let [{:keys [response gzip-body]} (get @assets_ uri)]
    (if (and gzip-body (accepts-gzip? request))
      (-> response
          (assoc :body gzip-body)
          (assoc-in [:headers "Content-Encoding"] "gzip"))
      response)
    {:status 404}))

(defonce datastar-js-body
  (:body (http/get datastar-url {:timeout 10000})))

(def datastar-js
  (static-asset
   {:body datastar-js-body
    :content-type "text/javascript; charset=utf-8"
    :compress? true}))

(defonce renders_ (atom {}))

(def on-load-js
  "@post(window.location.pathname + (window.location.search + '&u=').replace(/^&/,'?'), {retryMaxCount: Infinity, openWhenHidden: false, retry: 'error'})")

(def tabid-js
  "self.crypto.randomUUID().substring(0,8)")

(defn shim-page [head-elements]
  (str
   "<!doctype html>"
   (hp/html
    [:html {:lang "en"}
     (into
      [:head
       [:meta {:charset "UTF-8"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1.0"}]]
      (concat head-elements
              [[:script {:defer true
                         :type "module"
                         :src datastar-js}]]))
     [:body
      [:div {:data-signals:tabid tabid-js}]
      [:div {:data-init on-load-js
             :data-on:online__window on-load-js}]
      [:noscript "Your browser does not support JavaScript!"]
      [:main {:id "morph"}]]])))

(defn shim-page-handler [request head-elements]
  (let [body (shim-page head-elements)
        gzip? (accepts-gzip? request)
        etag (str (digest body) (when gzip? "-gzip"))
        headers {"Content-Type" "text/html; charset=utf-8"
                 "ETag" etag
                 "Vary" "Accept-Encoding"}]
    (if (= (get-in request [:headers "if-none-match"]) etag)
      {:status 304
       :headers (select-keys headers ["ETag" "Vary"])}
      {:status 200
       :headers (cond-> headers
                  gzip? (assoc "Content-Encoding" "gzip"))
       :body (if gzip? (gzip body) body)})))

(defn get-sid [request]
  (some->> (get-in request [:headers "cookie"])
           (re-find #"(?:^|; )__Host-sid=([^; ]+)")
           second))

(defn session-cookie [sid]
  (str "__Host-sid=" sid "; Path=/; Secure; HttpOnly; SameSite=Lax"))

(defn wrap-session [handler]
  (fn [request]
    (let [sid (get-sid request)
          get? (= (:request-method request) :get)
          same-origin? (= (get-in request [:headers "sec-fetch-site"])
                          "same-origin")]
      (cond
        (and sid (or get? same-origin?))
        (handler (assoc request :sid sid))

        get?
        (let [sid (random-unguessable-uid)]
          (-> (handler (assoc request :sid sid))
              (assoc-in [:headers "Set-Cookie"] [(session-cookie sid)])))

        :else
        {:status 403}))))

(defn request-signals [request]
  (let [body (d*/get-signals request)]
    (json/parse-string (if (string? body) body (slurp body)) true)))

(defn refresh-all! []
  (doseq [render (vals @renders_)]
    (render)))

(defn action-handler [request thunk]
  (if-some [elements (thunk request)]
    (hk-gen/->sse-response
     request
     (cond-> {:headers {"Cache-Control" "no-store"
                        "Vary" "Accept-Encoding"}
              hk-gen/on-open (fn [sse-gen]
                               (d*/patch-elements!
                                sse-gen
                                (hp/html elements)
                                {d*/selector "body"
                                 d*/patch-mode d*/pm-append})
                               (d*/close-sse! sse-gen))}
       (accepts-gzip? request) (assoc hk-gen/write-profile hk-gen/gzip-profile)))
    {:status  204
     :headers {"Cache-Control" "no-store"}}))

(defn render-page-handler [request page]
  (hk-gen/->sse-response
   request
   (cond-> {:headers {"Vary" "Accept-Encoding"}
            hk-gen/on-open (fn [sse-gen]
                             (let [render #(d*/patch-elements! sse-gen (hp/html (page request)))]
                               (swap! renders_ assoc sse-gen render)
                               (refresh-all!)))
            hk-gen/on-close (fn [sse-gen _status]
                              (swap! renders_ dissoc sse-gen)
                              (refresh-all!))}
     (accepts-gzip? request)
     (assoc hk-gen/write-profile hk-gen/gzip-profile))))

(defn routes [pages actions head-elements
              {:keys [request-method uri] :as request}]
  (case request-method
    :get
    (if (contains? pages uri)
      (shim-page-handler request head-elements)
      (static-asset-handler request))

    :post
    (if-let [page (get pages uri)]
      (render-page-handler request page)
      (if-let [thunk (get actions uri)]
        (action-handler request thunk)
        {:status 404}))

    {:status 404}))

(defn page-router [pages actions head-elements]
  (fn [request]
    (routes @pages @actions @head-elements request)))

(defn start-app
  [{:keys [port router ctx-start ctx-stop]
    :or {port 8080
         ctx-start (fn [] {})
         ctx-stop (fn [_ctx])}}]
  (let [ctx (ctx-start)
        wrap-ctx (fn [handler]
                   (fn [request]
                     (handler (merge request ctx))))
        wrapped-router (-> router wrap-ctx wrap-session)
        stop-server (srv/run-server wrapped-router {:port port})]
    {:wrapped-router wrapped-router
     :ctx ctx
     :stop (fn stop [& [opts]]
             (stop-server opts)
             (ctx-stop ctx))}))

;;;  -- end framework --
;;;  -- begin app --

(require '[babashka.fs :as fs]
         '[babashka.process :as process]
         '[clojure.edn :as edn]
         '[clojure.pprint :as pprint]
         '[clojure.test :as test])

(import '[java.io RandomAccessFile]
        '[java.net URI URLDecoder URLEncoder])

(defn escape-html [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn require! [condition message]
  (when-not condition (throw (ex-info message {}))))

(def providers
  {:github {:label "GitHub" :issues? true :wiki? true :contributed? true :gists? true :exclude-forks? true}
   :gitlab {:label "GitLab" :issues? true :wiki? true :exclude-forks? true}
   :gitea {:label "Gitea / Forgejo" :issues? true :wiki? true :exclude-forks? true}
   :gogs {:label "Gogs" :issues? true :wiki? true :exclude-forks? true}
   :bitbucket {:label "Bitbucket"}
   :onedev {:label "OneDev" :issues? true :exclude-forks? true}
   :sourcehut {:label "SourceHut" :wiki? true}
   :opengist {:label "Opengist"}
   :any {:label "Plain Git"}})

(def host-providers
  {"github.com" :github "gitlab.com" :gitlab "codeberg.org" :gitea
   "gitea.com" :gitea "bitbucket.org" :bitbucket "git.sr.ht" :sourcehut})

(defn repository [entry]
  (require! (map? entry) "Each repository entry must be a map.")
  (require! (every? #{:url :provider :scope :organization? :include :issues? :wiki? :contributed? :gists? :exclude-forks?} (keys entry))
            "Unknown repository option.")
  (let [raw (:url entry)]
    (require! (string? raw) "URL must be a string.")
    (let [uri (URI. (str/trim raw))
          host (some-> (.getHost uri) str/lower-case)
          provider (or (:provider entry) (get host-providers host) :any)
          segments (str/split (str/replace (.getPath uri) #"^/|/+$" "") #"/")
          segments (update segments (dec (count segments)) #(str/replace % #"\.git$" ""))
          segments (if (= provider :github) (mapv str/lower-case segments) segments)
          owner? (= :owner (or (:scope entry)
                               (if (and (not= :any provider) (= 1 (count segments))) :owner :repo)))
          include (:include entry)]
      (require! (contains? providers provider) "Unknown provider.")
      (require! (or (nil? (:scope entry)) (#{:repo :owner} (:scope entry))) "Invalid selection scope.")
      (require! (and (= "https" (.getScheme uri)) host
                     (nil? (.getUserInfo uri)) (nil? (.getQuery uri))
                     (nil? (.getFragment uri)) (= -1 (.getPort uri)))
                "Use an HTTPS clone URL without credentials, query, fragment, or port.")
      (require! (and (seq segments)
                     (every? #(and (re-matches #"[A-Za-z0-9_.~-]+" %)
                                   (not (#{"" "." ".."} %))
                                   (not (str/starts-with? % "-"))) segments))
                "Invalid repository path.")
      (require! (or (not= :github provider) (<= 1 (count segments) 2))
                "GitHub URLs must identify an owner or an owner/repository.")
      (require! (or (not= :any provider) (not owner?))
                "Plain Git URLs identify individual repositories, not owners.")
      (require! (or owner? (= provider :any) (>= (count segments) 2) (= provider :onedev))
                "A forge repository URL needs an owner and repository name.")
      (doseq [key [:issues? :wiki? :organization? :contributed? :gists? :exclude-forks?]]
        (require! (or (not (contains? entry key)) (boolean? (get entry key)))
                  "Backup options must be booleans."))
      (doseq [key [:issues? :wiki? :contributed? :gists?]]
        (require! (or (not (get entry key)) (get-in providers [provider key]))
                  (str (name provider) " does not support " (name key) ".")))
      (when include
        (require! (and owner? (not= provider :any) (vector? include) (seq include)
                       (every? #(and (string? %) (re-matches #"[A-Za-z0-9_.-]+" %)
                                     (not (#{"." ".."} %))) include))
                  "Include must be a nonempty vector of repository names on an owner entry."))
      (require! (or (not (:organization? entry)) (#{:github :gitlab :gitea :gogs :bitbucket} provider))
                "Organization selection is not supported by this provider.")
      (let [url (str "https://" host "/" (str/join "/" segments)
                     (when (and (= provider :any)
                                (str/ends-with? (str/replace (.getPath uri) #"/+$" "") ".git"))
                       ".git"))
            owner (cond owner? (str/join "/" segments)
                        (= 1 (count segments)) "git"
                        :else (str/join "/" (butlast segments)))
            name (last segments)]
        {:entry (cond-> {:url url}
                  (:provider entry) (assoc :provider provider)
                  (:scope entry) (assoc :scope (:scope entry))
                  (:organization? entry) (assoc :organization? true)
                  include (assoc :include (vec (distinct (if (= provider :github) (map str/lower-case include) include))))
                  (:issues? entry) (assoc :issues? true)
                  (:wiki? entry) (assoc :wiki? true)
                  (:contributed? entry) (assoc :contributed? true)
                  (:gists? entry) (assoc :gists? true)
                  (contains? entry :exclude-forks?) (assoc :exclude-forks? (:exclude-forks? entry)))
         :id (digest url)
         :provider provider
         :origin (str "https://" host)
         :owner owner
         :name name
         :owner? owner?
         :path (str host "/" owner (when-not owner? (str "/" name ".git")))}))))

(defn inventory [document]
  (require! (and (map? document) (= #{:repos} (set (keys document)))
                 (vector? (:repos document)))
            "Inventory must contain only a :repos vector.")
  (let [repos (mapv repository (:repos document))]
    (require! (= (count repos) (count (set (map :id repos))))
              "Duplicate repository or owner URL.")
    (require! (= (count repos) (count (set (map :path repos))))
              "Repository storage paths collide.")
    {:repos (mapv :entry repos)}))

(defn load-inventory [dir]
  (let [file (fs/path dir "repos.edn")]
    (if (fs/exists? file)
      (inventory (edn/read-string (slurp (str file))))
      {:repos []})))

(defn with-file-lock [dir name f]
  (fs/create-dirs dir)
  (with-open [file (RandomAccessFile. (str (fs/path dir name)) "rw")
              channel (.getChannel file)]
    (let [lock (try (.tryLock channel)
                    (catch Exception _ nil))]
      (require! lock "Another operation is running; try again when it finishes.")
      (with-open [_held-lock lock]
        (f)))))

(defn save-inventory! [dir document]
  (let [document (inventory document)
        file (fs/create-temp-file {:dir dir :prefix ".repos-" :suffix ".edn"})]
    (try
      (spit (str file) (with-out-str (pprint/pprint document)))
      (fs/move file (fs/path dir "repos.edn") {:atomic-move true :replace-existing true})
      document
      (finally (fs/delete-if-exists file)))))

(def extra-options
  [[:issues? "issues" "Issues"] [:wiki? "wiki" "Wiki"]
   [:contributed? "contributed" "Contributed"] [:exclude-forks? "excludeforks" "Exclude forks"]
   [:gists? "gists" "Gists"]])

(defn form-extras [entry params]
  (let [provider (:provider (repository entry))]
    (reduce (fn [entry [key field]]
              (if (get-in providers [provider key])
                (assoc entry key (= "on" (get params field)))
                entry))
            entry extra-options)))

(defn update-repository! [dir id params]
  (with-file-lock dir "inventory.lock"
    #(let [document (load-inventory dir)]
       (require! (some (fn [entry] (= id (:id (repository entry)))) (:repos document))
                 "Unknown repository selection.")
       (save-inventory! dir
                        (update document :repos
                                (fn [entries]
                                  (mapv (fn [entry]
                                          (if (= id (:id (repository entry)))
                                            (form-extras entry params)
                                            entry))
                                        entries)))))))

(defn add-repository! [dir entry]
  (with-file-lock dir "inventory.lock"
    #(save-inventory! dir (update (load-inventory dir) :repos conj (:entry (repository entry))))))

(defn delete-repository! [dir id]
  (with-file-lock dir "inventory.lock"
    #(let [document (load-inventory dir)
           remaining (filterv (fn [entry] (not= id (:id (repository entry)))) (:repos document))]
       (require! (< (count remaining) (count (:repos document))) "Unknown repository selection.")
       (save-inventory! dir (assoc document :repos remaining)))))

(defn gickup-source [entry]
  (let [{:keys [provider origin owner name owner?]} (repository entry)
        token-file (System/getenv (str (str/upper-case (clojure.core/name provider)) "_TOKEN_FILE"))]
    [provider
     (if (= :any provider)
       {:url (:url entry) :user owner}
       (cond-> {:user owner
                :wiki (boolean (:wiki? entry))
                :issues (boolean (:issues? entry))}
         (not= :github provider) (assoc :url origin)
         (and (= :github provider) (not= origin "https://github.com")) (assoc :url origin)
         (not owner?) (assoc :include [name])
         (:include entry) (assoc :include (:include entry))
         (get-in providers [provider :exclude-forks?]) (assoc :filter {:excludeforks (get entry :exclude-forks? true)})
         (= provider :github) (assoc :contributed (boolean (:contributed? entry)) :gists (boolean (:gists? entry)))
         (= provider :bitbucket) (assoc :organization owner)
         ;; GitLab's group enumeration uses the token owner's account plus includeorgs.
         (and (= provider :gitlab) (:organization? entry)) (dissoc :user)
         (and (= provider :gitlab) (:organization? entry)) (assoc :includeorgs [owner])
         (= provider :sourcehut) (assoc :user (str/replace owner #"^~" ""))
         (not (str/blank? token-file)) (assoc :token_file token-file)))]))

(defn gickup-config [dir entries]
  {:source (reduce (fn [sources entry]
                     (let [[provider source] (gickup-source entry)]
                       (update sources provider (fnil conj []) source)))
                   {} entries)
   :destination {:local [{:path (str (fs/path dir "repos"))
                          :structured true :bare true :mirror true
                          :zip false :keep 0 :lfs false}]}})

(defn sync-repositories! [dir id]
  (with-file-lock dir "sync.lock"
    (fn []
      (let [entries (:repos (load-inventory dir))
            selected (if id (filterv #(= id (:id (repository %))) entries) entries)]
        (when id (require! (seq selected) "Unknown repository selection."))
        (when (seq selected)
          (fs/create-dirs (fs/path dir "repos"))
          (fs/with-temp-dir [tmp {:dir dir :prefix ".gickup-"}]
            (let [config (str (fs/path tmp "config.json"))]
              ;; JSON is valid YAML; do not interpolate inventory text into YAML or shell code.
              (spit config (json/generate-string (gickup-config dir selected)))
              (process/shell {:extra-env {"GIT_TERMINAL_PROMPT" "0"}}
                             (or (System/getenv "GICKUP_BIN") "gickup") config))))))))

(defn start-sync! [{:keys [state-dir busy_]} id]
  (require! (compare-and-set! busy_ false true) "A sync is already running.")
  (future
    (try
      (sync-repositories! state-dir id)
      (catch Exception e
        (binding [*out* *err*] (println "Mirror sync failed:" (ex-message e))))
      (finally (reset! busy_ false)))))

(def css
  (static-css
   "body{font:14px/1.4 system-ui;max-width:1440px;margin:1rem auto;padding:0 .8rem;background:#f8fafc;color:#172033}
    h1{font-size:1.5rem;margin:0 0 .2rem}p{margin:.35rem 0 .6rem}a{color:#174ca0}
    table{width:100%;border-collapse:collapse;margin:.6rem 0}th,td{text-align:left;padding:3px 6px;border-bottom:1px solid #ccd4df;vertical-align:middle}tbody tr{height:32px}
    th{font-size:.8rem;color:#586579}.upstream{max-width:0;width:100%}.upstream>div{display:flex;align-items:center;gap:.5rem}.upstream a,.upstream small{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.upstream a{min-width:0}.upstream small{max-width:110px;flex-shrink:0;font-size:10px}
    input[type=url]{box-sizing:border-box;width:min(65%,850px);padding:.35rem .45rem;margin:.25rem .6rem .35rem 0}
    button,select{font:inherit;padding:.25rem .5rem}button{cursor:pointer}button:disabled{cursor:default}
    fieldset{border:1px solid #ccd4df;border-radius:4px;margin:.8rem 0;padding:.55rem .7rem}
    label[for=url]{display:block}.toolbar,.actions{display:flex;flex-wrap:wrap;gap:.4rem;align-items:center}
    .message{padding:.5rem .7rem;background:#fff3ce}.muted,small{color:#586579}
    .options{display:flex;flex-wrap:wrap;gap:.3rem .8rem;margin:.3rem 0}.options label{white-space:nowrap}
    .extras form,.extras .options{display:flex;align-items:center;flex-wrap:nowrap;gap:.4rem;margin:0}.extras{font-size:11px;white-space:nowrap}.extras input{width:12px;height:12px;margin:0;vertical-align:middle}td button{font-size:12px;padding:2px 6px;line-height:20px}
    form{margin:0}.actions{flex-wrap:nowrap;white-space:nowrap;font-size:12px}table{min-width:860px}code{overflow-wrap:anywhere}
    @media(max-width:800px){body{padding:0 .5rem}input[type=url]{width:100%}table{display:block;overflow-x:auto;min-width:0}.upstream{min-width:220px}}"))

(defn csrf-field [context]
  [:input {:type "hidden" :name "csrf" :value (:csrf context)}])

(defn encode [s] (URLEncoder/encode s "UTF-8"))

(defn page [{:keys [state-dir cgit-url busy_ base-path] :as context} message]
  (str "<!doctype html>"
       (hp/html
        [:html {:lang "en"}
         [:head [:meta {:charset "UTF-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title "Dependency mirrors"]
          [:link {:rel "stylesheet" :href (str base-path css)}]
          [:script {:type "module" :src (str base-path datastar-js)}]]
         [:body
          [:h1 "Dependency mirrors"]
          [:p.muted "Offline source repositories. Add a repository or a forge user/organization."]
          (when message [:p.message {:role "status"} (escape-html message)])
          [:div.toolbar
           [:form {:method "post" :action (str base-path "/sync")}
            (csrf-field context)
            [:button {:type "submit" :disabled @busy_} "Sync All"]]
           [:a {:href cgit-url} "Browse cgit"]
           [:a {:href (str base-path "/")} "Refresh"]
           (when @busy_ [:span "Sync running"])]
          [:fieldset
           [:legend "Add a repository or owner"]
           [:form {:method "post" :action (str base-path "/add")
                   :data-signals (json/generate-string {:url "" :providerOverride "auto"})
                   :data-computed:provider
                   (str "$providerOverride !== 'auto' ? $providerOverride : (() => {try {return ("
                        (json/generate-string host-providers)
                        ")[new URL($url).hostname.toLowerCase()] || 'any'} catch {return 'any'}})()")
                   :data-computed:owner
                   "$provider !== 'any' && (() => {try {return new URL($url).pathname.replace(/^\\/+|\\/+$/g,'').split('/').length === 1} catch {return false}})()"}
            (csrf-field context)
            [:label {:for "url"} "Repository or user/organization URL"]
            [:input {:id "url" :name "url" :type "url" :required true
                     :data-bind "url" :placeholder "https://github.com/cooperspencer/"}]
            [:label {:for "provider"} "Provider"]
            [:select {:id "provider" :name "provider" :data-bind "providerOverride"}
             [:option {:value "auto"} "Detect from URL"]
             (for [[key {:keys [label]}] (sort-by (comp :label val) providers)]
               [:option {:value (name key)} label])]
            [:p.muted {:data-text "$url ? ($owner ? 'All repositories for this owner' : 'Single repository') + ' · ' + $provider : 'Enter a URL; known forges are detected automatically.'"}
             "Enter a URL; known forges are detected automatically."]
            [:div.options
             (for [[key field label] extra-options
                   :let [supported (mapv (comp name first) (filter #(get-in % [1 key]) providers))
                         available (str (json/generate-string supported) ".includes($provider)")]]
               [:label {:data-show available}
                [:input {:name field :type "checkbox"
                         :checked (= key :exclude-forks?)
                         :data-attr:disabled (str "!(" available ")")}]
                (str " " label)])]
            [:p.muted "For self-hosted forges, select the provider. Credentials, if needed, come from the service—not this form."]
            [:button {:type "submit"} "Add"]]]
          [:table
           [:thead [:tr [:th "Upstream / selection"] [:th "Extras"] [:th "Actions"]]]
           [:tbody
            (for [entry (sort-by (comp str/lower-case :url) (:repos (load-inventory state-dir)))
                  :let [{:keys [id owner? path provider]} (repository entry)]]
              [:tr
               [:td.upstream
                [:div
                 [:a {:href (:url entry) :title (:url entry) :rel "noreferrer"} (escape-html (:url entry))]
                 (when owner?
                   [:small {:title (if (:include entry) (str/join ", " (:include entry)) "All repositories")}
                    (if (:include entry) (escape-html (str/join ", " (:include entry))) "All")])]]
               [:td.extras
                (if (some #(get-in providers [provider (first %)]) extra-options)
                  [:form {:method "post" :action (str base-path "/update")}
                   (csrf-field context)
                   [:input {:type "hidden" :name "id" :value id}]
                   [:div.options
                    (for [[key field label] extra-options
                          :when (get-in providers [provider key])]
                      [:label {:title label} [:input {:name field :type "checkbox"
                                                      :checked (get entry key (= key :exclude-forks?))}]
                       (str " " (case key :contributed? "Contrib." :exclude-forks? "No forks" label))])]
                   [:button {:type "submit"} "Save"]]
                  [:span.muted "Git only"])]
               [:td [:div.actions
                     [:a {:href (if owner? cgit-url (str cgit-url path "/"))} "cgit"]
                     [:form {:method "post" :action (str base-path "/sync")}
                      (csrf-field context)
                      [:input {:type "hidden" :name "id" :value id}]
                      [:button {:type "submit" :disabled @busy_} "Sync"]]
                     [:form {:method "post" :action (str base-path "/delete")
                             :data-on:submit "if (confirm('Delete this inventory entry? Mirrored files will be kept.')) el.submit()"}
                      (csrf-field context)
                      [:input {:type "hidden" :name "id" :value id}]
                      [:button {:type "submit"} "Delete"]]]]])]]
          [:p.muted "Git history is mirrored in full. LFS objects, release assets and implicit submodules are not downloaded. Removing an inventory entry does not delete its mirror."]]])))

(defn form-params [body]
  (into {}
        (for [pair (str/split (slurp body) #"&")
              :let [[key value] (str/split pair #"=" 2)]]
          [(URLDecoder/decode key "UTF-8") (URLDecoder/decode (or value "") "UTF-8")])))

(defn html-response [context message status]
  {:status status
   :headers {"Content-Type" "text/html; charset=utf-8"
             "Cache-Control" "no-store"
             "X-Content-Type-Options" "nosniff"
             "Content-Security-Policy" "default-src 'self'; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'"}
   :body (page context message)})

(defn app-handler [context request]
  (let [{:keys [request-method uri headers]} request]
    (try
      (require! (contains? (:hosts context) (get headers "host")) "Unexpected Host header.")
      (case [request-method uri]
        [:get "/health"] {:status 200 :body "ok"}
        [:get "/"] (html-response context nil 200)
        [:post "/add"]
        (let [params (form-params (:body request))]
          (require! (and (= (get params "csrf") (:csrf context))
                         (= (get headers "origin") (or (:public-origin context) (str "http://" (get headers "host"))))
                         (not= "cross-site" (get headers "sec-fetch-site")))
                    "Cross-origin request rejected.")
          (let [provider (get params "provider" "auto")
                entry (cond-> {:url (get params "url")}
                        (not= provider "auto") (assoc :provider (keyword provider)))
                entry (form-extras entry params)]
            (add-repository! (:state-dir context) entry))
          {:status 303 :headers {"Location" (str (:base-path context) "/")} :body ""})
        [:post "/update"]
        (let [params (form-params (:body request))]
          (require! (and (= (get params "csrf") (:csrf context))
                         (= (get headers "origin") (or (:public-origin context) (str "http://" (get headers "host"))))
                         (not= "cross-site" (get headers "sec-fetch-site")))
                    "Cross-origin request rejected.")
          (update-repository! (:state-dir context) (get params "id") params)
          {:status 303 :headers {"Location" (str (:base-path context) "/")} :body ""})
        [:post "/delete"]
        (let [params (form-params (:body request))]
          (require! (and (= (get params "csrf") (:csrf context))
                         (= (get headers "origin") (or (:public-origin context) (str "http://" (get headers "host"))))
                         (not= "cross-site" (get headers "sec-fetch-site")))
                    "Cross-origin request rejected.")
          (delete-repository! (:state-dir context) (get params "id"))
          {:status 303 :headers {"Location" (str (:base-path context) "/")} :body ""})
        [:post "/sync"]
        (let [params (form-params (:body request))
              id (not-empty (get params "id"))]
          (require! (and (= (get params "csrf") (:csrf context))
                         (= (get headers "origin") (or (:public-origin context) (str "http://" (get headers "host"))))
                         (not= "cross-site" (get headers "sec-fetch-site")))
                    "Cross-origin request rejected.")
          (when id
            (require! (some #(= id (:id (repository %))) (:repos (load-inventory (:state-dir context))))
                      "Unknown repository selection."))
          (start-sync! context id)
          {:status 303 :headers {"Location" (str (:base-path context) "/")} :body ""})
        (if (= request-method :get) (static-asset-handler request) {:status 405 :body "Method not allowed"}))
      (catch Exception e
        (let [fallback {:status 400
                        :headers {"Content-Type" "text/plain; charset=utf-8" "Cache-Control" "no-store"}
                        :body (ex-message e)}]
          (if (and (contains? (:hosts context) (get headers "host"))
                   (= :post request-method) (#{"/add" "/sync" "/delete" "/update"} uri))
            (try
              (html-response context (ex-message e) 400)
              ;; A malformed inventory can prevent the page itself from rendering.
              (catch Exception _ fallback))
            fallback))))))

(defn handler [context request]
  (let [base-path (or (:base-path context) "")
        uri (:uri request)]
    (cond
      (and (seq base-path) (= uri base-path))
      {:status 308 :headers {"Location" (str base-path "/")} :body ""}
      (str/starts-with? uri (str base-path "/"))
      (app-handler context (update request :uri subs (count base-path)))
      :else {:status 404 :body "Not found"})))

(defn start-server! [{:keys [state-dir bind port cgit-url base-path public-origin]}]
  (require! (or (nil? base-path) (= "" base-path) (re-matches #"(?:/[A-Za-z0-9_-]+)+" base-path))
            "Base path must contain path segments without a trailing slash.")
  (when public-origin
    (let [uri (URI. public-origin)]
      (require! (and (#{"http" "https"} (.getScheme uri)) (.getHost uri)
                     (str/blank? (.getPath uri)) (nil? (.getUserInfo uri))
                     (nil? (.getQuery uri)) (nil? (.getFragment uri)))
                "Public origin must be an HTTP(S) origin without a path.")))
  (fs/create-dirs state-dir)
  (with-file-lock state-dir "inventory.lock"
    #(when-not (fs/exists? (fs/path state-dir "repos.edn"))
       (save-inventory! state-dir {:repos []})))
  (load-inventory state-dir)
  (let [context {:base-path (or base-path "") :public-origin public-origin
                 :state-dir state-dir :cgit-url (str (str/replace cgit-url #"/+$" "") "/")
                 :busy_ (atom false) :csrf (random-unguessable-uid)
                 :hosts (cond-> #{(str bind ":" port)}
                          (= bind "127.0.0.1") (conj (str "localhost:" port))
                          public-origin (conj (.getAuthority (URI. public-origin))))}]
    {:context context
     :stop (srv/run-server (partial handler context)
                           {:ip bind :port port :max-body 16384
                            :legacy-unsafe-remote-addr? false})}))

(test/deftest repository-selections
  (test/is (= "https://git.example.com/project.git"
              (get-in (repository {:url "https://git.example.com/project.git"}) [:entry :url])))
  (test/is (= {:url "https://github.com/cooperspencer/gickup"}
              (:entry (repository {:url "https://GitHub.com/CooperSpencer/Gickup.git/"}))))
  (test/is (:owner? (repository {:url "https://github.com/cooperspencer/"})))
  (test/is (= ["gickup"] (get-in (repository {:url "https://github.com/cooperspencer/" :include ["Gickup"]})
                                 [:entry :include])))
  (doseq [url ["file:///etc" "http://example.com/repo" "https://user:pass@example.com/repo"
               "https://example.com/../repo" "https://github.com/a/b/tree/main" "https://example.com/a?b=c"]]
    (test/is (thrown? Exception (repository {:url url}))))
  (test/is (thrown? Exception (repository {:url "https://git.example.com/repo" :issues? true}))))

(test/deftest provider-options
  (doseq [[url provider] [["https://gitlab.com/team/repo" :gitlab]
                          ["https://codeberg.org/team/repo" :gitea]
                          ["https://bitbucket.org/team/repo" :bitbucket]
                          ["https://git.sr.ht/~team/repo" :sourcehut]]]
    (test/is (= provider (:provider (repository {:url url})))))
  (doseq [provider (keys providers)]
    (test/is (= provider (first (gickup-source {:url "https://forge.example/team/repo" :provider provider}))))))
(test/deftest provider-defaults
  (let [[_ github] (gickup-source {:url "https://github.com/team"})]
    (test/is (= {:excludeforks true} (:filter github)))
    (test/is (false? (:contributed github)))
    (test/is (false? (:gists github))))
  (let [[_ github] (gickup-source {:url "https://github.com/team" :exclude-forks? false :contributed? true :gists? true})]
    (test/is (= {:excludeforks false} (:filter github)))
    (test/is (:contributed github))
    (test/is (:gists github)))
  (test/is (thrown? Exception (repository {:url "https://gitlab.com/team" :contributed? true}))))

(test/deftest inventory-and-config
  (fs/with-temp-dir [dir]
    (add-repository! dir {:url "https://github.com/cooperspencer/"})
    (test/is (thrown? Exception (add-repository! dir {:url "https://github.com/cooperspencer"})))
    (test/is (= 1 (count (:repos (load-inventory dir)))))
    (let [config (gickup-config dir [{:url "https://github.com/cooperspencer"}
                                     {:url "https://github.com/tailscale/tailscale" :issues? true}
                                     {:url "https://git.zx2c4.com/cgit"}])]
      (test/is (nil? (get-in config [:source :github 0 :include])))
      (test/is (= ["tailscale"] (get-in config [:source :github 1 :include])))
      (test/is (true? (get-in config [:source :github 1 :issues])))
      (test/is (= "git" (get-in config [:source :any 0 :user])))
      (test/is (= 0 (get-in config [:destination :local 0 :keep]))))
    (with-file-lock dir "sync.lock"
      #(test/is (thrown? Exception (with-file-lock dir "sync.lock" (fn [] :unexpected)))))
    (test/is (= :released (with-file-lock dir "sync.lock" (fn [] :released))))))

(test/deftest local-gickup-mirror
  (when (fs/which "gickup")
    (fs/with-temp-dir [dir]
      (let [source (str (fs/path dir "fixture"))
            target (str (fs/path dir "mirror"))
            config-file (str (fs/path dir "gickup.json"))
            config (assoc-in (gickup-config target [{:url "https://example.test/vendor/fixture"}])
                             [:source :any 0 :url] source)
            run (fn [& args] (apply process/shell {:out :string :err :string} args))]
        (run "git" "init" "--initial-branch=main" source)
        (spit (str (fs/path source "README")) "first version\n")
        (run "git" "-C" source "add" "README")
        (run "git" "-C" source "-c" "user.name=Test" "-c" "user.email=test@example.test"
             "commit" "-m" "Initial fixture")
        (spit config-file (json/generate-string config))
        (run "gickup" config-file)
        (let [mirrors (filter #(fs/exists? (fs/path % "HEAD")) (fs/glob target "**/*.git"))
              mirror (str (first mirrors))]
          (test/is (= 1 (count mirrors)))
          (test/is (= "true\n" (:out (run "git" "-C" mirror "rev-parse" "--is-bare-repository"))))
          (test/is (= (:out (run "git" "-C" source "rev-parse" "HEAD"))
                      (:out (run "git" "--git-dir" mirror "rev-parse" "refs/heads/main"))))
          (spit (str (fs/path source "README")) "second version\n")
          (run "git" "-C" source "-c" "user.name=Test" "-c" "user.email=test@example.test"
               "commit" "-am" "Update fixture")
          (run "gickup" config-file)
          (test/is (= "second version\n" (:out (run "git" "--git-dir" mirror "show" "main:README")))))))))

(test/deftest update-existing-extras
  (fs/with-temp-dir [dir]
    (let [entry {:url "https://github.com/cooperspencer" :include ["gickup"] :wiki? true}
          other {:url "https://codeberg.org/example/repo"}
          context {:state-dir dir :csrf "test-csrf" :hosts #{"127.0.0.1:8084"}
                   :cgit-url "http://127.0.0.1:8085/" :busy_ (atom false)}
          request {:request-method :post :uri "/update"
                   :headers {"host" "127.0.0.1:8084" "origin" "http://127.0.0.1:8084"}}
          body (str "csrf=test-csrf&id=" (encode (:id (repository entry))) "&issues=on&gists=on")]
      (add-repository! dir entry)
      (add-repository! dir other)
      (test/is (= 400 (:status (handler context (assoc request :body (java.io.StringReader. "csrf=wrong"))))))
      (test/is (= {:repos [entry other]} (load-inventory dir)))
      (test/is (= 303 (:status (handler context (assoc request :body (java.io.StringReader. body))))))
      (test/is (= {:repos [{:url (:url entry) :include ["gickup"] :issues? true :gists? true :exclude-forks? false} other]}
                  (load-inventory dir)))
      (test/is (thrown? Exception (update-repository! dir "missing" {}))))))

(test/deftest delete-keeps-mirror-files
  (fs/with-temp-dir [dir]
    (let [entry {:url "https://github.com/cooperspencer"}
          other {:url "https://github.com/tailscale/tailscale"}
          sentinel (fs/path dir "repos" "keep-me")
          context {:state-dir dir :csrf "test-csrf" :hosts #{"127.0.0.1:8084"}
                   :cgit-url "http://127.0.0.1:8085/" :busy_ (atom false)}
          request {:request-method :post :uri "/delete"
                   :headers {"host" "127.0.0.1:8084" "origin" "http://127.0.0.1:8084"}}
          body (str "csrf=test-csrf&id=" (encode (:id (repository entry))))]
      (add-repository! dir entry)
      (add-repository! dir other)
      (fs/create-dirs (fs/parent sentinel))
      (spit (str sentinel) "keep")
      (test/is (= 400 (:status (handler context (assoc request :body (java.io.StringReader. "csrf=wrong"))))))
      (test/is (= {:repos [entry other]} (load-inventory dir)))
      (test/is (= {:status 303 :headers {"Location" "/"} :body ""}
                  (handler context (assoc request :body (java.io.StringReader. body)))))
      (test/is (= {:repos [other]} (load-inventory dir)))
      (test/is (= "keep" (slurp (str sentinel))))
      (test/is (thrown? Exception (delete-repository! dir (:id (repository entry))))))))

(test/deftest add-errors-preserve-page
  (fs/with-temp-dir [dir]
    (let [context {:state-dir dir :csrf "test-csrf" :hosts #{"127.0.0.1:8084"}
                   :cgit-url "http://127.0.0.1:8085/" :busy_ (atom false)}
          entry {:url "https://github.com/cooperspencer"}]
      (add-repository! dir entry)
      (doseq [[url message] [[(:url entry) "Duplicate repository or owner URL."]
                             ["http://example.com/repo" "Use an HTTPS clone URL"]]]
        (let [response (handler context
                                {:request-method :post :uri "/add"
                                 :headers {"host" "127.0.0.1:8084" "origin" "http://127.0.0.1:8084"}
                                 :body (java.io.StringReader. (str "csrf=test-csrf&url=" (encode url)))})]
          (test/is (= {:status 400 :content-type "text/html; charset=utf-8"
                       :message true :form true :list true}
                      {:status (:status response)
                       :content-type (get-in response [:headers "Content-Type"])
                       :message (str/includes? (:body response) message)
                       :form (str/includes? (:body response) "action=\"/add\"")
                       :list (str/includes? (:body response) "<tbody><tr>")}))))
      (test/is (= {:repos [entry]} (load-inventory dir))))))

(test/deftest inventory-display-order
  (fs/with-temp-dir [dir]
    (save-inventory! dir {:repos [{:url "https://github.com/zebra"} {:url "https://github.com/alpha"}]})
    (let [html (page {:state-dir dir :csrf "test" :cgit-url "/" :busy_ (atom false)} nil)]
      (test/is (< (str/index-of html "action=\"/add\"") (str/index-of html "<table")))
      (test/is (< (str/index-of html "https://github.com/alpha") (str/index-of html "https://github.com/zebra")))
      (test/is (= "https://github.com/zebra" (get-in (load-inventory dir) [:repos 0 :url]))))))

(test/deftest prefixed-editor
  (fs/with-temp-dir [dir]
    (let [context {:state-dir dir :csrf "test-csrf" :hosts #{"src.example.test"}
                   :base-path "/edit" :public-origin "https://src.example.test"
                   :cgit-url "https://src.example.test/" :busy_ (atom false)}
          request {:request-method :get :uri "/edit/" :headers {"host" "src.example.test"}}
          response (handler context request)]
      (test/is (str/includes? (:body response) "action=\"/edit/add\""))
      (test/is (str/includes? (:body response) (str "src=\"/edit" datastar-js "\"")))
      (test/is (= 200 (:status (handler context (assoc request :uri (str "/edit" datastar-js))))))
      (test/is (= {:status 308 :headers {"Location" "/edit/"} :body ""}
                  (handler context (assoc request :uri "/edit"))))
      (test/is (= 404 (:status (handler context (assoc request :uri "/")))))
      (doseq [[origin expected] [["http://src.example.test" 400] ["https://src.example.test" 303]]]
        (let [result (handler context
                              {:request-method :post :uri "/edit/add"
                               :headers {"host" "src.example.test" "origin" origin}
                               :body (java.io.StringReader. "csrf=test-csrf&url=https%3A%2F%2Fgithub.com%2Fexample")})]
          (test/is (= expected (:status result)))
          (when (= expected 303)
            (test/is (= "/edit/" (get-in result [:headers "Location"])))))))))

(test/deftest request-protection
  (fs/with-temp-dir [dir]
    (let [context {:state-dir dir :csrf "test-csrf" :hosts #{"127.0.0.1:8084"}
                   :cgit-url "http://127.0.0.1:8085/" :busy_ (atom false)}
          request {:request-method :post :uri "/add"
                   :headers {"host" "127.0.0.1:8084" "origin" "https://evil.example"}
                   :body (java.io.StringReader. "csrf=test-csrf&url=https%3A%2F%2Fgithub.com%2Fa")}]
      (test/is (= 400 (:status (handler context request))))
      (test/is (= [] (:repos (load-inventory dir))))
      (test/is (= 400 (:status (handler context {:request-method :get :uri "/" :headers {"host" "evil.example"}}))))
      (test/is (= 200 (:status (handler context {:request-method :get :uri "/" :headers {"host" "127.0.0.1:8084"}})))))))

(defn parse-options [args]
  (loop [options {:bind "127.0.0.1" :port 8084 :cgit-url "http://127.0.0.1:8085/"}
         args args]
    (if (empty? args) options
        (let [[key value & more] args]
          (require! (and value (contains? #{"--state-dir" "--bind" "--port" "--cgit-url" "--id" "--base-path" "--public-origin"} key))
                    "Usage: dependency-mirror [serve|sync|--test] [--state-dir DIR] [--bind IP] [--port PORT] [--cgit-url URL] [--base-path /PATH] [--public-origin ORIGIN] [--id ID]")
          (recur (assoc options (keyword (subs key 2)) (if (= key "--port") (parse-long value) value)) more)))))

(defn -main [& args]
  (let [command (or (first args) "serve")
        options (parse-options (rest args))]
    (case command
      "--test" (let [result (test/run-tests 'user)]
                 (when (pos? (+ (:fail result) (:error result)))
                   (System/exit 1)))
      "sync" (do (require! (:state-dir options) "sync requires --state-dir.")
                 (sync-repositories! (:state-dir options) (:id options)))
      "serve" (let [dir (or (:state-dir options) (str (fs/create-temp-dir {:prefix "dependency-mirror-"})))
                    options (assoc options :state-dir dir)]
                (require! (or (:state-dir (parse-options (rest args))) (= "127.0.0.1" (:bind options)))
                          "Non-local listeners require an explicit state directory.")
                (start-server! options)
                (println (str "Dependency mirrors: http://" (:bind options) ":" (:port options) "/"))
                (println "State directory:" dir)
                @(promise))
      (throw (ex-info "Expected serve, sync, or --test." {})))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))