#!/usr/bin/env bb

(require '[babashka.deps :as deps])
(deps/add-deps
 '{:deps
   {dev.data-star.clojure/sdk
    {:mvn/version "1.0.0-RC11"}

    dev.data-star.clojure/http-kit
    {:mvn/version "1.0.0-RC11"}}})

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

;;;  -- start framework --

(def datastar-url "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js")

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
         '[clojure.test :as test]
         '[flatland.ordered.map :refer [ordered-map]])

(import '[java.nio.charset StandardCharsets]
        '[java.nio.file CopyOption Files StandardCopyOption])

(def supported-types
  #{"A" "AAAA" "CAA" "CNAME" "MX" "SRV" "TXT"})

(def surfaces
  [[:public "Public" :publicTtl]
   [:lan "LAN" :lanTtl]
   [:tailscale "Tailscale" :tailscaleTtl]])

(def canonical-document-keys
  [:zone :domain :surfaces :records])

(def canonical-record-keys
  [:id :name :type :public :publicTtl :lan :lanTtl :tailscale :tailscaleTtl])

(defn canonical-json [document]
  (let [records (mapv #(apply ordered-map
                              (mapcat (fn [key]
                                        (when (contains? % key) [key (get % key)]))
                                      canonical-record-keys))
                      (:records document))
        document (ordered-map
                  :zone (:zone document)
                  :domain (:domain document)
                  :surfaces (mapv name (:surfaces document))
                  :records records)]
    (str (json/generate-string document {:pretty true}) "\n")))

(defn source-revision [content]
  (digest (.getBytes content StandardCharsets/UTF_8)))

(defn relative-name? [name]
  (and (string? name)
       (not (str/blank? name))
       (not (str/ends-with? name "."))
       (not (str/includes? name ".."))
       (or (#{"@" "*"} name)
           (boolean (re-matches #"(?i)[a-z0-9_*-]+(?:\.[a-z0-9_*-]+)*" name)))))

(defn normalized-id-part [value]
  (let [value (case value
                "@" "apex"
                "*" "wildcard"
                value)]
    (-> value
        str/lower-case
        (str/replace #"[^a-z0-9]+" "-")
        (str/replace #"(^-|-$)" ""))))

(defn generated-id [zone name type]
  (str (normalized-id-part zone)
       "-"
       (normalized-id-part name)
       "-"
       (str/lower-case type)))

(defn record-errors [zone document record original-id]
  (let [enabled (set (:surfaces document))
        original (some #(when (= original-id (:id %)) %) (:records document))
        id-owner (some #(when (= (:id record) (:id %)) %) (:records document))
        selected (filter #(contains? record %) enabled)
        errors (cond-> {}
                 (or (not (string? (:id record))) (str/blank? (:id record)))
                 (assoc :id "ID is required.")

                 (and original-id (nil? original))
                 (assoc :id "The record no longer exists in the working copy.")

                 (and original-id (not= original-id (:id record)))
                 (assoc :id "Existing IDs are immutable.")

                 (and (nil? original-id)
                      (not= (:id record)
                            (generated-id zone (:name record) (:type record))))
                 (assoc :id "New ID does not match its zone, name, and type.")

                 (and id-owner (not= original-id (:id id-owner)))
                 (assoc :id "ID already exists in this zone.")

                 (not (relative-name? (:name record)))
                 (assoc :name "Use a non-empty relative DNS name.")

                 (not (contains? supported-types (:type record)))
                 (assoc :type "Choose a supported record type.")

                 (empty? selected)
                 (assoc :surfaces "Select at least one enabled surface."))]
    (reduce (fn [errors [surface _ ttl-key]]
              (let [values (get record surface)
                    ttl (get record ttl-key)]
                (cond-> errors
                  (and (contains? record surface)
                       (not (contains? enabled surface)))
                  (assoc surface "Surface is not enabled for this zone.")

                  (and (contains? enabled surface)
                       (contains? record surface)
                       (or (not (vector? values))
                           (empty? values)
                           (some #(or (not (string? %)) (str/blank? %)) values)))
                  (assoc surface "Values must be non-empty strings.")

                  (and (contains? record ttl-key)
                       (not (and (integer? ttl) (pos? ttl))))
                  (assoc ttl-key "TTL must be a positive integer."))))
            errors
            surfaces)))

(defn document-errors [document]
  (let [zone (:zone document)
        ids (map :id (:records document))]
    (cond-> []
      (not= (set canonical-document-keys) (set (keys document)))
      (conj "Document fields are invalid.")

      (or (not (string? zone)) (str/blank? zone))
      (conj "Zone is required.")

      (or (not (string? (:domain document)))
          (str/blank? (:domain document)))
      (conj "Domain is required.")

      (or (not (vector? (:surfaces document)))
          (empty? (:surfaces document))
          (not-every? #{:public :lan :tailscale} (:surfaces document)))
      (conj "Surfaces are invalid.")

      (not (vector? (:records document)))
      (conj "Records must be an array.")

      (not= (count ids) (count (set ids)))
      (conj "Record IDs must be unique.")

      (some (fn [record]
              (or (seq (record-errors zone document record (:id record)))
                  (not (every? (set canonical-record-keys) (keys record)))
                  (not-every? #(contains? record %) [:id :name :type])))
            (:records document))
      (conj "One or more records are invalid."))))

(defn read-zone [path]
  (let [content (slurp (str path))
        document (-> (json/parse-string content true)
                     (update :surfaces #(mapv keyword %)))
        errors (document-errors document)]
    (when (seq errors)
      (throw (ex-info (str "Invalid zone file " path ": " (str/join " " errors))
                      {:path (str path)})))
    {:path (str path)
     :baseline content
     :revision (source-revision content)
     :document document}))

(defn load-zones [zones-dir]
  (let [paths (sort-by str (fs/list-dir zones-dir "*.json"))
        zones (map read-zone paths)]
    (when (empty? zones)
      (throw (ex-info (str "No JSON zone files found in " zones-dir) {})))
    (into (sorted-map)
          (map (fn [{:keys [document] :as zone}]
                 [(:zone document) zone]))
          zones)))

(defn initial-session [zones]
  {:zones zones
   :working (into {} (map (fn [[zone data]] [zone (:document data)]) zones))
   :selected (or (when (contains? zones "home") "home")
                 (first (keys zones)))
   :draft nil
   :errors {}
   :filter ""
   :sort-key :name
   :sort-direction :asc
   :message nil})

(defn session! [sessions_ sid zones]
  (or (get @sessions_ sid)
      (get (swap! sessions_ #(if (contains? % sid)
                               %
                               (assoc % sid (initial-session zones))))
           sid)))

(defn staged-zone? [session zone]
  (not= (get-in session [:zones zone :document])
        (get-in session [:working zone])))

(defn staged-count [session]
  (reduce +
          (for [[zone baseline] (:zones session)
                :let [before (:records (:document baseline))
                      after (get-in session [:working zone :records])
                      before-by-id (into {} (map (juxt :id identity)) before)
                      after-by-id (into {} (map (juxt :id identity)) after)
                      ids (into (set (keys before-by-id)) (keys after-by-id))]]
            (count (filter #(not= (get before-by-id %) (get after-by-id %)) ids)))))

(defn stale-zones [session zones]
  (filterv (fn [zone]
             (let [{:keys [path revision]} (get-in session [:zones zone])]
               (or (not (fs/exists? path))
                   (not= revision (source-revision (slurp path))))))
           zones))

(defn require-fresh! [session zones]
  (when-let [stale (seq (stale-zones session zones))]
    (throw (ex-info (str "Source changed externally: " (str/join ", " stale))
                    {:stale stale}))))

(defn record-index [document id]
  (first (keep-indexed #(when (= id (:id %2)) %1) (:records document))))

(defn stage-record [session zone draft original-id]
  (require-fresh! session [zone])
  (let [document (get-in session [:working zone])
        errors (record-errors zone document draft original-id)]
    (if (seq errors)
      (assoc session :draft (assoc (:draft session) :record draft)
             :errors errors
             :message "Fix validation errors before saving.")
      (let [records (:records document)
            i (when original-id (record-index document original-id))
            records' (if (some? i)
                       (assoc records i draft)
                       (conj records draft))]
        (assoc-in (assoc session :draft nil :errors {} :message "Change staged.")
                  [:working zone :records]
                  records')))))

(defn stage-delete [session zone id]
  (require-fresh! session [zone])
  (let [document (get-in session [:working zone])]
    (-> session
        (assoc-in [:working zone :records]
                  (filterv #(not= id (:id %)) (:records document)))
        (assoc :draft nil :errors {} :message "Deletion staged."))))

(defn atomic-replace! [path content]
  (let [target (.toAbsolutePath (fs/path path))
        temp (Files/createTempFile (.getParent target)
                                   (str "." (.getFileName target) ".")
                                   ".tmp"
                                   (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (Files/writeString temp content StandardCharsets/UTF_8
                         (make-array java.nio.file.OpenOption 0))
      (Files/move temp target
                  (into-array CopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (finally
        (Files/deleteIfExists temp)))))

(defn commit-session! [session]
  (let [changed (filterv #(staged-zone? session %) (keys (:zones session)))
        replacements (into {}
                           (map (fn [zone]
                                  (let [document (get-in session [:working zone])
                                        errors (document-errors document)]
                                    (when (seq errors)
                                      (throw (ex-info (str "Invalid staged zone " zone ": "
                                                           (str/join " " errors))
                                                      {:zone zone})))
                                    [zone (canonical-json document)])))
                           changed)]
    (require-fresh! session changed)
    (doseq [zone changed]
      (atomic-replace! (get-in session [:zones zone :path])
                       (get replacements zone)))
    (reduce (fn [next-session zone]
              (let [path (get-in next-session [:zones zone :path])
                    content (slurp path)
                    document (get-in next-session [:working zone])]
                (-> next-session
                    (assoc-in [:zones zone :baseline] content)
                    (assoc-in [:zones zone :revision] (source-revision content))
                    (assoc-in [:zones zone :document] document))))
            (assoc session :draft nil :errors {} :message "Changes committed to disk.")
            changed)))

(defn empty-record [_zone]
  {:id ""
   :name ""
   :type "A"})

(defn update-draft-from-signals [session signals]
  (let [{:keys [zone original-id record]} (:draft session)
        document (get-in session [:working zone])
        selected (set (keys record))
        record-name (get signals :name (:name record))
        type (get signals :type (:type record))
        id (if original-id
             original-id
             (if (or (str/blank? record-name) (str/blank? type))
               ""
               (generated-id zone record-name type)))
        record (reduce (fn [next-record [surface _ ttl-key]]
                         (if (contains? selected surface)
                           (let [prefix (name surface)
                                 value-keys (map #(keyword (str prefix %))
                                                 (range (count (get record surface))))
                                 values (mapv #(get signals % "") value-keys)
                                 ttl-text (str/trim (str (get signals ttl-key
                                                              (get record ttl-key ""))))]
                             (cond-> (assoc next-record surface values)
                               (not (str/blank? ttl-text))
                               (assoc ttl-key (or (parse-long ttl-text) ttl-text))))
                           next-record))
                       {:id id :name record-name :type type}
                       surfaces)]
    (assoc session
           :draft (assoc (:draft session) :record record)
           :errors (record-errors zone document record original-id)
           :message nil)))

(defn visible-errors [session]
  (select-keys (:errors session) (get-in session [:draft :touched] #{})))

(def site-css
  (static-css
   "body{font-family:system-ui,sans-serif;margin:2rem auto;max-width:1280px;padding:0 1rem;color:#202124}header,.toolbar,.filters,.actions,.surface-head,.value-row{display:flex;gap:.6rem;align-items:center;flex-wrap:wrap}header{justify-content:space-between}select,input,button{font:inherit;padding:.4rem}.field{display:grid;gap:.35rem}.filters{margin-top:.8rem}.filters input{min-width:24rem}.primary{background:#1769e0;color:white;border:1px solid #1769e0;border-radius:4px}.danger{color:#a00016}.muted{color:#666;font-size:.82rem}.message{padding:.6rem;background:#f1f3f4}.error{color:#b00020;font-size:.82rem}table{width:100%;border-collapse:collapse;margin-top:1rem;table-layout:fixed}th,td{text-align:left;border-bottom:1px solid #ddd;padding:.45rem;vertical-align:top;overflow-wrap:anywhere}th{font-size:.8rem;color:#555}.rrset{font-size:.82rem}.rrset span{display:block}.editor td{background:#f7f9fc}.edit-grid{display:grid;grid-template-columns:1fr 1fr;gap:.8rem}.surface{border:1px solid #ddd;padding:.7rem;margin:.6rem 0}.surface-head{justify-content:space-between}.value-row{margin:.35rem 0}.value-row input{flex:1}dialog{border:1px solid #aaa;border-radius:6px;max-width:720px;width:calc(100% - 2rem);padding:1.2rem}dialog::backdrop{background:#0008}.badge{background:#eee;border-radius:1rem;padding:.2rem .55rem}.staged{background:#fff2c2}@media(max-width:700px){.edit-grid{grid-template-columns:1fr}.filters input{min-width:12rem}table{font-size:.78rem}}"))

(def head-elements
  [[:title {:id "page-title"} "DNS records"]
   [:link {:rel "stylesheet" :href site-css}]])

(defn action-js [action]
  (str "@post('" action "')"))

(defn field-error [errors field]
  (when-let [error (get errors field)]
    [:div {:class "error"} error]))

(declare change-draft toggle-surface add-value remove-value save-draft cancel-draft
         begin-add begin-clone begin-edit confirm-delete cancel-delete commit-changes
         reset-session select-zone change-filter change-sort)

(defn value-inputs [surface values]
  (map-indexed
   (fn [i value]
     (let [signal (str (name surface) i)]
       [:div {:class "value-row"}
        [:input {:aria-label (str (name surface) " value " (inc i))
                 :data-bind signal
                 :data-on:input__debounce.200ms (str "$field="
                                                     (json/generate-string (name surface))
                                                     "; " (action-js change-draft))
                 :data-init (str "$" signal "=" (json/generate-string value))
                 :value value}]
        [:button {:type "button"
                  :data-on:click (str "$surface=" (json/generate-string (name surface))
                                      "; $valueindex=" i "; "
                                      (action-js remove-value))}
         "Remove"]]))
   values))

(defn surface-editor [record errors [surface label ttl-key]]
  (when (contains? record surface)
    [:fieldset {:class "surface"}
     [:div {:class "surface-head"}
      [:legend label]
      [:button {:type "button"
                :data-on:click (str "$surface=" (json/generate-string (name surface))
                                    "; " (action-js add-value))}
       "Add value"]]
     (value-inputs surface (get record surface))
     (field-error errors surface)
     [:label "TTL "
      [:input {:type "number"
               :min 1
               :placeholder (if (= surface :public) "default 3600" "default 300")
               :data-bind (name ttl-key)
               :data-on:input__debounce.200ms (str "$field="
                                                   (json/generate-string (name ttl-key))
                                                   "; " (action-js change-draft))
               :data-init (str "$" (name ttl-key) "="
                               (json/generate-string (str (get record ttl-key ""))))
               :value (get record ttl-key "")}]]
     (field-error errors ttl-key)]))

(defn editor-fields [{:keys [record original-id]} errors]
  [:div
   [:div {:class "edit-grid"}
    [:label {:class "field"} "Name"
     [:input {:data-bind "name"
              :data-on:input__debounce.200ms (str "$field=\"name\"; "
                                                  (action-js change-draft))
              :data-init (str "$name=" (json/generate-string (:name record)))
              :value (:name record)}]
     (field-error errors :name)]
    [:label {:class "field"} "ID"
     [:input {:value (:id record) :readonly true}]
     (field-error errors :id)]
    [:label {:class "field"} "Type"
     (into [:select {:data-bind "type"
                     :data-on:change (str "$field=\"type\"; "
                                          (action-js change-draft))
                     :data-init (str "$type=" (json/generate-string (:type record)))
                     :value (:type record)}]
           (map #(vector :option {:value % :selected (= % (:type record))} %)
                (sort supported-types)))
     (field-error errors :type)]]
   (field-error errors :surfaces)
   [:div {:class "actions"}
    (for [[surface label _] surfaces]
      [:label
       [:input {:type "checkbox"
                :checked (contains? record surface)
                :data-on:change (str "$surface=" (json/generate-string (name surface))
                                     "; " (action-js toggle-surface))}]
       (str " " label)])]
   (keep #(surface-editor record errors %) surfaces)
   [:div {:class "actions"}
    [:button {:class "primary"
              :type "button"
              :data-on:click (action-js save-draft)}
     "Save"]
    [:button {:type "button" :data-on:click (action-js cancel-draft)} "Cancel"]
    (when original-id
      [:button {:class "danger"
                :type "button"
                :data-on:click "document.getElementById('delete-dialog').showModal()"}
       "Delete"])]])

(defn surface-cell [record surface ttl-key]
  (if-let [values (get record surface)]
    [:div {:class "rrset"}
     (map #(vector :span %) values)
     (when (contains? record ttl-key)
       [:span {:class "muted"} (str "TTL " (get record ttl-key))])]
    [:span {:class "muted"} "—"]))

(defn record-search-text [record]
  (str/lower-case
   (str/join " "
             (concat [(:name record) (:type record) (:id record)]
                     (mapcat #(get record (first %) []) surfaces)))))

(defn visible-records [session document]
  (let [query (str/lower-case (str/trim (:filter session)))
        direction (if (= :desc (:sort-direction session)) #(compare %2 %1) compare)
        sort-value (fn [record]
                     (case (:sort-key session)
                       :type (:type record)
                       :id (:id record)
                       (:name record)))]
    (->> (:records document)
         (filter #(or (str/blank? query)
                      (str/includes? (record-search-text %) query)))
         (sort-by sort-value direction))))

(defn sort-button [session key label]
  [:button {:data-on:click (str "$sortkey=" (json/generate-string (name key))
                                "; " (action-js change-sort))}
   (str label (when (= key (:sort-key session))
                (if (= :asc (:sort-direction session)) " ↑" " ↓")))])

(defn records-table [session zone]
  (let [document (get-in session [:working zone])
        draft (:draft session)
        enabled (set (:surfaces document))
        shown-surfaces (filter #(contains? enabled (first %)) surfaces)
        column-count (+ 4 (count shown-surfaces))]
    [:table
     [:thead
      (into [:tr [:th (sort-button session :name "Name")]
             [:th (sort-button session :type "Type")]]
            (concat
             (map (fn [[_ label _]] [:th label]) shown-surfaces)
             [[:th (sort-button session :id "ID")] [:th]]))]
     (into
      [:tbody]
      (mapcat (fn [record]
                (let [editing? (= (:id record) (:original-id draft))]
                  [(into [:tr [:td (:name record)]
                          [:td (:type record)]]
                         (concat
                          (map (fn [[surface _ ttl-key]]
                                 [:td (surface-cell record surface ttl-key)])
                               shown-surfaces)
                          [[:td {:class "muted"} (:id record)]
                           [:td {:class "actions"}
                            [:button {:data-on:click (str "$recordid="
                                                          (json/generate-string (:id record))
                                                          "; " (action-js begin-edit))}
                             "Edit"]
                            [:button {:data-on:click (str "$recordid="
                                                          (json/generate-string (:id record))
                                                          "; " (action-js begin-clone)
                                                          "; document.getElementById('add-dialog').showModal()")}
                             "Clone"]]]))
                   (when editing?
                     [:tr {:class "editor"}
                      [:td {:colspan column-count}
                       (editor-fields draft (visible-errors session))]])]))
              (visible-records session document)))]))

(defn page [{:keys [sid sessions_ zones]}]
  (let [session (session! sessions_ sid zones)
        zone (:selected session)
        draft (:draft session)
        additions? (= :add (:kind draft))]
    (list
     [:title {:id "page-title"} (str "DNS records — " zone)]
     [:main {:id "morph"}
      [:header
       [:div [:h1 "DNS records"] [:div {:class "muted"} "Files only; no provider changes."]]
       [:div {:class "actions"}
        [:span {:class (str "badge " (when (pos? (staged-count session)) "staged"))}
         (str (staged-count session) " change" (when (not= 1 (staged-count session)) "s") " staged")]
        [:button {:class "primary"
                  :disabled (zero? (staged-count session))
                  :data-on:click (action-js commit-changes)}
         "Commit changes"]
        [:button {:data-on:click (action-js reset-session)} "Reset"]]]
      [:div {:class "toolbar"}
       [:label "Zone "
        (into [:select {:data-bind "zone"
                        :data-on:change (action-js select-zone)}]
              (map (fn [[alias data]]
                     [:option {:value alias :selected (= alias zone)}
                      (str (get-in data [:document :domain]) " (" alias ")")])
                   zones))]
       [:button {:data-on:click (str (action-js begin-add)
                                     "; document.getElementById('add-dialog').showModal()")}
        "Add record"]]
      [:div {:class "filters"}
       [:label {:class "field"} "Filter"
        [:input {:type "search"
                 :placeholder "Search names, values, and IDs"
                 :data-bind "filter"
                 :data-init (str "$filter=" (json/generate-string (:filter session)))
                 :data-on:input__debounce.200ms (action-js change-filter)}]]]
      (when-let [message (:message session)]
        [:p {:class (if (seq (:errors session)) "error" "message")} message])
      (records-table session zone)
      [:dialog {:id "add-dialog"
                :data-preserve-attr "open"
                :data-init (when additions? "if (!el.open) el.showModal()")}
       [:h2 "Add record"]
       (when additions? (editor-fields draft (visible-errors session)))]
      [:dialog {:id "delete-dialog"}
       [:h2 "Delete record?"]
       [:p "This stages deletion from the working copy. Disk is unchanged until Commit changes."]
       [:div {:class "actions"}
        [:button {:class "danger"
                  :data-on:click (str "el.closest('dialog').close(); "
                                      (action-js confirm-delete))}
         "Delete"]
        [:button {:data-on:click "document.getElementById('delete-dialog').close()"} "Cancel"]]]])))

(defn update-session! [{:keys [sessions_ sid]} f]
  (swap! sessions_ update sid f)
  (refresh-all!))

(defaction change-filter [request]
  (let [filter-text (:filter (request-signals request) "")]
    (update-session! request #(assoc % :filter filter-text)))
  nil)

(defaction change-sort [request]
  (let [sort-key (keyword (:sortkey (request-signals request)))
        allowed #{:name :type :id}]
    (update-session! request
                     #(if (contains? allowed sort-key)
                        (if (= sort-key (:sort-key %))
                          (update % :sort-direction {:asc :desc :desc :asc})
                          (assoc % :sort-key sort-key :sort-direction :asc))
                        %)))
  nil)

(defaction select-zone [request]
  (let [zone (:zone (request-signals request))]
    (update-session! request #(if (contains? (:zones %) zone)
                                (assoc % :selected zone :draft nil :errors {} :message nil)
                                %)))
  nil)

(defaction begin-edit [request]
  (let [id (:recordid (request-signals request))]
    (update-session! request
                     (fn [session]
                       (let [zone (:selected session)
                             document (get-in session [:working zone])
                             record (some #(when (= id (:id %)) %) (:records document))]
                         (if record
                           (assoc session :draft {:kind :edit
                                                  :zone zone
                                                  :original-id id
                                                  :record record
                                                  :touched #{}}
                                  :errors {}
                                  :message nil)
                           session)))))
  nil)

(defaction begin-clone [request]
  (let [id (:recordid (request-signals request))]
    (update-session! request
                     (fn [session]
                       (let [zone (:selected session)
                             record (some #(when (= id (:id %)) %)
                                          (get-in session [:working zone :records]))]
                         (if record
                           (assoc session :draft {:kind :add
                                                  :zone zone
                                                  :original-id nil
                                                  :record (assoc record :id "" :name "")
                                                  :touched #{}}
                                  :errors {}
                                  :message nil)
                           session)))))
  nil)

(defaction begin-add [request]
  (update-session! request
                   (fn [session]
                     (let [zone (:selected session)
                           document (get-in session [:working zone])
                           record (reduce (fn [record [surface _ _]]
                                            (if (some #{surface} (:surfaces document))
                                              (assoc record surface [""])
                                              record))
                                          (empty-record zone)
                                          surfaces)]
                       (assoc session :draft {:kind :add
                                              :zone zone
                                              :original-id nil
                                              :record record
                                              :touched #{}}
                              :errors {}
                              :message nil))))
  nil)

(defaction change-draft [request]
  (let [signals (request-signals request)
        field (keyword (:field signals))]
    (update-session! request
                     (fn [session]
                       (let [session (update-draft-from-signals session signals)]
                         (if field
                           (update-in session [:draft :touched] conj field)
                           session)))))
  nil)

(defaction toggle-surface [request]
  (let [signals (request-signals request)
        surface (keyword (:surface signals))]
    (update-session! request
                     (fn [session]
                       (let [session (update-draft-from-signals session signals)
                             enabled (set (get-in session [:working (:selected session) :surfaces]))]
                         (if (contains? enabled surface)
                           (let [session (-> (if (contains? (get-in session [:draft :record]) surface)
                                               (update-in session [:draft :record] dissoc surface)
                                               (assoc-in session [:draft :record surface] [""]))
                                             (update-in [:draft :touched] conj :surfaces surface))
                                 {:keys [zone original-id record]} (:draft session)
                                 document (get-in session [:working zone])]
                             (assoc session :errors
                                    (record-errors zone document record original-id)))
                           session)))))
  nil)

(defaction add-value [request]
  (let [signals (request-signals request)
        surface (keyword (:surface signals))]
    (update-session! request
                     (fn [session]
                       (let [session (update-draft-from-signals session signals)]
                         (if (contains? (set (get-in session [:working (:selected session) :surfaces])) surface)
                           (update-in session [:draft :record surface] conj "")
                           session)))))
  nil)

(defaction remove-value [request]
  (let [signals (request-signals request)
        surface (keyword (:surface signals))
        i (parse-long (str (:valueindex signals)))]
    (update-session! request
                     (fn [session]
                       (let [session (update-draft-from-signals session signals)]
                         (if (and (some? i)
                                  (contains? (get-in session [:draft :record]) surface)
                                  (> (count (get-in session [:draft :record surface])) 1))
                           (update-in session [:draft :record surface]
                                      (fn [values]
                                        (vec (concat (subvec values 0 i)
                                                     (subvec values (inc i))))))
                           session)))))
  nil)

(defaction save-draft [request]
  (let [signals (request-signals request)]
    (update-session! request
                     (fn [session]
                       (let [session (update-draft-from-signals session signals)
                             session (assoc-in session [:draft :touched]
                                               (set (keys (:errors session))))
                             {:keys [zone original-id record]} (:draft session)]
                         (try
                           (stage-record session zone record original-id)
                           (catch Exception exception
                             (assoc session :message (ex-message exception))))))))
  nil)

(defaction cancel-draft [request]
  (update-session! request #(assoc % :draft nil :errors {} :message nil))
  nil)

(defaction confirm-delete [request]
  (update-session! request
                   (fn [session]
                     (let [{:keys [zone original-id]} (:draft session)]
                       (try
                         (stage-delete session zone original-id)
                         (catch Exception exception
                           (assoc session :message (ex-message exception)))))))
  nil)

(defaction cancel-delete [_request]
  nil)

(defaction reset-session [{:keys [zones-dir] :as request}]
  (update-session! request
                   (fn [session]
                     (try
                       (let [zones (load-zones zones-dir)
                             selected (:selected session)
                             reset (initial-session zones)]
                         (cond-> (assoc reset :message "Reloaded zone files from disk.")
                           (contains? zones selected) (assoc :selected selected)))
                       (catch Exception exception
                         (assoc session :message (str "Reset failed: " (ex-message exception)))))))
  nil)

(defaction commit-changes [request]
  (update-session! request
                   (fn [session]
                     (try
                       (commit-session! session)
                       (catch Exception exception
                         (assoc session :message (ex-message exception))))))
  nil)

(def pages
  {"/" #'page})

(def actions
  (action-map select-zone change-filter change-sort begin-edit begin-add begin-clone change-draft
              toggle-surface add-value remove-value save-draft cancel-draft confirm-delete
              cancel-delete commit-changes reset-session))

(defn router [{:keys [request-method uri] :as request}]
  (case [request-method uri]
    [:get "/health"] {:status 200
                      :headers {"Content-Type" "text/plain; charset=utf-8"}
                      :body "ok"}
    (routes pages actions head-elements request)))

(defn fixture-document [zone domain surfaces records]
  (ordered-map :zone zone :domain domain :surfaces surfaces :records records))

(defn write-fixture! [dir document]
  (spit (str (fs/path dir (str (:zone document) ".json")))
        (str (json/generate-string document {:pretty true}) "\n")))

(defn action-request [sessions_ zones zones-dir sid signals]
  {:request-method :post
   :sid sid
   :sessions_ sessions_
   :zones zones
   :zones-dir (str zones-dir)
   :body (json/generate-string signals)})

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defn run-tests! []
  (fs/with-temp-dir [dir]
    (let [home (fixture-document
                "home" "home.example" ["public" "lan"]
                [{:id "home-text-txt" :name "text" :type "TXT"
                  :public ["hello txt"] :lan ["private txt"]}])
          other (fixture-document
                 "casey.link" "casey.example" ["public"]
                 [{:id "legacy-id" :name "@" :type "A" :public ["192.0.2.1"]}])
          _ (doseq [document [home other]] (write-fixture! dir document))
          zones (load-zones dir)
          sessions_ (atom {"a" (initial-session zones) "b" (initial-session zones)})
          post! (fn [sid action signals]
                  (action (action-request sessions_ zones dir sid signals)))
          home-path (str (fs/path dir "home.json"))
          home-before (slurp home-path)]
      (test/is (= ["casey.link" "home"] (vec (keys zones))))
      (test/is (= "casey-link-apex-a" (generated-id "casey.link" "@" "A")))
      (test/is (= "home-wildcard-cname" (generated-id "home" "*" "CNAME")))
      (post! "a" begin-clone-fn {:recordid "home-text-txt"})
      (test/is (= {:id ""
                   :name ""
                   :type "TXT"
                   :public ["hello txt"]
                   :lan ["private txt"]}
                  (get-in @sessions_ ["a" :draft :record])))
      (post! "a" cancel-draft-fn {})
      (post! "a" begin-add-fn {})
      (test/is (= {:id "" :name ""}
                  (select-keys (get-in @sessions_ ["a" :draft :record]) [:id :name])))
      (post! "a" toggle-surface-fn
             {:surface "lan" :name "" :type "A" :public0 "" :lan0 ""})
      (test/is (not (contains? (get-in @sessions_ ["a" :draft :record]) :lan)))
      (test/is (= {} (visible-errors (get @sessions_ "a"))))
      (post! "a" toggle-surface-fn
             {:surface "lan" :name "" :type "A" :public0 ""})
      (test/is (= [""] (get-in @sessions_ ["a" :draft :record :lan])))
      (post! "a" save-draft-fn
             {:name "" :type "A" :public0 "" :lan0 ""})
      (test/is (= #{:id :name :public :lan}
                  (set (keys (visible-errors (get @sessions_ "a"))))))
      (post! "a" cancel-draft-fn {})
      (post! "a" begin-add-fn {})
      (post! "a" save-draft-fn
             {:name "www" :type "CNAME" :public0 "target.example."
              :lan0 "target.home.example."})
      (test/is (= {:staged 1 :other-session 0 :disk home-before}
                  {:staged (staged-count (get @sessions_ "a"))
                   :other-session (staged-count (get @sessions_ "b"))
                   :disk (slurp home-path)}))
      (post! "a" begin-edit-fn {:recordid "home-text-txt"})
      (post! "a" change-draft-fn
             {:name "text" :type "BOGUS" :public0 "" :lan0 "private txt"
              :publicTtl "0"})
      (test/is (= #{:type :public :publicTtl}
                  (set (keys (get-in @sessions_ ["a" :errors])))))
      (post! "a" cancel-draft-fn {})
      (test/is (nil? (get-in @sessions_ ["a" :draft])))
      (post! "a" select-zone-fn {:zone "casey.link"})
      (post! "a" begin-edit-fn {:recordid "legacy-id"})
      (post! "a" save-draft-fn {:name "@" :type "A" :public0 "192.0.2.2"})
      (test/is (= "legacy-id"
                  (get-in @sessions_ ["a" :working "casey.link" :records 0 :id])))
      (post! "a" commit-changes-fn {})
      (test/is (= 0 (staged-count (get @sessions_ "a"))))
      (test/is (= 2 (count (:records (json/parse-string (slurp home-path) true)))))
      (post! "a" begin-edit-fn {:recordid "home-text-txt"})
      (post! "a" confirm-delete-fn {})
      (post! "a" reset-session-fn {})
      (test/is (= {:staged 0
                   :records 2
                   :selected "casey.link"
                   :message "Reloaded zone files from disk."}
                  {:staged (staged-count (get @sessions_ "a"))
                   :records (count (get-in @sessions_ ["a" :working "home" :records]))
                   :selected (get-in @sessions_ ["a" :selected])
                   :message (get-in @sessions_ ["a" :message])}))
      (post! "a" select-zone-fn {:zone "home"})
      (post! "a" begin-edit-fn {:recordid "home-text-txt"})
      (post! "a" confirm-delete-fn {})
      (let [forms (tree-seq coll? seq
                            (page {:sid "a" :sessions_ sessions_ :zones zones}))]
        (test/is (= {:dialog {:id "delete-dialog"}
                     :confirm-action (str "el.closest('dialog').close(); "
                                          (action-js confirm-delete))}
                    {:dialog (some #(when (= "delete-dialog" (get-in % [1 :id]))
                                      (second %))
                                   forms)
                     :confirm-action (some #(when (= "danger" (get-in % [1 :class]))
                                              (get-in % [1 :data-on:click]))
                                           forms)})))
      (spit home-path "{external}\n")
      (post! "a" commit-changes-fn {})
      (test/is (= {:message "Source changed externally: home"
                   :staged 1
                   :disk "{external}\n"}
                  {:message (get-in @sessions_ ["a" :message])
                   :staged (staged-count (get @sessions_ "a"))
                   :disk (slurp home-path)}))
      (test/is (try (load-zones dir) false (catch Exception _ true)))
      (test/is (= "{external}\n" (slurp home-path)))
      (post! "a" reset-session-fn {})
      (test/is (= {:staged 1
                   :disk "{external}\n"
                   :error? true}
                  {:staged (staged-count (get @sessions_ "a"))
                   :disk (slurp home-path)
                   :error? (str/starts-with? (get-in @sessions_ ["a" :message])
                                             "Reset failed:")}))
      (println "dns-admin-ui: 21 fixture assertions passed")
      {:test 21 :pass 21 :fail 0 :error 0})))

(defn parse-args [args]
  (loop [options {:port 8083
                  :zones-dir (str (fs/expand-home "~/nixcfg-private/terranix/dns/zones"))}
         args args]
    (if (empty? args)
      options
      (case (first args)
        "--port" (recur (assoc options :port (parse-long (second args))) (nnext args))
        "--zones-dir" (recur (assoc options :zones-dir (second args)) (nnext args))
        "--test" (assoc options :test? true)
        (throw (ex-info (str "Unknown argument: " (first args)) {}))))))

(defn start-local-app [context port]
  (let [wrapped-router (-> (fn [request]
                             (router (merge request context)))
                           wrap-session)
        stop-server (srv/run-server wrapped-router
                                    {:ip "127.0.0.1"
                                     :port port
                                     :legacy-unsafe-remote-addr? false})]
    {:handler wrapped-router
     :stop #(stop-server {:timeout 100})}))

(defn -main [& args]
  (let [{:keys [port zones-dir test?]} (parse-args args)]
    (if test?
      (run-tests!)
      (let [zones (load-zones zones-dir)
            sessions_ (atom {})
            url (str "http://127.0.0.1:" port "/")]
        (start-local-app {:zones zones :zones-dir zones-dir :sessions_ sessions_} port)
        (println "serving" url "from" zones-dir)
        @(promise)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))