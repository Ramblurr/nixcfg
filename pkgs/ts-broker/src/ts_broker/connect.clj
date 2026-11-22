(ns ts-broker.connect
  "Read-only 1Password Connect catalog: vaults, items and field labels.

  Field values never leave this namespace. Responses are cached for a short
  time, so page renders do not query Connect on every change."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [org.httpkit.client :as http])
  (:import
   [java.net StandardProtocolFamily URLEncoder UnixDomainSocketAddress]
   [java.nio.channels SocketChannel]))

(def ^:private ttl-ms 30000)

(defn- proxy-client
  "Returns an HTTP client whose connections all go to the unix socket at
  `path`."
  [^String path]
  (let [address (UnixDomainSocketAddress/of path)]
    (http/make-client {:address-finder  (constantly address)
                       :channel-factory (fn [_] (SocketChannel/open StandardProtocolFamily/UNIX))})))

(defn client
  "Returns a Connect client for the server at `url` with `token`. With
  `:proxy-socket`, requests go through the HTTP proxy on that unix socket,
  such as ts-plug's `-http-proxy`."
  ([url token] (client url token {}))
  ([url token {:keys [proxy-socket]}]
   {:url     (str/replace url #"/+$" "")
    :cache   (atom {})
    :request (cond-> {:headers {"Authorization" (str "Bearer " token)}
                      :as      :text
                      :timeout 10000}
               proxy-socket (assoc :client (proxy-client proxy-socket)
                                   :proxy-url "http://localhost"))}))

(defn- fetch [{:keys [url request]} path]
  (let [{:keys [status body error]} @(http/get (str url path) request)]
    (when error
      (throw (ex-info "Connect is unreachable." {:type ::unavailable})))
    (when-not (= 200 status)
      (throw (ex-info (str "Connect answered " status ".") {:type ::unavailable :status status})))
    (json/parse-string body true)))

(defn- cached
  "Returns `(f body)` for the Connect response at `path`, from the cache
  when it is fresh. Only the result of `f` is cached."
  [{:keys [cache] :as client} path f]
  (let [now (System/currentTimeMillis)
        {:keys [at value]} (get @cache path)]
    (if (and at (< (- now at) ttl-ms))
      value
      (let [value (f (fetch client path))]
        (swap! cache assoc path {:at now :value value})
        value))))

(defn- encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn vaults
  "Returns the vaults that the token can read as maps of `:id` and `:name`,
  ordered by name."
  [client]
  (cached client "/v1/vaults"
          #(->> (map (fn [v] (select-keys v [:id :name])) %)
                (sort-by (comp str/lower-case str :name))
                vec)))

(defn items
  "Returns the items of vault `vault-id` as maps of `:id`, `:title` and
  `:category`, ordered by title."
  [client vault-id]
  (cached client (str "/v1/vaults/" (encode vault-id) "/items")
          #(->> (map (fn [i] (select-keys i [:id :title :category])) %)
                (sort-by (comp str/lower-case str :title))
                vec)))

(defn item
  "Returns item `item-id` of vault `vault-id` with `:id`, `:title` and
  `:fields`. Each field holds `:id`, `:label`, `:type` and `:section`, the
  section label or nil. Field values are dropped."
  [client vault-id item-id]
  (cached client (str "/v1/vaults/" (encode vault-id) "/items/" (encode item-id))
          (fn [{:keys [id title fields sections]}]
            (let [section-labels (into {} (map (juxt :id :label)) sections)]
              {:id     id
               :title  title
               :fields (mapv (fn [f]
                               {:id      (:id f)
                                :label   (:label f)
                                :type    (:type f)
                                :section (get section-labels (get-in f [:section :id]))})
                             fields)}))))

(defn- field-label [{:keys [id label]}]
  (if (str/blank? label) id label))

(defn titles
  "Returns the current vault, item and field titles of `credentials` as
  maps of `:name`, `:vault-title`, `:item-title` and `:field-label`. Skips
  a credential whose vault, item or field Connect does not have. Throws
  when Connect is unavailable."
  [client credentials]
  (let [vault-names (into {} (map (juxt :id :name)) (vaults client))]
    (vec (for [{:keys [name vault field] item-id :item} credentials
               :let [i (when (vault-names vault)
                         (try
                           (item client vault item-id)
                           (catch clojure.lang.ExceptionInfo e
                             (when-not (= 404 (:status (ex-data e)))
                               (throw e)))))
                     f (first (filter #(= field (:id %)) (:fields i)))]
               :when f]
           {:name        name
            :vault-title (vault-names vault)
            :item-title  (:title i)
            :field-label (field-label f)}))))

(defn error-message
  "Returns the message of a Connect failure `e`, or nil for other errors."
  [e]
  (when (= ::unavailable (:type (ex-data e)))
    (ex-message e)))
