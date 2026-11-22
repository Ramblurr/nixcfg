(ns ts-broker.lib
  "Defines UI pages and actions as vars that carry their routes, and the
  helpers both use. See [[ts-broker.http]] for how routes are served.

  A [[defview]] var holds its page path. GET on the path serves the shell
  page, and POST opens the page's SSE stream. A [[defaction]] var holds the
  POST path of its action, `/action/` and the action's name, so no route
  table lists them. [[routes]] collects both from namespaces."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str])
  (:import
   [java.net URLEncoder]))

(defn- definition
  "Returns the forms that define `sym-fn` with `doc`, `args` and `body`,
  and `sym` as `value` with `data` and the function var under `k` in its
  metadata."
  [sym doc args body value k data]
  (let [fn-sym (symbol (str sym "-fn"))]
    `(do
       (defn ~fn-sym ~@(when doc [doc]) ~args ~@body)
       (def ~sym ~@(when doc [doc]) ~value)
       (alter-meta! (var ~sym) assoc ~k ~(assoc data :fn `(var ~fn-sym))))))

(defmacro defaction
  "Defines `sym` as the POST path of an action and `sym-fn` as its function
  of the app context and the request. The function reads its parameters
  from the request body, throws on failure, and may return a map with
  `:replace`, an element that replaces the element with its id, and
  `:redirect`, a path the browser then loads."
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym & more]
  (let [[doc args & body] (if (string? (first more)) more (cons nil more))]
    (definition sym doc args body (str "/action/" sym) ::action {})))

(defmacro defview
  "Defines `sym` as the path of a page and `sym-fn` as its function of the
  app context and the request that returns the `main#morph` element. The
  page reads its parameters from the query string.

  | option   | description |
  |----------|-------------|
  | `:path`  | URL path of the page |
  | `:title` | Title of the shell page |"
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym & more]
  (let [[doc {:keys [path title]} args & body] (if (string? (first more)) more (cons nil more))]
    (when-not (and (string? path) (str/starts-with? path "/") (string? title))
      (throw (ex-info "A view needs a path and a title." {:name sym :path path :title title})))
    (definition sym doc args body path ::view {:title title})))

(defn routes
  "Returns the pages and actions defined in the namespaces `nss` as
  `{:pages {path {:title t :fn f}} :actions {path f}}`."
  [& nss]
  (let [vars (mapcat (comp vals ns-publics) nss)]
    {:pages   (into {} (keep #(some->> (::view (meta %)) (vector @%))) vars)
     :actions (into {} (keep #(some->> (::action (meta %)) :fn (vector @%))) vars)}))

(defn param
  "Returns the first value of parameter `k` of `request`, trimmed."
  [request k]
  (some-> (get-in request [:params k]) first str/trim))

(defn params
  "Returns the non-blank values of parameter `k` of `request`, trimmed."
  [request k]
  (vec (remove str/blank? (map str/trim (get-in request [:params k])))))

(defn href
  "Returns `path` with the non-nil entries of `query` as its query string."
  [path query]
  (let [query (remove (comp nil? val) query)]
    (str path
         (when (seq query)
           (str "?" (str/join "&" (for [[k v] query]
                                    (str (name k) "=" (URLEncoder/encode (str v) "UTF-8")))))))))

(defn post
  "Returns a Datastar expression that POSTs `payload` as JSON to `path`."
  [path payload]
  (str "@post('" path "', {payload: " (json/generate-string payload) "})"))

(defn confirm-post
  "Returns a Datastar expression that POSTs `payload` to `path` after the
  user confirms `message`."
  [message path payload]
  (str "confirm(" (json/generate-string message) ") && " (post path payload)))

(defn form-post
  "Returns a Datastar expression that POSTs the enclosing form to `path`."
  [path]
  (str "@post('" path "', {contentType: 'form'})"))
