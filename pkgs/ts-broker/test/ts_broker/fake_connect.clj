(ns ts-broker.fake-connect
  "A 1Password Connect server for tests. It answers the vault, item list and
  item routes with the shapes Connect sends, and 401 without the token."
  (:require
   [cheshire.core :as json]
   [org.httpkit.server :as server]))

(def token "fake-connect-token")

(def vaults
  [{:id "vlt0000000000000000000000a" :name "Agents" :type "USER_CREATED"
    :attributeVersion 1 :contentVersion 3 :items 2
    :createdAt "2026-01-01T00:00:00Z" :updatedAt "2026-01-01T00:00:00Z"}
   {:id "vlt0000000000000000000000b" :name "Infra" :type "USER_CREATED"
    :attributeVersion 1 :contentVersion 1 :items 1
    :createdAt "2026-01-01T00:00:00Z" :updatedAt "2026-01-01T00:00:00Z"}])

(defn- field [id label value & [section]]
  (cond-> {:id id :type "CONCEALED" :purpose "" :label label :value value}
    section (assoc :section {:id section})))

(def items
  {"vlt0000000000000000000000a"
   [{:id "itm0000000000000000000000a" :title "GitHub token" :category "API_CREDENTIAL"
     :vault {:id "vlt0000000000000000000000a"} :version 2
     :fields [(field "credential" "credential" "ghp_secret_value")
              (field "username" "username" "casey-bot")
              (field "f1" "note" "one" "s1")
              (field "f2" "note" "two" "s2")]
     :sections [{:id "s1" :label "One"} {:id "s2" :label "Two"}]}
    {:id "itm0000000000000000000000b" :title "Anthropic API" :category "API_CREDENTIAL"
     :vault {:id "vlt0000000000000000000000a"} :version 1
     :fields [(field "credential" "credential" "sk-ant-secret")]}]
   "vlt0000000000000000000000b"
   [{:id "itm0000000000000000000000c" :title "Hetzner DNS" :category "PASSWORD"
     :vault {:id "vlt0000000000000000000000b"} :version 1
     :fields [(field "password" "password" "hz-secret")]}]})

(defn- summary [item]
  (dissoc item :fields :sections))

(defn- answer [status body]
  {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string body)})

(defn handler
  "Ring handler of the fake Connect server."
  [{:keys [uri headers]}]
  (if (not= (str "Bearer " token) (get headers "authorization"))
    (answer 401 {:status 401 :message "Invalid token"})
    (if-let [[_ vault item] (re-matches #"/v1/vaults(?:/([^/]+)/items(?:/([^/]+))?)?" uri)]
      (cond
        (nil? vault) (answer 200 vaults)
        (nil? item) (answer 200 (mapv summary (get items vault)))
        :else (if-let [i (first (filter #(= item (:id %)) (get items vault)))]
                (answer 200 i)
                (answer 404 {:status 404 :message "Item not found"})))
      (answer 404 {:status 404 :message "Not found"}))))

(defn start!
  "Starts the fake server on a free port on 127.0.0.1 and returns
  `{:url u :stop f}`."
  []
  (let [s (server/run-server handler {:ip "127.0.0.1" :port 0 :legacy-return-value? false})]
    {:url  (str "http://127.0.0.1:" (server/server-port s))
     :stop #(server/server-stop! s)}))
