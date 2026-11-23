#!/usr/bin/env bb
;; Prints the value of an op://vault/item/[section/]field reference, read
;; from 1Password Connect through the HTTP proxy on a unix socket, such as
;; ts-plug's -http-proxy. `op read` cannot dial a unix socket proxy.
;; Vaults, items, sections and fields match by ID or by name.
(require '[cheshire.core :as json] '[clojure.string :as str] '[org.httpkit.client :as http])
(import '[java.net StandardProtocolFamily UnixDomainSocketAddress]
        '[java.nio.channels SocketChannel])

(defn- fail [message] (throw (ex-info message {:safe true})))

(defn- unicode-blank? [s]
  (or (not (string? s))
      (nil? (re-find #"[^\t\n\x0B\f\r \u0085\u00A0\p{Zs}]" s))))

(defn parse-reference [reference]
  (let [parts (some-> (second (re-matches #"op://([^?]+)" reference)) (str/split #"/"))]
    (case (count parts)
      3 (zipmap [:vault :item :field] parts)
      4 (zipmap [:vault :item :section :field] parts)
      (fail (str "Unsupported reference: " reference)))))

(defn- connect [url token-file proxy-socket]
  (let [address (UnixDomainSocketAddress/of ^String proxy-socket)]
    {:url (str/replace url #"/+$" "")
     :request {:headers {"Authorization" (str "Bearer " (str/trim (slurp token-file)))}
               :as :text
               :timeout 10000
               :proxy-url "http://localhost"
               :client (http/make-client
                        {:address-finder (constantly address)
                         :channel-factory (fn [_] (SocketChannel/open StandardProtocolFamily/UNIX))})}}))

(defn- fetch [{:keys [url request]} path]
  (let [{:keys [status body error]} @(http/get (str url path) request)]
    (cond error (fail "Connect is unreachable")
          (not= 200 status) (fail (str "Connect answered " status))
          :else (json/parse-string body true))))

(defn- one [kind wanted candidates name-key]
  (let [matches (filter #(or (= wanted (:id %)) (= wanted (get % name-key))) candidates)]
    (case (count matches)
      1 (first matches)
      0 (fail (str "No " kind " " wanted))
      (fail (str "More than one " kind " " wanted)))))

(defn read-reference [client reference]
  (let [{:keys [vault item section field]} (parse-reference reference)
        vault-id (:id (one "vault" vault (fetch client "/v1/vaults") :name))
        items-path (str "/v1/vaults/" vault-id "/items")
        item-id (:id (one "item" item (fetch client items-path) :title))
        {:keys [fields sections]} (fetch client (str items-path "/" item-id))
        fields (if section
                 (let [section-id (:id (one "section" section sections :label))]
                   (filter #(= section-id (get-in % [:section :id])) fields))
                 fields)
        value (:value (one "field" field fields :label))]
    (if-not (unicode-blank? value)
      value
      (fail (str "Empty value for " reference)))))

(defn -main [& args]
  (if (not= 4 (count args))
    (do (binding [*out* *err*] (println "Usage: op-connect-read.clj URL TOKEN-FILE PROXY-SOCKET REFERENCE"))
        (System/exit 2))
    (let [[url token-file proxy-socket reference] args]
      (try (println (read-reference (connect url token-file proxy-socket) reference))
           (catch Exception e
             (binding [*out* *err*]
               (println "Cannot read" reference (if (:safe (ex-data e)) (ex-message e) (.getName (class e)))))
             (System/exit 1))))))
(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
