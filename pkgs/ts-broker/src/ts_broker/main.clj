(ns ts-broker.main
  "Command line entry point of ts-broker."
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.http-client :as http-client]
   [clojure.string :as str]
   [ts-broker.connect :as connect]
   [ts-broker.http :as http]
   [ts-broker.socket :as socket]
   [ts-broker.store :as store]
   [ts-broker.ui :as ui])
  (:import
   [java.time Instant]))

(def spec
  {:db                 {:desc    "SQLite database file"
                        :default "/var/lib/ts-broker/broker.db"}
   :socket             {:desc    "Unix socket for decide requests"
                        :default "/run/ts-broker/decide.sock"}
   :port               {:desc    "UI port on 127.0.0.1"
                        :coerce  :long
                        :default 795}
   :connect-url        {:desc "1Password Connect URL for the catalog"}
   :connect-token-file {:desc "File with the Connect token (default: the op-connect-token systemd credential)"}})

(def usage
  (str "Usage: ts-broker serve [options]\n\n"
       "Answers ts-egress decide requests on a unix socket and serves the\n"
       "approval UI on 127.0.0.1.\n\nOptions:\n"
       (cli/format-opts {:spec spec :order [:db :socket :port :connect-url :connect-token-file]})
       "\n"))

(def datastar-url
  "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js")

(defn- datastar-js
  "Returns the Datastar bundle from the file in `TS_BROKER_DATASTAR`, which
  the package sets, or from the CDN."
  []
  (if-let [path (System/getenv "TS_BROKER_DATASTAR")]
    (slurp path)
    (:body (http-client/get datastar-url {:timeout 10000}))))

(defn- token-file [opts]
  (or (:connect-token-file opts)
      (some-> (System/getenv "CREDENTIALS_DIRECTORY") (fs/path "op-connect-token") str)))

(defn- fail! [message]
  (binding [*out* *err*]
    (println message))
  (System/exit 2))

(defn start!
  "Starts the store, the decide socket and the UI from parsed `opts`.
  Returns the running system for [[stop!]]."
  [opts]
  (let [token (some-> (token-file opts) slurp str/trim)
        st (store/start! (:db opts) {})
        ctx {:store   st
             :clock   #(Instant/now)
             :connect (some-> (:connect-url opts) (connect/client token))}]
    {:store  st
     :socket (socket/start! st (:socket opts) {})
     :http   (http/start! {:port (:port opts) :ctx ctx :pages ui/pages :actions ui/actions :css ui/css
                           :datastar-js (datastar-js)})}))

(defn stop!
  "Stops `system`."
  [{:keys [store socket http]}]
  (http/stop! http)
  (socket/stop! socket)
  (store/stop! store))

(defn -main [& args]
  (let [[command & more] args]
    (case command
      "serve" (let [opts (cli/parse-opts more {:spec spec :restrict (keys spec)})]
                (when (and (:connect-url opts) (not (some-> (token-file opts) fs/exists?)))
                  (fail! "ts-broker: --connect-url needs a token file or the op-connect-token credential."))
                (let [system (try
                               (start! opts)
                               (catch clojure.lang.ExceptionInfo e
                                 (if (= ::store/schema (:type (ex-data e)))
                                   (fail! (str "ts-broker: " (ex-message e)))
                                   (throw e))))]
                  (binding [*out* *err*]
                    (println "ts-broker: decide on" (:socket opts) "and UI on 127.0.0.1:" (http/port (:http system))))
                  (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop! system)))
                  @(promise)))
      ("help" "--help" "-h" nil) (print usage)
      (fail! (str "ts-broker: unknown command " (pr-str command) "\n\n" usage)))))
