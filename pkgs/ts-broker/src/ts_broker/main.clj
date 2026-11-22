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
   [ts-broker.actions]
   [ts-broker.lib :as lib]
   [ts-broker.pages :as pages])
  (:import
   [java.time Instant]
   [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit]))

(def spec
  {:db                 {:desc    "SQLite database file"
                        :default "/var/lib/ts-broker/broker.db"}
   :socket             {:desc    "Unix socket for decide requests"
                        :default "/run/ts-broker/decide.sock"}
   :ui-socket          {:desc    "Unix socket of the UI, for ts-plug"
                        :default "/run/ts-broker/ui/ui.sock"}
   :connect-url        {:desc "1Password Connect URL for the catalog"}
   :connect-proxy      {:desc "Unix socket of the HTTP proxy that Connect requests go through"}
   :connect-token-file {:desc "File with the Connect token (default: the op-connect-token systemd credential)"}})

(def usage
  (str "Usage: ts-broker serve [options]\n\n"
       "Answers ts-egress decide requests on a unix socket and serves the\n"
       "approval UI on another.\n\nOptions:\n"
       (cli/format-opts {:spec spec :order [:db :socket :ui-socket :connect-url :connect-proxy :connect-token-file]})
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

(defn- refresh-titles!
  "Updates the stored 1Password titles of every credential from Connect."
  [{:keys [store connect]}]
  (try
    (store/write! store {:command :credential/titles
                         :titles  (connect/titles connect (store/read store store/credentials))})
    (catch Exception e
      (binding [*out* *err*]
        (println "ts-broker: title refresh failed:" (ex-message e))))))

(defn start!
  "Starts the store, the decide socket and the UI from parsed `opts`. With
  a Connect URL it also refreshes the credentials' 1Password titles at
  start and every 10 minutes. Returns the running system for [[stop!]]."
  [opts]
  (let [token (some-> (token-file opts) slurp str/trim)
        st (store/start! (:db opts) {})
        ctx {:store   st
             :clock   #(Instant/now)
             :connect (some-> (:connect-url opts) (connect/client token {:proxy-socket (:connect-proxy opts)}))}]
    {:store  st
     :titles (when (:connect ctx)
               (doto (ScheduledThreadPoolExecutor. 1)
                 (.scheduleWithFixedDelay #(refresh-titles! ctx) 0 10 TimeUnit/MINUTES)))
     :socket (socket/start! st (:socket opts) {})
     :http   (http/start! (merge (lib/routes 'ts-broker.pages 'ts-broker.actions)
                                 {:socket (:ui-socket opts) :ctx ctx :css pages/css :datastar-js (datastar-js)}))}))

(defn stop!
  "Stops `system`."
  [{:keys [store socket http ^ScheduledThreadPoolExecutor titles]}]
  (some-> titles .shutdownNow)
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
                    (println "ts-broker: decide on" (:socket opts) "and UI on" (:ui-socket opts)))
                  (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop! system)))
                  @(promise)))
      ("help" "--help" "-h" nil) (print usage)
      (fail! (str "ts-broker: unknown command " (pr-str command) "\n\n" usage)))))
