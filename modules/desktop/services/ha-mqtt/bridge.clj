;; Standalone script: the requested ha-mqtt directory is not a classpath root.
#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns ha-mqtt.bridge
  (:refer-clojure :exclude [run!])
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files Paths]
           [java.nio.file.attribute PosixFilePermissions]))

(def actions #{"speaker-mute" "speaker-unmute" "speaker-toggle" "speaker-get-mute" "shutdown"})

(defn log! [outcome]
  (binding [*out* *err*] (println (str "ha-mqtt: " outcome)) (flush)))

(defn start-child! [lifecycle argv options]
  (locking lifecycle
    (when (:stopping? @lifecycle)
      (throw (ex-info "stopping" {})))
    (let [child (process/process argv (merge {:in "" :err :discard} options))]
      (swap! lifecycle update :children conj child)
      child)))

(defn terminate! [child]
  (let [handle (.toHandle (:proc child))
        descendants (vec (iterator-seq (.iterator (.descendants handle))))]
    (doseq [descendant (reverse descendants)] (.destroyForcibly descendant))
    (.destroyForcibly handle)))

(defn stop-child! [lifecycle child]
  (terminate! child)
  (swap! lifecycle update :children disj child))

(defn stop! [lifecycle]
  (locking lifecycle
    (swap! lifecycle assoc :stopping? true)
    (doseq [child (:children @lifecycle)] (terminate! child))))

(defn run! [lifecycle argv timeout-ms]
  (let [child (start-child! lifecycle argv {:out :string})]
    (try
      (let [result (deref child timeout-ms ::timeout)]
        (when (or (= ::timeout result) (not= 0 (:exit result)))
          (throw (ex-info "subprocess failed" {})))
        (:out result))
      (finally (stop-child! lifecycle child)))))

(defn valid-prefix? [prefix]
  (and (string? prefix) (not (str/blank? prefix))
       (not (str/ends-with? prefix "/"))
       (not (re-find #"[+#\x00]" prefix))))

(defn command [prefix message]
  (let [{:keys [topic packet]} message
        bytes (get-in packet [:payload :data])
        payload (:payload message)]
    (when (and (= topic (str prefix "/command")) (= topic (:topic packet))
               (= "publish" (:cmd packet)) (false? (:retain packet))
               (false? (:dup packet)) (= 0 (:qos packet))
               (string? payload) (contains? actions payload)
               (= "Buffer" (get-in packet [:payload :type]))
               (= (mapv int payload) bytes))
      payload)))

(defn password [path]
  (let [file (.toRealPath (Paths/get path (make-array String 0)) (make-array java.nio.file.LinkOption 0))
        permissions (Files/getPosixFilePermissions file (make-array java.nio.file.LinkOption 0))]
    (when (or (str/starts-with? (str file) "/nix/store/")
              (not (#{"r--------" "rw-------"} (PosixFilePermissions/toString permissions)))
              (not= (System/getProperty "user.name")
                    (str (Files/getOwner file (make-array java.nio.file.LinkOption 0)))))
      (throw (ex-info "unsafe password file" {})))
    (let [value (str/replace (slurp (str file)) #"\r?\n$" "")]
      (when (or (empty? value) (re-find #"[\r\n\x00]" value))
        (throw (ex-info "invalid password file" {})))
      value)))

(defn mqtt-options! [config]
  (let [mqtt (:mqtt config)
        common (merge {:username (:username mqtt) :password (password (:passwordFile mqtt))
                       :hostname (:host mqtt) :port (:port mqtt)
                       :protocol (if (get-in mqtt [:tls :enable]) "mqtts" "mqtt")
                       :mqttVersion 5 :clean true :sessionExpiryInterval 0
                       :reconnectPeriod 0 :maximumReconnectTimes 0 :keepalive 30}
                      (when-let [ca (get-in mqtt [:tls :caFile])] {:ca ca}))
        dir (Files/createTempDirectory
             (Paths/get (or (System/getenv "TMPDIR") (System/getProperty "java.io.tmpdir"))
                        (make-array String 0))
             "ha-mqtt-" (make-array java.nio.file.attribute.FileAttribute 0))
        file (.resolve dir "mqttx.json")]
    (Files/setPosixFilePermissions dir (PosixFilePermissions/fromString "rwx------"))
    (Files/createFile file (into-array java.nio.file.attribute.FileAttribute
                                       [(PosixFilePermissions/asFileAttribute
                                         (PosixFilePermissions/fromString "rw-------"))]))
    (spit (str file) (json/generate-string {:sub common :pub common}))
    {:dir dir :file file}))

(defn mqtt-argv [options kind]
  ["mqttx-cli" kind "--load-options" (str (:file options))
   "--client-id" (str "ha-mqtt-" kind "-" (random-uuid))])

(defn publish-state!
  ([lifecycle config options]
   (publish-state! lifecycle config options true))
  ([lifecycle config options force?]
   ;; Serialize reads and publications so an older observation cannot arrive last.
   (locking options
     (let [state (str/trim (run! lifecycle ["speaker-get-mute"] 5000))]
       (when-not (#{"0" "1"} state) (throw (ex-info "invalid speaker state" {})))
       (when (or force? (not= state (:published-state @lifecycle)))
         ;; Remember only broker-acknowledged state; failed publications remain retryable.
         (run! lifecycle (into (mqtt-argv options "pub")
                               ["--topic" (str (:topicPrefix config) "/speaker/muted")
                                "--message" state "--qos" "1" "--retain"])
               5000)
         (swap! lifecycle assoc :published-state state)
         (log! "speaker-state-published"))))))

(defn watch-speaker! [lifecycle config options]
  (while (not (:stopping? @lifecycle))
    (try
      (let [child (start-child! lifecycle ["pactl" "subscribe"]
                                {:extra-env {"LC_ALL" "C"}})]
        (try
          (publish-state! lifecycle config options)
          (log! "speaker-monitor-started")
          (with-open [reader (io/reader (:out child))]
            (doseq [event (line-seq reader)
                    :when (re-find #"^Event '(?:new|change|remove)' on (?:sink|server) #" event)]
              (publish-state! lifecycle config options false)))
          (finally (stop-child! lifecycle child))))
      (catch Exception _ (log! "speaker-monitor-failed")))
    (when-not (:stopping? @lifecycle)
      (log! "speaker-monitor-restarting")
      (Thread/sleep 3000))))

(defn shutdown! [lifecycle config pending]
  (cond
    (not (get-in config [:shutdown :enable])) (log! "shutdown-disabled")
    (not (compare-and-set! pending false true)) (log! "shutdown-already-pending")
    :else
    (future
      (try
        (let [grace (get-in config [:shutdown :gracePeriodMs])
              capabilities (run! lifecycle ["dunstify" "--capabilities"] 5000)]
          (when-not (some #{"actions"} (str/split-lines capabilities))
            (throw (ex-info "notification actions unavailable" {})))
          (log! "shutdown-pending")
          (let [start (System/nanoTime)
                response (str/trim
                          (run! lifecycle
                                ["dunstify" "--block" "--urgency" "critical"
                                 "--appname" "ha-mqtt" "--timeout" (str grace)
                                 "--action" "cancel,Cancel shutdown"
                                 "Remote shutdown requested"
                                 (str "Shutdown after " (quot grace 1000)
                                      " seconds. Choose Cancel shutdown or dismiss this notification to cancel.")]
                                (+ grace 5000)))
                elapsed (/ (- (System/nanoTime) start) 1000000)]
            (cond
              (#{"cancel" "2" "3"} response) (log! "shutdown-cancelled")
              (and (= "1" response) (>= elapsed grace))
              (locking lifecycle
                (when (:stopping? @lifecycle) (throw (ex-info "stopping" {})))
                (if (get-in config [:shutdown :dryRun])
                  (log! "shutdown-dry-run")
                  (do (run! lifecycle ["systemctl" "poweroff"] 5000)
                      (log! "shutdown-poweroff-requested"))))
              :else (log! "shutdown-failed"))))
        (catch Exception _ (log! "shutdown-failed"))
        (finally (reset! pending false))))))

(defn dispatch! [lifecycle config options pending message]
  (if-let [action (command (:topicPrefix config) message)]
    (if (= "shutdown" action)
      (shutdown! lifecycle config pending)
      (try
        (when-not (= "speaker-get-mute" action) (run! lifecycle [action] 5000))
        (publish-state! lifecycle config options)
        (catch Exception _ (log! "speaker-action-or-publish-failed"))))
    (log! "command-rejected")))

(defn subscribe! [lifecycle config options pending]
  (let [child (start-child! lifecycle
                            (into (mqtt-argv options "sub")
                                  ["--topic" (str (:topicPrefix config) "/command")
                                   "--qos" "0" "--retain-as-published" "true"
                                   "--retain-handling" "2" "--maximum-packet-size" "1024"
                                   "--output-mode" "clean"])
                            {})]
    (try
      ;; Parse successive JSON values, not lines or human-oriented MQTTX logs.
      (try (publish-state! lifecycle config options)
           (catch Exception _ (log! "speaker-refresh-failed")))
      (with-open [reader (io/reader (:out child))]
        (doseq [message (json/parsed-seq reader true)]
          (dispatch! lifecycle config options pending message)))
      (finally (stop-child! lifecycle child)))))

(defn main [config-path]
  (try
    (let [config (json/parse-string (slurp config-path) true)]
      (when-not (valid-prefix? (:topicPrefix config))
        (throw (ex-info "invalid topic prefix" {})))
      (let [lifecycle (atom {:stopping? false :children #{}})
            pending (atom false)
            options (mqtt-options! config)
            cleanup (fn []
                      (stop! lifecycle)
                      (Files/deleteIfExists (:file options))
                      (Files/deleteIfExists (:dir options)))
            hook (Thread. cleanup)]
        (.addShutdownHook (Runtime/getRuntime) hook)
        (try
          (future (watch-speaker! lifecycle config options))
          (while (not (:stopping? @lifecycle))
            (try (subscribe! lifecycle config options pending)
                 (catch Exception _ (log! "subscription-failed")))
            (when-not (:stopping? @lifecycle)
              (log! "subscription-restarting")
              (Thread/sleep 3000)))
          (finally (cleanup)))))
    (catch Exception _ (log! "configuration-or-runtime-failed") (System/exit 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (main (first *command-line-args*)))
