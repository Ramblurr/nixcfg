#!/usr/bin/env bb
(ns scripts.signal-t3-bridge
  "Forwards messages from one Signal group to one T3 thread."
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as process]
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   java.time.Instant))

(def spec
  {:account     {:desc    "Signal account number that signal-cli uses"
                 :ref     "<number>"
                 :require true}
   :group-id    {:desc    "Base64 Signal group ID to watch"
                 :ref     "<id>"
                 :require true}
   :thread      {:desc    "T3 thread ID that receives the messages"
                 :ref     "<id>"
                 :require true}
   :signal-cli  {:desc    "signal-cli command"
                 :ref     "<cmd>"
                 :default "signal-cli"}
   :t3          {:desc    "t3 command"
                 :ref     "<cmd>"
                 :default "t3"}
   :ignore-own  {:desc   "Skip messages that this Signal account sent from other devices"
                 :coerce :boolean}
   :retries     {:desc    "Number of retries when t3 send fails"
                 :ref     "<n>"
                 :coerce  :long
                 :default 5}
   :dry-run     {:desc   "Print messages to stdout instead of sending them to T3"
                 :coerce :boolean}
   :help        {:desc   "Show this help"
                 :alias  :h
                 :coerce :boolean}})

(def usage
  (str "Usage: signal-t3-bridge.clj [OPTIONS]\n\n"
       "Watch one Signal group with signal-cli and send each new message to a T3 thread.\n"
       "The process runs until signal-cli exits. It then exits nonzero, so a\n"
       "supervisor such as systemd can restart it.\n\n"
       "signal-cli runs as a daemon on the D-Bus session bus. Other programs\n"
       "use the account through it, for example:\n"
       "  signal-cli --dbus send -g <id> -m <text>\n\n"
       "Options:\n"
       (cli/format-opts {:spec  spec
                         :order [:account :group-id :thread :signal-cli :t3
                                 :ignore-own :retries :dry-run :help]})
       "\n\nExamples:\n"
       "  signal-t3-bridge.clj --account <number> --group-id <id> --thread <id>\n"
       "  signal-t3-bridge.clj --account <number> --group-id <id> --thread x --dry-run\n"))

(defn log [& xs]
  (binding [*out* *err*]
    (println (str/join " " xs))))

(defn attachment-path [{:keys [id]}]
  (str (fs/path (or (System/getenv "XDG_DATA_HOME")
                    (fs/path (fs/home) ".local" "share"))
                "signal-cli" "attachments" id)))

(defn group-message
  "Returns the group message in `event`, or nil when `event` has none.
  Covers messages from other members and messages that this account sent from
  another device."
  [{:keys [envelope account]}]
  (let [{:keys [dataMessage syncMessage]} envelope
        sent                              (:sentMessage syncMessage)
        message                           (or dataMessage sent)]
    (when (:groupInfo message)
      {:group-id    (get-in message [:groupInfo :groupId])
       :group-name  (get-in message [:groupInfo :groupName])
       :own?        (some? sent)
       :sender      (if sent "me" (or (not-empty (:sourceName envelope)) "unknown"))
       :number      (if sent account (:sourceNumber envelope))
       :timestamp   (:timestamp message)
       :text        (:message message)
       :attachments (:attachments message)})))

(defn ->t3-message [{:keys [group-name sender number timestamp text attachments]}]
  (str "Signal message in group \"" group-name "\" from " sender
       (when number (str " (" number ")"))
       " at " (Instant/ofEpochMilli timestamp) ":\n\n"
       (or text "")
       (when (seq attachments)
         (str "\n\nAttachments:\n"
              (str/join "\n"
                        (for [a attachments]
                          (str "- " (or (:filename a) (:id a))
                               " (" (:contentType a) "): " (attachment-path a))))))))

(defn forward? [opts {:keys [group-id own? text attachments]}]
  (and (= group-id (:group-id opts))
       (not (and own? (:ignore-own opts)))
       (or (not (str/blank? text)) (seq attachments))))

(defn send-to-t3! [{:keys [t3 thread retries]} message]
  (loop [attempt 0]
    (let [{:keys [exit err]} (process/shell {:in       message
                                             :out      :string
                                             :err      :string
                                             :continue true}
                                            t3 "thread" "send" thread "-")]
      (cond
        (zero? exit)        true
        (< attempt retries) (do (log "t3 thread send failed, retrying:" (str/trim err))
                                (Thread/sleep (* 5000 (inc attempt)))
                                (recur (inc attempt)))
        :else               (do (log "t3 thread send failed, message dropped:" (str/trim err))
                                (log message)
                                false)))))

(defn handle-line! [opts line]
  (when-let [message (try
                       (group-message (json/parse-string line true))
                       (catch Exception e
                         (log "Cannot parse signal-cli output:" (ex-message e))
                         nil))]
    (when (forward? opts message)
      (let [text (->t3-message message)]
        (if (:dry-run opts)
          (println text "\n")
          (send-to-t3! opts text))))))

(defn watch! [{:keys [signal-cli account] :as opts}]
  (log "Watching group" (:group-id opts) "for" account)
  (let [proc (process/process {:out :stream :err :inherit}
                              signal-cli "-a" account "-o" "json"
                              "daemon" "--dbus")]
    (with-open [reader (io/reader (:out proc))]
      (doseq [line (line-seq reader)
              :when (not (str/blank? line))]
        (handle-line! opts line)))
    (let [exit (:exit @proc)]
      (log "signal-cli exited with status" exit)
      (if (zero? exit) 1 exit))))

(defn -main [& args]
  (if (some #{"-h" "--help"} args)
    (println usage)
    (let [opts (cli/parse-opts args {:spec     spec
                                     :restrict true
                                     :error-fn (fn [{:keys [msg]}]
                                                 (log (str "Error: " msg "\n"))
                                                 (log usage)
                                                 (System/exit 2))})]
      (System/exit (watch! opts)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
