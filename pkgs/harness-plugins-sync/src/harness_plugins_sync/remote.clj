(ns harness-plugins-sync.remote
  "Fans out `harness-plugins-sync` commands to remote peer hosts over SSH."
  (:require
   [babashka.process :as proc]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   java.net.InetAddress
   java.util.concurrent.TimeUnit))

(def default-timeout-ms
  "Default per-peer SSH wall-clock timeout in milliseconds."
  15000)

(def ^:private tailscale-auth-pattern
  #"(https://login\.tailscale\.com/\S+)")

(defn extract-auth-url
  "Extracts a Tailscale SSH auth URL from `s`, or returns nil."
  [s]
  (when-let [[_ url] (re-find tailscale-auth-pattern (str s))]
    (str/replace url #"[.,;:)\]]+$" "")))

(defn local-hostname
  "Returns the short local hostname."
  []
  (try
    (-> (InetAddress/getLocalHost)
        .getHostName
        (str/split #"\.")
        first)
    (catch Exception _
      (or (System/getenv "HOSTNAME") "localhost"))))

(defn local-username
  "Returns the current OS username."
  []
  (or (System/getenv "USER") (System/getProperty "user.name") ""))

(defn- peer-host [spec]
  (let [s (str spec)
        i (str/index-of s "@")]
    (if i (subs s (inc i)) s)))

(defn- peer-user [spec]
  (let [s (str spec)
        i (str/index-of s "@")]
    (when i (subs s 0 i))))

(defn- local-peer? [spec self-host self-user]
  (and (= (peer-host spec) self-host)
       (let [u (peer-user spec)]
         (or (nil? u) (= u self-user)))))

(defn target-plan
  "Computes `{:local? bool :remotes [host ...]}` from `peers`, `all?`, `hosts`,
  and `self-host` (matching optional `user@host` peers against `self-user`)."
  ([peers all? hosts self-host]
   (target-plan peers all? hosts self-host (local-username)))
  ([peers all? hosts self-host self-user]
   (let [explicit (vec (distinct hosts))]
     (cond
       (seq explicit)
       {:local?  (boolean (some #(local-peer? % self-host self-user) explicit))
        :remotes (vec (remove #(local-peer? % self-host self-user) explicit))}

       all?
       {:local?  true
        :remotes (vec (distinct (remove #(local-peer? % self-host self-user) peers)))}

       :else
       {:local? true :remotes []}))))

(defn forward-flags
  "Builds CLI arguments forwarded to a remote `harness-plugins-sync` invocation."
  [subcmd {:keys [pack profile force dry-run]}]
  (cond-> ["harness-plugins-sync" subcmd "--json"]
    (seq pack)    (into (mapcat #(vector "--pack" %) pack))
    (seq profile) (into (mapcat #(vector "--profile" %) profile))
    force         (conj "--force")
    dry-run       (conj "--dry-run")))

(defn- read-stream-async [stream on-line]
  (let [lines (atom [])
        t     (Thread.
               ^Runnable
               (fn []
                 (try
                   (with-open [r (io/reader stream)]
                     (loop []
                       (when-let [line (.readLine r)]
                         (swap! lines conj line)
                         (when on-line (on-line line))
                         (recur))))
                   (catch Exception _ nil))))]
    (.setDaemon t true)
    (.start t)
    {:thread t :lines lines}))

(defn run-remote
  "Runs `subcmd` on remote `host` via SSH and returns a structured peer result.

  Fails fast when Tailscale SSH emits an interactive auth URL on stderr or
  stdout, killing the stalled SSH process and returning `:status :auth-required`
  with `:auth-url`. Enforces `timeout-ms` (default 15000 ms)."
  ([ssh-bin host subcmd input]
   (run-remote ssh-bin host subcmd input default-timeout-ms))
  ([ssh-bin host subcmd input timeout-ms]
   (let [remote-argv (forward-flags subcmd input)
         ssh-args    (into [ssh-bin "-o" "BatchMode=yes" "-o" "ConnectTimeout=5" host]
                           remote-argv)
         p           (proc/process ssh-args {:out :stream :err :stream})
         ^Process jp (:proc p)
         auth-url    (promise)
         on-line     (fn [line]
                       (when-let [url (extract-auth-url line)]
                         (deliver auth-url url)
                         (.destroyForcibly jp)))
         out-reader  (read-stream-async (:out p) on-line)
         err-reader  (read-stream-async (:err p) on-line)
         finished?   (.waitFor jp (long timeout-ms) TimeUnit/MILLISECONDS)]
     (when-not finished?
       (.destroyForcibly jp)
       (.waitFor jp 2 TimeUnit/SECONDS))
     (.join ^Thread (:thread out-reader) 1000)
     (.join ^Thread (:thread err-reader) 1000)
     (let [out-str (str/join "\n" @(:lines out-reader))
           err-str (str/join "\n" @(:lines err-reader))
           url     (when (realized? auth-url) @auth-url)]
       (cond
         url
         {:host     host
          :status   :auth-required
          :auth-url url
          :message  (str "Tailscale SSH auth required: " url)}

         (not finished?)
         {:host    host
          :status  :unreachable
          :message (str "SSH timed out after " (max 1 (quot timeout-ms 1000)) "s")}

         (zero? (.exitValue jp))
         (try
           {:host   host
            :status :ok
            :result (json/read-str out-str :key-fn keyword)}
           (catch Exception e
             {:host    host
              :status  :error
              :message (str "invalid JSON from remote host: " (ex-message e))}))

         (= 255 (.exitValue jp))
         {:host    host
          :status  :unreachable
          :message (let [msg (str/trim err-str)]
                     (if (str/blank? msg) "SSH connection failed" msg))}

         :else
         {:host    host
          :status  :error
          :message (let [msg (str/trim (if (str/blank? err-str) out-str err-str))]
                     (if (str/blank? msg) (str "remote exited with status " (.exitValue jp)) msg))})))))
