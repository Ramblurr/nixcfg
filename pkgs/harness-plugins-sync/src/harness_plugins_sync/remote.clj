(ns harness-plugins-sync.remote
  "Fans out `harness-plugins-sync` commands to remote peer hosts over SSH."
  (:require
   [babashka.process :as proc]
   [clojure.data.json :as json]
   [clojure.string :as str])
  (:import
   java.net.InetAddress))

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

(defn run-remote
  "Runs `subcmd` on remote `host` via SSH and returns a structured peer result."
  [ssh-bin host subcmd input]
  (let [remote-argv (forward-flags subcmd input)
        ssh-args    (into [ssh-bin "-o" "BatchMode=yes" "-o" "ConnectTimeout=5" host]
                          remote-argv)
        res         (proc/sh ssh-args {:out :string :err :string :continue true})]
    (cond
      (zero? (:exit res))
      (try
        {:host   host
         :status :ok
         :result (json/read-str (:out res) :key-fn keyword)}
        (catch Exception e
          {:host    host
           :status  :error
           :message (str "invalid JSON from remote host: " (ex-message e))}))

      (= 255 (:exit res))
      {:host    host
       :status  :unreachable
       :message (let [msg (str/trim (:err res))]
                  (if (str/blank? msg) "SSH connection failed" msg))}

      :else
      {:host    host
       :status  :error
       :message (let [msg (str/trim (if (str/blank? (:err res)) (:out res) (:err res)))]
                  (if (str/blank? msg) (str "remote exited with status " (:exit res)) msg))})))
