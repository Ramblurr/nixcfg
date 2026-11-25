(ns motd.collect
  "Collect live system state into the map shape that motd.render expects.

  Parsers are pure functions of raw text. The collect functions read files
  and run subprocesses at the boundary, and return nil when a source is
  absent."
  (:require
   [babashka.process :as process]
   [cheshire.core :as json]
   [clojure.string :as str]))

(defn- parse-num
  [s]
  (when-let [s (some-> s str/trim not-empty)]
    (parse-long s)))

(defn parse-os-release
  "Return the OS label, such as \"NixOS 25.05\", from os-release text."
  [text]
  (let [kv (into {}
                 (keep (fn [line]
                         (when-let [[_ k v] (re-matches #"([A-Z_]+)=\"?(.*?)\"?" line)]
                           [k v])))
                 (str/split-lines (or text "")))]
    (cond
      (and (kv "NAME") (kv "VERSION_ID")) (str (kv "NAME") " " (kv "VERSION_ID"))
      (kv "PRETTY_NAME")                  (kv "PRETTY_NAME")
      (kv "NAME")                         (kv "NAME")
      :else                               "Linux")))

(defn parse-uptime
  "Return whole seconds of uptime from /proc/uptime text."
  [text]
  (-> (str/trim text) (str/split #"\s+") first parse-double long))

(defn parse-loadavg
  "Return the 1, 5, and 15 minute load averages from /proc/loadavg text."
  [text]
  (mapv parse-double (take 3 (str/split (str/trim text) #"\s+"))))

(defn parse-cpuinfo
  "Return {:model :cores} from /proc/cpuinfo text."
  [text]
  (let [lines (str/split-lines text)
        field (fn [k]
                (some (fn [line]
                        (when-let [[_ v] (re-matches (re-pattern (str k "\\s*:\\s*(.*)")) line)]
                          v))
                      lines))]
    {:model (or (field "model name") (field "Hardware") (field "Model") "unknown")
     :cores (max 1 (count (filter #(re-matches #"processor\s*:.*" %) lines)))}))

(defn parse-meminfo
  "Return a map of /proc/meminfo keys to byte counts."
  [text]
  (into {}
        (keep (fn [line]
                (when-let [[_ k v unit] (re-matches #"(\S+):\s+(\d+)(?:\s+(kB))?" line)]
                  [k (cond-> (parse-long v) unit (* 1024))])))
        (str/split-lines text)))

(defn memory-state
  "Return the :memory map from parsed meminfo."
  [mem]
  (let [total (get mem "MemTotal" 0)]
    {:used-bytes  (- total (get mem "MemAvailable" (get mem "MemFree" 0)))
     :total-bytes total
     :swap-used   (- (get mem "SwapTotal" 0) (get mem "SwapFree" 0))
     :swap-total  (get mem "SwapTotal" 0)}))

(defn parse-zram-mm-stat
  "Return {:orig-bytes :compr-bytes} from a zram mm_stat line."
  [text]
  (let [[orig compr] (map parse-long (str/split (str/trim text) #"\s+"))]
    {:orig-bytes orig :compr-bytes compr}))

(defn parse-zram-algo
  "Return the selected algorithm from comp_algorithm text, such as \"lzo [zstd]\"."
  [text]
  (let [text (str/trim text)]
    (or (second (re-find #"\[(\S+)\]" text))
        (first (str/split text #"\s+")))))

(defn zswap-state
  "Return the :zswap map, or nil when zswap is disabled."
  [{:keys [enabled max-pool-percent compressor]} mem]
  (when (= "Y" (some-> enabled str/trim))
    {:pool-bytes     (get mem "Zswap" 0)
     :stored-bytes   (get mem "Zswapped" 0)
     :max-pool-bytes (when-let [pct (parse-num max-pool-percent)]
                       (quot (* pct (get mem "MemTotal" 0)) 100))
     :algo           (some-> compressor str/trim)}))

(def ^:private skipped-iface-prefixes
  ["veth" "docker" "br-" "virbr" "podman" "cni"])

(defn- skip-iface?
  [ifname]
  (or (= "lo" ifname)
      (some #(str/starts-with? ifname %) skipped-iface-prefixes)))

(defn parse-ip-json
  "Return :network entries from `ip -j -4 addr show up` output."
  [text]
  (vec
   (for [{:keys [ifname operstate flags addr_info]} (json/parse-string text true)
         :when (not (skip-iface? ifname))
         {:keys [local prefixlen]}                 addr_info]
     {:iface ifname
      :addr  (if (and prefixlen (not= 32 prefixlen)) (str local "/" prefixlen) local)
      :kind  (if (str/starts-with? ifname "tailscale") :tailscale :lan)
      :state (if (or (= "UP" operstate) (some #{"LOWER_UP"} flags)) :up :down)})))

(def ^:private skipped-mount-prefixes
  ["/dev" "/proc" "/sys" "/run" "/boot" "/nix/store"])

(def ^:private skipped-fstypes
  #{"overlay" "efivarfs" "nsfs" "devtmpfs" "squashfs" "ramfs" "autofs"})

(defn- under?
  [mount prefix]
  (or (= mount prefix) (str/starts-with? mount (str prefix "/"))))

(defn real-mount?
  "Return true when a df row is a mount worth showing by default."
  [{:keys [mount fs]}]
  (not (or (some #(under? mount %) skipped-mount-prefixes)
           (contains? skipped-fstypes fs)
           (str/starts-with? fs "fuse")
           (and (= "tmpfs" fs) (not= "/" mount)))))

(defn parse-df-output
  "Return df rows from `df -B1 --output=source,target,fstype,size,used,avail` output."
  [text]
  (vec
   (keep (fn [line]
           (let [tokens (str/split (str/trim line) #"\s+")
                 n      (count tokens)]
             (when (>= n 6)
               (let [[size used avail] (map parse-long (subvec tokens (- n 3)))]
                 (when (and size used avail)
                   {:source      (first tokens)
                    :mount       (str/join " " (subvec tokens 1 (- n 4)))
                    :fs          (nth tokens (- n 4))
                    :total-bytes size
                    :used-bytes  used
                    :avail-bytes avail})))))
         (rest (str/split-lines text)))))

(defn parse-zpool-list
  "Return pool name to health keyword from `zpool list -H -o name,health` output."
  [text]
  (into {}
        (keep (fn [line]
                (let [[pool health] (str/split (str/trim line) #"\s+")]
                  (when (and pool health)
                    [pool (case health
                            "ONLINE"   :ok
                            "DEGRADED" :degraded
                            :faulted)]))))
        (str/split-lines (or text ""))))

(defn disk-status
  "Return the health of a df row, using pool health for ZFS datasets."
  [pools {:keys [fs source]}]
  (or (when (= "zfs" fs)
        (get pools (first (str/split source #"/"))))
      :ok))

(defn select-disks
  "Return :disks entries from df rows, filtered to `wanted` mounts when non-nil."
  [rows pools wanted]
  (let [by-mount (into {} (map (juxt :mount identity)) rows)
        chosen   (if (some? wanted)
                   (keep by-mount wanted)
                   (filter real-mount? rows))]
    (mapv (fn [row]
            (-> row
                (select-keys [:mount :fs :used-bytes :avail-bytes :total-bytes])
                (assoc :status (disk-status pools row))))
          chosen)))

(defn parse-systemctl-show
  "Return property maps from `systemctl show` output, one per unit."
  [text]
  (->> (str/split (or text "") #"\n\s*\n")
       (map (fn [block]
              (into {}
                    (keep #(let [[k v] (str/split % #"=" 2)]
                             (when (and v (seq k)) [k v])))
                    (str/split-lines block))))
       (filter #(get % "Id"))
       vec))

(defn parse-failed-units
  "Return unit names from `systemctl list-units --state=failed --plain --no-legend` output."
  [text]
  (vec (keep #(first (str/split (str/trim %) #"\s+" 2))
             (remove str/blank? (str/split-lines (or text ""))))))

(def ^:private active-states
  #{:active :failed :inactive :activating :reloading})

(defn- unit-kind
  [unit-name]
  (cond
    (str/ends-with? unit-name ".service") :service
    (str/ends-with? unit-name ".timer")   :timer
    :else                                 :unit))

(defn- realtime-sec
  "Return epoch seconds from \"@<sec>\" or a microsecond count."
  [v]
  (cond
    (str/blank? v)            nil
    (str/starts-with? v "@")  (parse-num (subs v 1))
    (re-matches #"\d+" v)     (quot (parse-long v) 1000000)
    :else                     nil))

(defn unit-state
  "Return a unit entry from systemctl properties, uptime seconds, and epoch seconds."
  [props uptime-sec now-sec]
  (let [unit-name (get props "Id")
        active    (keyword (get props "ActiveState"))
        mono-us   (parse-num (get props "StateChangeTimestampMonotonic"))
        next-sec  (some-> (realtime-sec (get props "NextElapseUSecRealtime"))
                          (- now-sec)
                          (max 0))]
    (cond-> {:name   unit-name
             :kind   (unit-kind unit-name)
             :active (if (contains? active-states active) active :inactive)
             :sub    (get props "SubState")}
      (pos? (or mono-us 0)) (assoc :since-sec (max 0 (- uptime-sec (quot mono-us 1000000))))
      next-sec              (assoc :next-sec next-sec))))

(defn- read-file
  "Read a file through a stream. `slurp` of a path fails on procfs in Babashka."
  [path]
  (slurp (java.io.FileInputStream. (str path))))

(defn- slurp-safe
  [path]
  (try (read-file path) (catch Exception _ nil)))

(defn- run
  "Run a command and return its stdout, or nil when it fails or is absent."
  [& cmd]
  (try
    (let [{:keys [exit out]} (process/sh cmd)]
      (when (zero? exit) out))
    (catch Exception _ nil)))

(def ^:private show-properties
  "--property=Id,ActiveState,SubState,StateChangeTimestampMonotonic,NextElapseUSecRealtime")

(defn- systemctl-show
  [unit-names]
  (when (seq unit-names)
    (parse-systemctl-show
     (apply run "systemctl" "show" show-properties "--timestamp=unix" "--no-pager" unit-names))))

(defn- collect-units
  [curated-names uptime-sec now-sec failed-out]
  (let [curated      (mapv #(unit-state % uptime-sec now-sec) (systemctl-show curated-names))
        failed-names (parse-failed-units failed-out)
        by-name      (into {} (map (juxt :name identity)) curated)
        extra        (into {}
                           (map (juxt :name identity))
                           (map #(unit-state % uptime-sec now-sec)
                                (systemctl-show (remove by-name failed-names))))]
    {:curated curated
     :failed  (vec (keep #(or (by-name %) (extra %)) failed-names))}))

(defn- collect-zram
  []
  (when-let [dirs (seq (filter #(re-matches #"zram\d+" (.getName ^java.io.File %))
                               (.listFiles (java.io.File. "/sys/block"))))]
    (let [devs (keep (fn [dir]
                       (let [p      (str dir "/")
                             total  (parse-num (slurp-safe (str p "disksize")))
                             mm     (slurp-safe (str p "mm_stat"))]
                         (when (and mm (pos? (or total 0)))
                           (assoc (parse-zram-mm-stat mm)
                                  :total-bytes total
                                  :algo        (some-> (slurp-safe (str p "comp_algorithm")) parse-zram-algo)))))
                     dirs)]
      (when (seq devs)
        {:orig-bytes  (reduce + (map :orig-bytes devs))
         :compr-bytes (reduce + (map :compr-bytes devs))
         :total-bytes (reduce + (map :total-bytes devs))
         :algo        (:algo (first devs))}))))

(defn- read-hostname
  []
  (or (some-> (slurp-safe "/etc/hostname") str/trim not-empty)
      (.getHostName (java.net.InetAddress/getLocalHost))))

(defn collect-state
  "Return live system state for config {:role :units :disks}."
  [{:keys [role units disks]}]
  (let [ip-f     (future (run "ip" "-j" "-4" "addr" "show" "up"))
        df-f     (future (run "df" "-B1" "--output=source,target,fstype,size,used,avail"))
        zpool-f  (future (run "zpool" "list" "-H" "-o" "name,health"))
        failed-f (future (run "systemctl" "list-units" "--state=failed" "--plain" "--no-legend" "--no-pager"))
        uptime   (parse-uptime (read-file "/proc/uptime"))
        now-sec  (quot (System/currentTimeMillis) 1000)
        units-f  (future (collect-units units uptime now-sec @failed-f))
        cpuinfo  (parse-cpuinfo (read-file "/proc/cpuinfo"))
        load     (parse-loadavg (read-file "/proc/loadavg"))
        mem      (parse-meminfo (read-file "/proc/meminfo"))
        zswap-p  "/sys/module/zswap/parameters/"]
    {:host    {:hostname   (read-hostname)
               :role       (or role "host")
               :os         (parse-os-release (slurp-safe "/etc/os-release"))
               :kernel     (str/trim (read-file "/proc/sys/kernel/osrelease"))
               :arch       (or (some-> (slurp-safe "/proc/sys/kernel/arch") str/trim)
                               (System/getProperty "os.arch"))
               :uptime-sec uptime}
     :network (or (some-> @ip-f parse-ip-json) [])
     :cpu     (assoc cpuinfo
                     :load      load
                     :usage-pct (min 100 (long (Math/round (* 100.0 (/ (first load) (max 1 (:cores cpuinfo))))))))
     :memory  (memory-state mem)
     :zram    (collect-zram)
     :zswap   (zswap-state {:enabled          (slurp-safe (str zswap-p "enabled"))
                            :max-pool-percent (slurp-safe (str zswap-p "max_pool_percent"))
                            :compressor       (slurp-safe (str zswap-p "compressor"))}
                           mem)
     :disks   (select-disks (some-> @df-f parse-df-output)
                            (some-> @zpool-f parse-zpool-list)
                            disks)
     :units   @units-f}))
