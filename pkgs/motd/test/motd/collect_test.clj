(ns motd.collect-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [motd.collect :as collect]
   [motd.render :as render]))

(deftest os-release-prefers-name-and-version
  (is (= "NixOS 26.11"
         (collect/parse-os-release
          "NAME=NixOS\nVERSION_ID=\"26.11\"\nPRETTY_NAME=\"NixOS 26.11 (Zokor)\"\n")))
  (is (= "Debian GNU/Linux" (collect/parse-os-release "PRETTY_NAME=\"Debian GNU/Linux\"\n")))
  (is (= "Linux" (collect/parse-os-release nil))))

(deftest proc-uptime-and-loadavg
  (is (= 143188 (collect/parse-uptime "143188.21 3005543.34\n")))
  (is (= [1.97 1.56 2.57] (collect/parse-loadavg "1.97 1.56 2.57 2/4493 2082726\n"))))

(deftest cpuinfo-model-and-cores
  (let [text (str/join "\n" ["processor\t: 0"
                             "model name\t: AMD Ryzen 9 7950X3D 16-Core Processor"
                             ""
                             "processor\t: 1"
                             "model name\t: AMD Ryzen 9 7950X3D 16-Core Processor"])]
    (is (= {:model "AMD Ryzen 9 7950X3D 16-Core Processor" :cores 2}
           (collect/parse-cpuinfo text)))))

(def ^:private meminfo-text
  (str/join "\n" ["MemTotal:       65551552 kB"
                  "MemFree:         1000000 kB"
                  "MemAvailable:   22445096 kB"
                  "SwapTotal:      75481084 kB"
                  "SwapFree:       40945076 kB"
                  "Zswap:           3173360 kB"
                  "Zswapped:       15110092 kB"
                  "HugePages_Total:       0"]))

(deftest meminfo-to-memory-state
  (let [mem (collect/parse-meminfo meminfo-text)]
    (is (= 0 (get mem "HugePages_Total")))
    (is (= {:used-bytes  (* 1024 (- 65551552 22445096))
            :total-bytes (* 1024 65551552)
            :swap-used   (* 1024 (- 75481084 40945076))
            :swap-total  (* 1024 75481084)}
           (collect/memory-state mem)))))

(deftest zram-parsers
  (is (= {:orig-bytes 1610612736 :compr-bytes 503316480}
         (collect/parse-zram-mm-stat "1610612736 503316480 520093696 0 520093696 12 0 0 0\n")))
  (is (= "zstd" (collect/parse-zram-algo "lzo lzo-rle lz4 [zstd]\n")))
  (is (= "zstd" (collect/parse-zram-algo "zstd\n"))))

(deftest zswap-absent-and-present
  (let [mem (collect/parse-meminfo meminfo-text)]
    (is (nil? (collect/zswap-state {:enabled "N\n"} mem)))
    (is (nil? (collect/zswap-state {:enabled nil} mem)))
    (is (= {:pool-bytes     (* 1024 3173360)
            :stored-bytes   (* 1024 15110092)
            :max-pool-bytes (quot (* 25 1024 65551552) 100)
            :algo           "zstd"}
           (collect/zswap-state {:enabled "Y\n" :max-pool-percent "25\n" :compressor "zstd\n"} mem)))))

(def ^:private ip-json
  (str "[{\"ifname\":\"lo\",\"flags\":[\"LOOPBACK\",\"UP\",\"LOWER_UP\"],\"operstate\":\"UNKNOWN\","
       "\"addr_info\":[{\"local\":\"127.0.0.1\",\"prefixlen\":8}]},"
       "{\"ifname\":\"prim\",\"flags\":[\"UP\",\"LOWER_UP\"],\"operstate\":\"UP\","
       "\"addr_info\":[{\"local\":\"10.9.4.3\",\"prefixlen\":22}]},"
       "{\"ifname\":\"veth12\",\"flags\":[\"UP\",\"LOWER_UP\"],\"operstate\":\"UP\","
       "\"addr_info\":[{\"local\":\"10.88.0.1\",\"prefixlen\":16}]},"
       "{\"ifname\":\"eth1\",\"flags\":[\"UP\",\"NO-CARRIER\"],\"operstate\":\"DOWN\","
       "\"addr_info\":[{\"local\":\"10.1.0.2\",\"prefixlen\":24}]},"
       "{\"ifname\":\"tailscale0\",\"flags\":[\"UP\",\"LOWER_UP\"],\"operstate\":\"UNKNOWN\","
       "\"addr_info\":[{\"local\":\"100.93.18.79\",\"prefixlen\":32}]}]"))

(deftest ip-json-skips-virtual-ifaces
  (is (= [{:iface "prim"       :addr "10.9.4.3/22"  :kind :lan       :state :up}
          {:iface "eth1"       :addr "10.1.0.2/24"  :kind :lan       :state :down}
          {:iface "tailscale0" :addr "100.93.18.79" :kind :tailscale :state :up}]
         (collect/parse-ip-json ip-json))))

(def ^:private df-text
  (str/join "\n" ["Filesystem  Mounted on  Type  1B-blocks  Used  Avail"
                  "rpool/root          /                zfs      1000 400 600"
                  "rpool/persist       /persist         zfs      2000 500 1500"
                  "tank/media          /tank/my media   zfs      3000 900 2100"
                  "/dev/nvme0n1p1      /boot            vfat     500  100 400"
                  "tmpfs               /run             tmpfs    800  10  790"
                  "tmpfs               /tmp             tmpfs    800  10  790"
                  "/dev/sda1           /srv/data        ext4     4000 1000 3000"
                  "efivarfs            /sys/firmware/efi/efivars efivarfs 10 1 9"
                  "portal              /usr/share/icons fuse     10 1 9"]))

(deftest df-output-parses-mounts-with-spaces
  (let [rows (collect/parse-df-output df-text)]
    (is (= 9 (count rows)))
    (is (= {:source "tank/media" :mount "/tank/my media" :fs "zfs"
            :total-bytes 3000 :used-bytes 900 :avail-bytes 2100}
           (nth rows 2)))))

(deftest default-disks-keep-real-mounts-and-map-pool-health
  (let [rows  (collect/parse-df-output df-text)
        pools (collect/parse-zpool-list "rpool\tONLINE\ntank\tDEGRADED\n")]
    (is (= [["/" :ok] ["/persist" :ok] ["/tank/my media" :degraded] ["/srv/data" :ok]]
           (map (juxt :mount :status) (collect/select-disks rows pools nil))))))

(deftest configured-disks-keep-config-order
  (let [rows (collect/parse-df-output df-text)]
    (is (= ["/srv/data" "/"]
           (map :mount (collect/select-disks rows {} ["/srv/data" "/missing" "/"]))))))

(deftest zpool-health-mapping
  (is (= {"a" :ok "b" :degraded "c" :faulted "d" :faulted}
         (collect/parse-zpool-list "a\tONLINE\nb\tDEGRADED\nc\tFAULTED\nd\tUNAVAIL\n")))
  (is (= {} (collect/parse-zpool-list nil)))
  (testing "ZFS datasets on an unknown pool and non-ZFS mounts are ok"
    (is (= :ok (collect/disk-status {} {:fs "zfs" :source "gone/x"})))
    (is (= :ok (collect/disk-status {"tank" :faulted} {:fs "ext4" :source "/dev/sda1"})))))

(def ^:private show-text
  (str/join "\n" ["Id=sshd.service"
                  "ActiveState=active"
                  "SubState=running"
                  "StateChangeTimestampMonotonic=31570489"
                  "ActiveEnterTimestampMonotonic=31570489"
                  ""
                  "Id=nix-gc.timer"
                  "ActiveState=active"
                  "SubState=waiting"
                  "StateChangeTimestampMonotonic=142102794775"
                  "NextElapseUSecRealtime=@1795647600"
                  ""
                  "Id=nonexist.service"
                  "ActiveState=inactive"
                  "SubState=dead"
                  "StateChangeTimestampMonotonic=0"
                  ""]))

(deftest systemctl-show-to-units
  (let [units (mapv #(collect/unit-state % 143188 1795640400)
                    (collect/parse-systemctl-show show-text))]
    (is (= [{:name "sshd.service" :kind :service :active :active :sub "running" :since-sec 143157}
            {:name "nix-gc.timer" :kind :timer :active :active :sub "waiting" :since-sec 1086 :next-sec 7200}
            {:name "nonexist.service" :kind :service :active :inactive :sub "dead"}]
           units))
    (testing "An elapsed timer does not get a negative next-sec"
      (is (= 0 (:next-sec (collect/unit-state {"Id" "x.timer" "ActiveState" "active"
                                               "NextElapseUSecRealtime" "@100"}
                                              10 200)))))
    (testing "Unknown active states render as inactive"
      (is (= :inactive (:active (collect/unit-state {"Id" "x.service" "ActiveState" "deactivating"}
                                                    10 200)))))))

(deftest failed-units-parse-first-column
  (is (= ["zrepl.service" "smartd.service"]
         (collect/parse-failed-units
          (str "zrepl.service  loaded failed failed zrepl daemon\n"
               "smartd.service loaded failed failed S.M.A.R.T. Daemon\n"))))
  (is (= [] (collect/parse-failed-units ""))))

(deftest collected-state-renders
  (testing "Live state from this machine has the shape render expects"
    (let [state (collect/collect-state {:role "host" :units ["sshd.service"] :disks ["/"]})
          out   (render/render state {:color? false})]
      (is (string? (get-in state [:host :hostname])))
      (is (pos? (get-in state [:cpu :cores])))
      (is (str/includes? out (get-in state [:host :hostname]))))))
