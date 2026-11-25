(ns motd.dummy
  "Dummy state fixtures for previewing the MOTD renderer.")

(def host-state
  "Representative NixOS physical host with mixed unit health and extra failures."
  {:host    {:hostname   "dewey"
             :role       "host"
             :os         "NixOS 25.05"
             :kernel     "6.12.19"
             :arch       "x86_64"
             :uptime-sec 1420380}
   :network [{:iface "enp3s0"     :addr "10.10.20.4/24"  :kind :lan       :state :up}
             {:iface "tailscale0" :addr "100.64.0.12"    :kind :tailscale :state :up}]
   :cpu     {:model     "AMD Ryzen 9 5950X"
             :cores     32
             :load      [0.42 0.38 0.31]
             :usage-pct 12}
   :memory  {:used-bytes  18468359372
             :total-bytes 68719476736
             :swap-used   0
             :swap-total  8589934592}
   :zram    {:orig-bytes  1610612736
             :compr-bytes 503316480
             :total-bytes 8589934592
             :algo        "zstd"}
   :zswap   {:pool-bytes     1181116006
             :stored-bytes   3328599654
             :max-pool-bytes 6871947673
             :algo           "zstd"}
   :disks   [{:mount       "/"
              :fs          "zfs"
              :used-bytes  22978075033
              :avail-bytes 105870943846
              :total-bytes 128849018880
              :status      :ok}
             {:mount       "/persist"
              :fs          "zfs"
              :used-bytes  54116587929
              :avail-bytes 74732430950
              :total-bytes 128849018880
              :status      :ok}
             {:mount       "/tank"
              :fs          "zfs"
              :used-bytes  14733455748300
              :avail-bytes 2858730232217
              :total-bytes 17592186044416
              :status      :degraded}
             {:mount       "/backup"
              :fs          "zfs"
              :used-bytes  8246337208320
              :avail-bytes 549755813888
              :total-bytes 8796093022208
              :status      :ok}]
   :units   {:curated [{:name "caddy.service"          :kind :service :active :active :sub "running" :since-sec 1420100}
                       {:name "postgresql.service"     :kind :service :active :active :sub "running" :since-sec 1420100}
                       {:name "tailscaled.service"     :kind :service :active :active :sub "running" :since-sec 1420200}
                       {:name "immich-server.service"  :kind :service :active :active :sub "running" :since-sec 268000}
                       {:name "zrepl.service"          :kind :service :active :failed :sub "failed"  :since-sec 1840}
                       {:name "restic-backup.timer"    :kind :timer   :active :active :sub "waiting" :next-sec 14400}]
             :failed  [{:name "zrepl.service"              :kind :service :active :failed :sub "failed" :since-sec 1840}
                       {:name "smartd.service"             :kind :service :active :failed :sub "failed" :since-sec 320}
                       {:name "paperless-consumer.service" :kind :service :active :failed :sub "failed" :since-sec 9120}]}})

(def microvm-state
  "Representative lightweight NixOS guest microvm with healthy units."
  {:host    {:hostname   "forge-vm"
             :role       "microvm"
             :os         "NixOS 25.05"
             :kernel     "6.12.19"
             :arch       "x86_64"
             :uptime-sec 312400}
   :network [{:iface "eth0"       :addr "10.20.0.14/24" :kind :lan       :state :up}
             {:iface "tailscale0" :addr "100.64.0.88"   :kind :tailscale :state :up}]
   :cpu     {:model     "QEMU Virtual CPU"
             :cores     4
             :load      [0.15 0.12 0.09]
             :usage-pct 4}
   :memory  {:used-bytes  1288490188
             :total-bytes 4294967296
             :swap-used   0
             :swap-total  0}
   :disks   [{:mount       "/"
              :fs          "tmpfs"
              :used-bytes  536870912
              :avail-bytes 1610612736
              :total-bytes 2147483648
              :status      :ok}
             {:mount       "/var/lib"
              :fs          "ext4"
              :used-bytes  19327352832
              :avail-bytes 45097156608
              :total-bytes 64424509440
              :status      :ok}]
   :units   {:curated [{:name "forgejo.service"     :kind :service :active :active :sub "running" :since-sec 312300}
                       {:name "caddy.service"       :kind :service :active :active :sub "running" :since-sec 312300}
                       {:name "tailscaled.service"  :kind :service :active :active :sub "running" :since-sec 312350}
                       {:name "gc-cleanup.timer"    :kind :timer   :active :active :sub "waiting" :next-sec 43200}]
             :failed  []}})

(def profiles
  {:host    host-state
   :microvm microvm-state
   :clean   (assoc-in host-state [:units :failed] [])})
