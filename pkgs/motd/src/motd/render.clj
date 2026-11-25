(ns motd.render
  "Pure data-driven MOTD renderer: ui = f(state, opts)."
  (:require
   [clojure.string :as str]))

(def ^:private ansi-pattern #"\u001b\[[0-9;]*m")

(def ^:private ansi-codes
  {:reset       "\u001b[0m"
   :bold        "\u001b[1m"
   :dim         "\u001b[2m"
   :red         "\u001b[31m"
   :green       "\u001b[32m"
   :yellow      "\u001b[33m"
   :cyan        "\u001b[36m"
   :bold-red    "\u001b[1;31m"
   :bold-cyan   "\u001b[1;36m"
   :bold-white  "\u001b[1;37m"})

(def ^:private unicode-glyphs
  {:tl       "╭"
   :tr       "╮"
   :bl       "╰"
   :br       "╯"
   :h        "─"
   :v        "│"
   :bar-full "█"
   :bar-half "▌"
   :bar-low  "░"
   :active   "●"
   :timer    "⏲︎"
   :failed   "✗"
   :inactive "○"
   :pending  "◐"
   :dot      "·"})

(def ^:private ascii-glyphs
  {:tl       "+"
   :tr       "+"
   :bl       "+"
   :br       "+"
   :h        "-"
   :v        "|"
   :bar-full "#"
   :bar-half "="
   :bar-low  "-"
   :active   "*"
   :timer    "t"
   :failed   "x"
   :inactive "o"
   :pending  "~"
   :dot      "|"})

(defn theme
  "Build rendering theme from options map."
  [{:keys [color? unicode? width]
    :or   {color? true unicode? true width 76}}]
  {:color?   (boolean color?)
   :unicode? (boolean unicode?)
   :width    (max 56 (long width))
   :glyphs   (if unicode? unicode-glyphs ascii-glyphs)})

(defn strip-ansi
  "Remove ANSI SGR escape sequences from string s."
  [s]
  (str/replace (str s) ansi-pattern ""))

(defn visible-len
  "Return display width of string s excluding ANSI escape codes and variation selectors."
  [s]
  (count (str/replace (strip-ansi s) "\ufe0e" "")))

(defn paint
  "Apply semantic style key to text when color is enabled."
  [th style-key text]
  (if-let [code (and (:color? th) (get ansi-codes style-key))]
    (str code text (:reset ansi-codes))
    (str text)))

(defn pad-right
  "Pad string s on the right to visible width n."
  [s n]
  (let [gap (- n (visible-len s))]
    (if (pos? gap)
      (str s (apply str (repeat gap " ")))
      s)))

(defn pad-left
  "Pad string s on the left to visible width n."
  [s n]
  (let [gap (- n (visible-len s))]
    (if (pos? gap)
      (str (apply str (repeat gap " ")) s)
      s)))

(defn format-bytes
  "Format byte count into compact human-readable IEC units."
  [n]
  (let [n (double (max 0 (or n 0)))]
    (cond
      (< n 1024.0)             (format "%dB" (long n))
      (< n (Math/pow 1024 2))  (format "%.0fK" (/ n 1024.0))
      (< n (Math/pow 1024 3))  (format "%.0fM" (/ n (Math/pow 1024 2)))
      (< n (Math/pow 1024 4))  (let [g (/ n (Math/pow 1024 3))]
                                 (if (>= g 100.0)
                                   (format "%.0fG" g)
                                   (format "%.1fG" g)))
      :else                    (format "%.1fT" (/ n (Math/pow 1024 4))))))

(defn format-duration
  "Format seconds into compact human-readable duration."
  [secs]
  (let [s     (max 0 (long (or secs 0)))
        days  (quot s 86400)
        hours (quot (mod s 86400) 3600)
        mins  (quot (mod s 3600) 60)]
    (cond
      (pos? days)  (format "%dd %dh %dm" days hours mins)
      (pos? hours) (format "%dh %dm" hours mins)
      (pos? mins)  (format "%dm" mins)
      :else        (format "%ds" s))))

(defn format-short-age
  "Format seconds into a single-unit compact age string."
  [secs]
  (let [s (max 0 (long (or secs 0)))]
    (cond
      (>= s 86400) (format "%dd" (quot s 86400))
      (>= s 3600)  (format "%dh" (quot s 3600))
      (>= s 60)    (format "%dm" (quot s 60))
      :else        (format "%ds" s))))

(defn pct-color
  "Return semantic color key for utilization percentage."
  [pct]
  (cond
    (>= pct 90) :red
    (>= pct 70) :yellow
    :else       :green))

(defn load-color
  "Return semantic color key for load average relative to core count."
  [load-val cores]
  (let [ratio (/ (double load-val) (max 1 (double (or cores 1))))]
    (cond
      (>= ratio 1.0) :red
      (>= ratio 0.7) :yellow
      :else          :green)))

(defn render-bar
  "Render a horizontal utilization bar of given character width."
  [th pct bar-width]
  (let [clamped (max 0.0 (min 100.0 (double (or pct 0))))
        filled  (long (Math/round (* (/ clamped 100.0) bar-width)))
        empty-n (max 0 (- bar-width filled))
        g       (:glyphs th)
        color   (pct-color clamped)]
    (str (paint th color (apply str (repeat filled (:bar-full g))))
         (paint th :dim (apply str (repeat empty-n (:bar-low g)))))))

(defn render-header
  "Render top framed identity box with host, OS, kernel, uptime, and load."
  [th {:keys [host cpu]}]
  (let [g        (:glyphs th)
        w        (:width th)
        inner    (- w 2)
        dot      (paint th :dim (:dot g))
        title    (paint th :bold-cyan (:hostname host))
        meta-str (paint th :dim (str (:os host) " " (:dot g) " " (:role host)))
        rule-n   (max 1 (- inner 6 (visible-len title) (visible-len meta-str)))
        top-line (str (paint th :dim (str (:tl g) (:h g) " "))
                      title
                      " "
                      (paint th :dim (apply str (repeat rule-n (:h g))))
                      " "
                      meta-str
                      (paint th :dim (str " " (:h g) (:tr g))))
        cores    (:cores cpu)
        load-str (->> (:load cpu)
                      (map #(paint th (load-color % cores) (format "%.2f" (double %))))
                      (str/join " "))
        info     (str "Linux " (:kernel host)
                      (paint th :dim (str " (" (:arch host) ")"))
                      "  " dot "  "
                      (paint th :dim "up ") (format-duration (:uptime-sec host))
                      "  " dot "  "
                      (paint th :dim "load ") load-str)
        mid-line (str (paint th :dim (:v g))
                      " "
                      (pad-right info (- inner 2))
                      " "
                      (paint th :dim (:v g)))
        bot-line (paint th :dim (str (:bl g) (apply str (repeat inner (:h g))) (:br g)))]
    [top-line mid-line bot-line]))

(defn- format-iface
  [th {:keys [iface addr state]}]
  (let [addr-style (if (= :up state) :dim :red)
        suffix     (when (not= :up state)
                     (str " " (paint th :red (name state))))]
    (str iface " " (paint th addr-style addr) suffix)))

(defn- format-ratio
  [orig compr]
  (when (and (pos? (or orig 0)) (pos? (or compr 0)))
    (format "%.1fx" (/ (double orig) (double compr)))))

(defn- format-meta-paren
  [th parts]
  (let [items (remove str/blank? parts)]
    (when (seq items)
      (paint th :dim (str " (" (str/join (str " " (:dot (:glyphs th)) " ") items) ")")))))

(defn- format-zram-row
  [th {:keys [orig-bytes compr-bytes total-bytes algo]}]
  (let [total (max 1 (long (or total-bytes 1)))
        orig  (long (or orig-bytes 0))
        compr (long (or compr-bytes 0))
        pct   (long (Math/round (* 100.0 (/ (double orig) (double total)))))
        meta  (format-meta-paren th [(format-ratio orig compr) algo])]
    ["Zram" (str (render-bar th pct 12)
                 " "
                 (paint th (pct-color pct) (pad-left (str pct "%") 4))
                 "  "
                 (format-bytes orig)
                 (paint th :dim (str " / " (format-bytes total)))
                 "   "
                 (paint th :dim (str (format-bytes compr) " RAM"))
                 meta)]))

(defn- format-zswap-row
  [th {:keys [pool-bytes stored-bytes max-pool-bytes algo]}]
  (let [pool  (long (or pool-bytes 0))
        stored (long (or stored-bytes 0))
        limit (max 1 (long (or max-pool-bytes (max 1 pool))))
        pct   (long (Math/round (* 100.0 (/ (double pool) (double limit)))))
        meta  (format-meta-paren th [(format-ratio stored pool) algo])]
    ["Zswap" (str (render-bar th pct 12)
                  " "
                  (paint th (pct-color pct) (pad-left (str pct "%") 4))
                  "  "
                  (format-bytes pool)
                  (when max-pool-bytes
                    (paint th :dim (str " / " (format-bytes max-pool-bytes))))
                  "   "
                  (paint th :dim (str (format-bytes stored) " stored"))
                  meta)]))

(defn render-system
  "Render System section covering network, CPU, memory, and optional zram/zswap."
  [th {:keys [network cpu memory zram zswap]}]
  (let [g          (:glyphs th)
        sep        (str "  " (paint th :dim (:dot g)) "  ")
        net-val    (if (seq network)
                     (str/join sep (map #(format-iface th %) network))
                     (paint th :dim "none"))
        cpu-pct    (long (Math/round (double (or (:usage-pct cpu) 0))))
        cpu-val    (str (render-bar th cpu-pct 12)
                        " "
                        (paint th (pct-color cpu-pct) (pad-left (str cpu-pct "%") 4))
                        "  "
                        (paint th :dim (str (:model cpu) " (" (:cores cpu) "c)")))
        mem-total  (max 1 (long (or (:total-bytes memory) 1)))
        mem-used   (long (or (:used-bytes memory) 0))
        mem-pct    (long (Math/round (* 100.0 (/ (double mem-used) (double mem-total)))))
        swap-total (long (or (:swap-total memory) 0))
        swap-str   (when (pos? swap-total)
                     (str "   "
                          (paint th :dim (str "swap "
                                              (format-bytes (:swap-used memory))
                                              " / "
                                              (format-bytes swap-total)))))
        mem-val    (str (render-bar th mem-pct 12)
                        " "
                        (paint th (pct-color mem-pct) (pad-left (str mem-pct "%") 4))
                        "  "
                        (format-bytes mem-used)
                        (paint th :dim (str " / " (format-bytes mem-total)))
                        swap-str)
        rows       (cond-> [["Network" net-val]
                            ["CPU"     cpu-val]
                            ["Memory"  mem-val]]
                     zram  (conj (format-zram-row th zram))
                     zswap (conj (format-zswap-row th zswap)))]
    (into [(paint th :bold-cyan "System")]
          (map (fn [[k v]]
                 (str "  " (paint th :dim (pad-right k 9)) " " v))
               rows))))

(defn- disk-status-badge
  [th status]
  (case status
    :ok       (paint th :green "ok")
    :degraded (paint th :yellow "degraded")
    :faulted  (paint th :red "faulted")
    (paint th :dim (name (or status :unknown)))))

(defn render-disks
  "Render Disks section with inline usage bars, capacity, availability, and status."
  [th {:keys [disks]}]
  (when (seq disks)
    (let [mount-w (max 8 (apply max (map #(count (:mount %)) disks)))
          rows    (map (fn [{:keys [mount used-bytes avail-bytes total-bytes status]}]
                         (let [total (max 1 (long (or total-bytes 1)))
                               used  (long (or used-bytes 0))
                               pct   (long (Math/round (* 100.0 (/ (double used) (double total)))))]
                           {:mount  mount
                            :bar    (render-bar th pct 12)
                            :pct    (paint th (pct-color pct) (pad-left (str pct "%") 4))
                            :cap    (str (pad-left (format-bytes used) 6)
                                         (paint th :dim (str " / " (pad-right (format-bytes total) 5))))
                            :avail  (paint th :dim (str (pad-left (format-bytes avail-bytes) 6) " avail"))
                            :status (disk-status-badge th status)}))
                       disks)]
      (into [(paint th :bold-cyan "Disks")]
            (map (fn [{:keys [mount bar pct cap avail status]}]
                   (str "  "
                        (pad-right mount mount-w)
                        "  " bar
                        " " pct
                        "  " cap
                        "  " avail
                        "  " status))
                 rows)))))

(defn- split-unit-name
  [unit-name]
  (if-let [idx (str/last-index-of unit-name ".")]
    [(subs unit-name 0 idx) (subs unit-name idx)]
    [unit-name ""]))

(defn- unit-glyph-and-style
  [th {:keys [kind active]}]
  (let [g (:glyphs th)]
    (case active
      :active     [(if (= :timer kind) (:timer g) (:active g)) :green]
      :failed     [(:failed g) :red]
      :activating [(:pending g) :yellow]
      :reloading  [(:pending g) :yellow]
      [(:inactive g) :dim])))

(defn- display-unit-name
  [unit-name]
  (if (str/ends-with? unit-name ".service")
    (subs unit-name 0 (- (count unit-name) 8))
    unit-name))

(defn- format-unit-name
  [th unit-name max-w]
  (let [shown   (display-unit-name unit-name)
        trimmed (if (> (count shown) max-w)
                  (str (subs shown 0 (- max-w 1)) "~")
                  shown)
        [base ext] (split-unit-name trimmed)]
    (pad-right (str base (paint th :dim ext)) max-w)))

(defn- unit-state-label
  [{:keys [kind active sub]}]
  (cond
    (= :failed active) "failed"
    (= :timer kind)    (or sub (name active))
    :else              (name active)))

(defn- unit-timing-label
  [{:keys [kind active since-sec next-sec]}]
  (cond
    (and (= :timer kind) (= :active active) next-sec)
    (str "in " (format-short-age next-sec))

    since-sec
    (format-short-age since-sec)

    :else ""))

(defn- render-unit-cell
  [th unit name-w]
  (let [[glyph style] (unit-glyph-and-style th unit)
        state-str     (unit-state-label unit)
        timing        (unit-timing-label unit)]
    (str (paint th style glyph)
         " "
         (format-unit-name th (:name unit) name-w)
         " "
         (paint th style (pad-right state-str 7))
         " "
         (paint th :dim (pad-left timing 6)))))

(defn render-curated-units
  "Render opt-in curated systemd units in a 2-column or 1-column aligned grid."
  [th {:keys [units]}]
  (let [curated (:curated units)]
    (when (seq curated)
      (let [two-col? (and (>= (:width th) 72) (> (count curated) 1))
            name-w   (if two-col? 18 34)
            cells    (mapv #(render-unit-cell th % name-w) curated)
            lines    (if two-col?
                       (map (fn [[a b]]
                              (if b
                                (str "  " a "   " b)
                                (str "  " a)))
                            (partition-all 2 cells))
                       (map #(str "  " %) cells))]
        (into [(paint th :bold-cyan "Units")] lines)))))

(defn uncurated-failed-units
  "Return failed units not already present in the curated unit list."
  [{:keys [units]}]
  (let [curated-names (into #{} (map :name) (:curated units))]
    (vec (remove #(contains? curated-names (:name %)) (:failed units)))))

(defn render-failed-units
  "Render any failed systemd units not already listed under curated units."
  [th state]
  (let [failed (uncurated-failed-units state)]
    (when (seq failed)
      (let [g      (:glyphs th)
            name-w (max 24 (apply max (map #(count (:name %)) failed)))
            header (paint th :bold-red (format "Failed Units (%d)" (count failed)))]
        (into [header]
              (map (fn [{:keys [name since-sec]}]
                     (let [[base ext] (split-unit-name name)
                           age        (when since-sec
                                        (str (format-short-age since-sec) " ago"))]
                       (str "  "
                            (paint th :red (:failed g))
                            " "
                            (pad-right (str base (paint th :dim ext)) name-w)
                            "  "
                            (paint th :red "failed")
                            (when age
                              (str "  " (paint th :dim age))))))
                   failed))))))

(defn render
  "Pure view function: state map + options -> rendered MOTD string."
  ([state]
   (render state {}))
  ([state opts]
   (let [th       (theme opts)
         sections (remove nil?
                          [(render-header th state)
                           (render-system th state)
                           (render-disks th state)
                           (render-curated-units th state)
                           (render-failed-units th state)])]
     (str (str/join "\n\n" (map #(str/join "\n" %) sections)) "\n"))))
