(ns motd.render-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [motd.dummy :as dummy]
   [motd.render :as render]))

(deftest no-color-strips-ansi-escapes
  (let [out (render/render dummy/host-state {:color? false})]
    (is (not (re-find #"\u001b\[" out)))
    (is (str/includes? out "dewey"))
    (is (str/includes? out "Failed Units (2)"))))

(deftest ascii-mode-uses-only-ascii
  (let [out (render/render dummy/host-state {:color? false :unicode? false})]
    (is (every? #(< (int %) 128) out))))

(deftest failed-units-exclude-curated-duplicates
  (let [uncurated (render/uncurated-failed-units dummy/host-state)
        names     (set (map :name uncurated))]
    (is (not (contains? names "zrepl.service")))
    (is (= #{"smartd.service" "paperless-consumer.service"} names))))

(deftest failed-section-omitted-when-no-extra-failures
  (let [out (render/render dummy/microvm-state {:color? false})]
    (is (not (str/includes? out "Failed Units")))))

(deftest box-lines-align-to-target-width
  (testing "Header box lines match requested width even with ANSI colors"
    (let [out   (render/render dummy/host-state {:color? true :width 76})
          lines (take 3 (str/split-lines out))]
      (is (= [76 76 76] (mapv render/visible-len lines))))))

(deftest zram-and-zswap-render-only-when-present
  (let [with-both (render/render dummy/host-state {:color? false})
        without   (render/render dummy/microvm-state {:color? false})]
    (is (str/includes? with-both "Zram"))
    (is (str/includes? with-both "3.2x"))
    (is (str/includes? with-both "Zswap"))
    (is (str/includes? with-both "2.8x"))
    (is (not (str/includes? without "Zram")))
    (is (not (str/includes? without "Zswap")))))

(deftest many-network-interfaces-wrap-within-width
  (let [many-ifaces (mapv (fn [i]
                            {:iface (str "eth" i)
                             :addr  (str "10.9." i ".10/24")
                             :kind  :lan
                             :state :up})
                          (range 7))
        state       (assoc dummy/host-state :network many-ifaces)
        lines       (render/render-system (render/theme {:color? false :width 76}) state)]
    (is (> (count lines) 6))
    (is (every? #(<= (render/visible-len %) 76) lines))))
