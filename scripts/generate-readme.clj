#!/usr/bin/env bb
(ns scripts.generate-readme
  (:require [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def begin "<!-- BEGIN HOSTS -->")
(def end "<!-- END HOSTS -->")
(def roles
  (array-map "desktop" ["Desktop" "&#x1F5A5;&#xFE0F;"]
             "laptop" ["Laptop" "&#x1F4BB;&#xFE0F;"]
             "gaming" ["Games Machine" "&#x1F3AE;&#xFE0F;"]
             "vm" ["Virtual Machine" "&#x1F404;&#xFE0F;"]
             "cloud" ["Cloud Server" "&#x2601;&#xFE0F;"]
             "server" ["Bare-metal server" "&#x1F5C4;&#xFE0F;"]
             "inactive" ["Not in service" "&#x1F9DF;&#xFE0F;"]))
(def operating-systems {"nixos" ["NixOS" "&#x2744;&#xFE0F;"]})
(def root (.getParentFile (.getParentFile (.getCanonicalFile (io/file *file*)))))

(defn cell [value]
  (if (nil? value)
    "Unknown"
    (-> (str value)
        (str/replace #"\r\n|[\n\r\u000b\u000c\u001c-\u001e\u0085\u2028\u2029]" " ")
        (str/escape {\& "&amp;" \< "&lt;" \> "&gt;"})
        (str/replace #"[\\`*_\[\]]" #(str "\\" %))
        (str/replace "|" "&#124;"))))

(defn icon [[label entity]]
  (str "<span title=\"" label "\">" entity "</span>"))

(defn documentation-url [url]
  (when-not (and (string? url) (str/starts-with? url "https://"))
    (throw (ex-info "documentation must be an HTTPS URL" {})))
  (apply str (map #(let [b (bit-and % 255)
                         c (char b)]
                     (if (re-matches #"[a-zA-Z0-9_.~:/#?=&%\-]" (str c))
                       (str c)
                       (format "%%%02X" b)))
                  (.getBytes url "UTF-8"))))

(defn host-row [[hostname host]]
  (when-not (re-matches #"[a-z0-9][a-z0-9-]*" hostname)
    (throw (ex-info (str "Invalid inventory hostname: " hostname) {})))
  (doseq [[field choices] [["channel" #{"stable" "unstable"}]
                           ["role" roles]
                           ["os" operating-systems]]]
    (when-not (contains? choices (get host field))
      (throw (ex-info (str hostname ": invalid " field) {}))))
  (when-not (contains? host "purpose")
    (throw (ex-info (str hostname ": missing purpose") {})))
  (let [{:strs [purpose documentation board cpu ramMiB gpu channel role os]} host]
    (when-not (or (nil? ramMiB) (and (integer? ramMiB) (pos? ramMiB)))
      (throw (ex-info (str hostname ": ramMiB must be a positive integer or null") {})))
    (str "| "
         (str/join " | "
                   [(str "[" hostname "](./hosts/" hostname "/)")
                    (if (nil? documentation)
                      (cell purpose)
                      (str "[" (cell purpose) "](" (documentation-url documentation) ")"))
                    (cell board)
                    (cell cpu)
                    (cell (when ramMiB
                            (if (zero? (mod ramMiB 1024))
                              (str (quot ramMiB 1024) " GiB")
                              (str ramMiB " MiB"))))
                    (cell gpu)
                    (cell channel)
                    (icon (roles role))
                    (icon (operating-systems os))])
         " |")))

(defn render-readme [source inventory]
  (when-not (and (= 1 (count (re-seq (re-pattern begin) source)))
                 (= 1 (count (re-seq (re-pattern end) source)))
                 (< (str/index-of source begin) (str/index-of source end)))
    (throw (ex-info "README must contain exactly one ordered pair of host table markers" {})))
  (let [visible (filter (fn [[hostname host]]
                          (let [show (get host "showInReadme" true)]
                            (when-not (boolean? show)
                              (throw (ex-info (str hostname ": showInReadme must be a boolean") {})))
                            show))
                        (sort-by key inventory))
        systems (set (map #(get (val %) "os") visible))
        legend (fn [entries]
                 (map (fn [[_ [label entity]]] (str "- " entity ": " label)) entries))
        rows (concat ["| Hostname | Purpose | Board | CPU | RAM | GPU | Channel | Role | OS |"
                      "|---|---|---|---|---|---|---|---|---|"]
                     (map host-row visible)
                     ["" "**Roles**" ""]
                     (legend roles)
                     ["" "**OS**" ""]
                     (legend (filter #(systems (key %)) operating-systems)))]
    (str (subs source 0 (str/index-of source begin)) begin "\n\n"
         (str/join "\n" rows) "\n\n"
         (subs source (str/index-of source end)))))

(def cli-spec
  {:restrict true
   :spec {:check {:coerce :boolean :desc "Fail if the table is stale; do not write"}
          :readme {:coerce :string :desc "README path (default: repository README.md)"}
          :inventory-json {:coerce :string :desc "Already evaluated public inventory"}
          :help {:coerce :boolean :alias :h :desc "Show help"}}})

(defn -main [& args]
  (try
    (let [opts (cli/parse-opts args cli-spec)]
      (if (:help opts)
        (do (println (cli/format-opts cli-spec)) 0)
        (let [inventory (json/parse-string
                         (if-let [path (:inventory-json opts)]
                           (slurp path :encoding "UTF-8")
                           (let [{:keys [exit out err]} (shell/sh "nix" "eval" "--json" "--file"
                                                                  (str (io/file root "hosts/inventory.nix")))]
                             (when-not (zero? exit)
                               (throw (ex-info err {:exit exit})))
                             out)))
              readme (or (:readme opts) (io/file root "README.md"))
              original (slurp readme :encoding "UTF-8")
              rendered (render-readme original inventory)]
          (cond
            (= original rendered) 0
            (:check opts) (do (binding [*out* *err*]
                                (println (str readme ": host table is stale; run bb scripts/generate-readme.clj")))
                              1)
            :else (do (spit readme rendered :encoding "UTF-8")
                      (println "Updated" (str readme))
                      0)))))
    (catch Exception error
      (binding [*out* *err*]
        (println "generate-readme:" (ex-message error)))
      1)))

(when (= *file* (System/getProperty "babashka.file"))
  (System/exit (apply -main *command-line-args*)))
