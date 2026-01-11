#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell sh]]
            [clojure.string :as str]))

(def self "pkgs/gitbutler-bin/package.nix")
(def download-url "https://app.gitbutler.com/downloads/release/linux/x86_64/deb")

(defn fetch-latest-url []
  ;; Use curl to get the redirect location header
  (-> (sh "curl" "-sI" download-url)
      :out
      (str/split #"\n")
      (->> (filter #(str/starts-with? (str/lower-case %) "location:"))
           first)
      (str/replace #"(?i)^location:\s*" "")
      str/trim))

(defn parse-version-build [url]
  ;; URL format: .../releases/release/0.18.3-2698/linux/x86_64/GitButler_0.18.3_amd64.deb
  (let [[_ version-build] (re-find #"/release/([^/]+)/linux" url)
        [version build] (str/split version-build #"-")]
    {:version version
     :build build}))

(defn prefetch-hash [url]
  (let [nix-hash (-> (sh "nix-prefetch-url" url)
                     :out
                     str/trim)]
    (-> (sh "nix" "hash" "to-sri" "--type" "sha256" nix-hash)
        :out
        str/trim)))

(defn update-nix-file [key value]
  (shell "ast-grep" "run"
         "--pattern" (str "{" key " = $VALUE;}")
         "--selector" "binding"
         "--rewrite" (str key " = \"" value "\";")
         "--update-all"
         self))

(defn -main [& _args]
  (println "Fetching latest GitButler release URL...")
  (let [url (fetch-latest-url)]
    (println (str "  URL: " url))

    (let [{:keys [version build]} (parse-version-build url)]
      (println (str "  Version: " version))
      (println (str "  Build: " build))

      (println "Fetching hash...")
      (let [hash (prefetch-hash url)]
        (println (str "  Hash: " hash))

        (println "\nUpdating package.nix...")
        (update-nix-file "version" version)
        (update-nix-file "build" build)
        (update-nix-file "hash" hash)

        (println "Done!")))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
