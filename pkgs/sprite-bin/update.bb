#!/usr/bin/env bb

(ns update
  (:require [babashka.http-client :as http]
            [babashka.process :refer [shell sh]]
            [clojure.string :as str]))

(def self "pkgs/sprite-bin/package.nix")
(def base-url "https://sprites-binaries.t3.storage.dev")

(def platforms
  {"aarch64-darwin" "darwin-arm64"
   "x86_64-darwin"  "darwin-amd64"
   "aarch64-linux"  "linux-arm64"
   "x86_64-linux"   "linux-amd64"})

(defn fetch-version []
  (-> (http/get (str base-url "/client/rc.txt"))
      :body
      str/trim
      (str/replace #"^v" "")))

(defn prefetch-hash [url]
  (let [nix-hash (-> (sh "nix-prefetch-url" url)
                     :out
                     str/trim)]
    (-> (sh "nix" "hash" "to-sri" "--type" "sha256" nix-hash)
        :out
        str/trim)))

(defn update-nix-version [version]
  (shell "ast-grep" "run"
         "--pattern" "{version = $VALUE;}"
         "--selector" "binding"
         "--rewrite" (str "version = \"" version "\";")
         "--update-all"
         self))

(defn update-nix-hash [nix-platform hash]
  (shell "ast-grep" "run"
         "--pattern" (str "{" nix-platform ".hash = $VALUE;}")
         "--selector" "binding"
         "--rewrite" (str nix-platform ".hash = \"" hash "\";")
         "--update-all"
         self))

(defn -main [& _args]
  ;; Fetch the latest RC version
  (println "Fetching latest RC version...")
  (let [version (fetch-version)]
    (println (str "Latest version: " version))

    ;; Fetch hash for each platform
    (let [updates (atom {:version version})]
      (doseq [[nix-platform binary-name] platforms]
        (let [url (format "%s/client/v%s/sprite-%s.tar.gz" base-url version binary-name)]
          (println (format "Fetching hash for %s (%s)..." nix-platform binary-name))
          (let [hash (prefetch-hash url)]
            (println (str "  URL: " url))
            (println (str "  Hash: " hash))
            (swap! updates assoc (keyword (str nix-platform ".hash")) hash))))

      ;; Update the nix file
      (println "\nUpdating package.nix...")

      (println (str "  Updating version to " version))
      (update-nix-version version)

      ;; Update hashes
      (doseq [platform (keys platforms)]
        (let [hash-key (keyword (str platform ".hash"))
              hash (get @updates hash-key)]
          (println (str "  Updating " platform ".hash"))
          (update-nix-hash platform hash)))

      (println "Done!"))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
