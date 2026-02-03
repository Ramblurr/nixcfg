#!/usr/bin/env bb
(ns update
  (:require [babashka.http-client :as http]
            [babashka.process :refer [shell sh]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def self "pkgs/nvidia/package.nix")

(def versions-url "https://raw.githubusercontent.com/aaronp24/nvidia-versions/main/nvidia-versions.txt")

(defn fetch-nvidia-version []
  (let [body (-> (http/get versions-url) :body)
        lines (str/split-lines body)
        target-row (->> lines
                        (filter #(str/includes? % "long-lived-branch-release"))
                        first)]
    (if target-row
      (let [cols (str/split target-row #"\s+")]
        (if (>= (count cols) 3)
          (nth cols 2)
          (throw (ex-info "Unexpected row format in nvidia-versions.txt"
                          {:row target-row}))))
      (throw (ex-info "Could not find long-lived-branch-release in nvidia-versions.txt"
                      {:url versions-url})))))

(defn prefetch-driver-hash [version]
  (let [url (format "https://us.download.nvidia.com/XFree86/Linux-x86_64/%s/NVIDIA-Linux-x86_64-%s.run"
                    version version)
        nix-hash (-> (sh "nix-prefetch-url" url)
                     :out
                     str/trim)]
    (-> (sh "nix" "hash" "to-sri" "--type" "sha256" nix-hash)
        :out
        str/trim)))

(defn prefetch-open-kernel-hash [version]
  (-> (sh "nix-prefetch-github" "--rev" version "NVIDIA" "open-gpu-kernel-modules")
      :out
      (json/parse-string true)
      :hash))

(defn update-nix-file [key value]
  (shell "ast-grep" "run"
         "--pattern" (str "{" key " = $VALUE;}")
         "--selector" "binding"
         "--rewrite" (str key " = \"" value "\";")
         "--update-all"
         self))

(defn -main [& _args]
  (println "Fetching NVIDIA driver version...")
  (let [version (fetch-nvidia-version)]
    (println (str "Version: " version))

    (println "Fetching driver hash...")
    (let [sha256-64bit (prefetch-driver-hash version)]
      (println (str "  sha256_64bit: " sha256-64bit))

      (println "Fetching open kernel modules hash...")
      (let [open-sha256 (prefetch-open-kernel-hash version)]
        (println (str "  openSha256: " open-sha256))

        (println "\nUpdating package.nix...")
        (update-nix-file "version" version)
        (update-nix-file "sha256_64bit" sha256-64bit)
        (update-nix-file "openSha256" open-sha256)

        (println "Done!")))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
