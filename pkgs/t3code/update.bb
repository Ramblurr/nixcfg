#!/usr/bin/env bb

(ns update
  (:require [babashka.http-client :as http]
            [babashka.process :refer [shell sh]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def package-file "pkgs/t3code/unwrapped.nix")
(def releases-url "https://api.github.com/repos/pingdotgg/t3code/releases?per_page=50")
(def nightly-tag #"^v(\d+\.\d+\.\d+-nightly\.\d{8}\.\d+)$")
(def fake-hash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")

(def hashes
  "Hash bindings in package-file, each matched by the line before it."
  {:src {:pattern #"(tag = \"v\$\{version\}\";\s+hash = \")[^\"]+(\")"}
   :pnpm-deps {:pattern #"(fetcherVersion = \d+;\s+hash = \")[^\"]+(\")"
               :attr "t3code.pnpmDeps"}
   :cargo-deps {:pattern #"(cargoHash = \")[^\"]+(\")"
                :attr "t3code.resourceMonitor.cargoDeps"}})

(defn repo-root []
  (-> (sh "git" "rev-parse" "--show-toplevel") :out str/trim))

(defn latest-nightly []
  (let [token (System/getenv "GITHUB_TOKEN")
        releases (-> (http/get releases-url
                               {:headers (cond-> {"Accept" "application/vnd.github+json"}
                                           token (assoc "Authorization" (str "Bearer " token)))})
                     :body
                     (json/parse-string true))]
    (or (->> releases
             (remove :draft)
             (sort-by :published_at #(compare %2 %1))
             (keep #(second (re-matches nightly-tag (:tag_name %))))
             first)
        (throw (ex-info "No nightly release found" {:url releases-url})))))

(defn replace-once [text pattern replacement]
  (let [matches (count (re-seq pattern text))]
    (when (not= 1 matches)
      (throw (ex-info "Expected exactly one match" {:pattern (str pattern) :matches matches})))
    (str/replace text pattern replacement)))

(defn set-hash! [file key value]
  (spit file (replace-once (slurp file) (get-in hashes [key :pattern]) (str "$1" value "$2"))))

(defn source-hash [version]
  (-> (sh "nix" "flake" "prefetch" "--json" (str "github:pingdotgg/t3code/v" version))
      :out
      (json/parse-string true)
      :hash))

(defn built-hash
  "Builds attr with a fake hash and returns the hash Nix reports instead."
  [root attr]
  (let [{:keys [exit err]} (sh {:dir root} "nix" "build" "--no-link" (str ".#" attr))]
    (or (second (re-find #"got:\s+(sha256-\S+)" err))
        (throw (ex-info (str "No hash mismatch reported for " attr)
                        {:exit exit :err (str/join "\n" (take-last 20 (str/split-lines err)))})))))

(defn -main [& _args]
  (let [root (repo-root)
        file (str root "/" package-file)
        current (second (re-find #"version = \"([^\"]+)\";" (slurp file)))
        version (latest-nightly)]
    (if (= current version)
      (println (str "t3code is already at " version))
      (do
        (println (str "Updating t3code " current " -> " version))
        (spit file (replace-once (slurp file) #"version = \"[^\"]+\";" (str "version = \"" version "\";")))
        (set-hash! file :src (source-hash version))
        (doseq [key [:pnpm-deps :cargo-deps]]
          (set-hash! file key fake-hash)
          (let [value (built-hash root (get-in hashes [key :attr]))]
            (println (str "  " (name key) ": " value))
            (set-hash! file key value)))
        (shell {:dir root} "nix" "build" "--no-link" ".#t3code" ".#t3code-desktop")))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
