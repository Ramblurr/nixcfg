#!/usr/bin/env bb
(ns update-bb-desktop
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn output [& command]
  (-> (apply shell {:out :string} command) :out str/trim))

(defn latest-version []
  (let [release (json/parse-string
                 (output "gh" "api" "repos/get-bb/bb/releases/tags/desktop-latest") true)
        versions (keep #(second (re-matches #"bb-([0-9]+\.[0-9]+\.[0-9]+)-x86_64\.AppImage"
                                            (:name %)))
                       (:assets release))]
    (when-not (= 1 (count versions))
      (throw (ex-info "Expected one stable Linux x86_64 desktop asset" {})))
    (first versions)))

(defn replace-binding [text key value]
  (let [pattern (re-pattern (str "(?m)^(\\s*" key " = )\"[^\"]+\";"))]
    (when-not (= 1 (count (re-seq pattern text)))
      (throw (ex-info "Expected one package binding" {:key key})))
    (str/replace text pattern (fn [[_ prefix]] (str prefix "\"" value "\";")))))

(defn -main [& args]
  (when (seq args)
    (throw (ex-info "Usage: bb pkgs/bb-desktop/update.bb" {})))
  (let [root (output "git" "rev-parse" "--show-toplevel")
        file (fs/path root "pkgs/bb-desktop/package.nix")
        version (latest-version)
        ;; Fetch the immutable release URL used by package.nix, not desktop-latest.
        url (format "https://github.com/get-bb/bb/releases/download/desktop-v%s/bb-%s-x86_64.AppImage"
                    version version)
        hash (:hash (json/parse-string (output "nix" "store" "prefetch-file" "--json" url) true))]
    (when-not (and (string? hash) (str/starts-with? hash "sha256-"))
      (throw (ex-info "Prefetch did not return a SHA-256 hash" {})))
    (let [original (slurp (str file))
          updated (-> original
                      (replace-binding "version" version)
                      (replace-binding "hash" hash))]
      (when-not (= original updated)
        (spit (str file) updated))
      (println (str "bb-desktop: " version (if (= original updated) " (unchanged)" " (updated)"))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
