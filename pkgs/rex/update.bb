#!/usr/bin/env bb
(ns update-rex
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :refer [shell]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def release-url "https://blob.superlogical.com/api/releases/rex/preview")

(def artifacts {"x86_64-linux"  "rex-linux-amd64.tar.gz"
                "aarch64-linux" "rex-linux-arm64.tar.gz"})

(defn output [& command]
  (-> (apply shell {:out :string} command) :out str/trim))

(defn sri-hash [release filename]
  (let [matches (filter #(= filename (:filename %)) (:artifacts release))]
    (when-not (= 1 (count matches))
      (throw (ex-info "Expected one release artifact" {:filename filename})))
    (output "nix" "hash" "convert" "--hash-algo" "sha256" "--to" "sri" (:sha256 (first matches)))))

(defn replace-binding [text key value]
  (let [pattern (re-pattern (str "(?m)^(\\s*" key " = )\"[^\"]+\";"))]
    (when-not (= 1 (count (re-seq pattern text)))
      (throw (ex-info "Expected one package binding" {:key key})))
    (str/replace text pattern (fn [[_ prefix]] (str prefix "\"" value "\";")))))

(defn -main [& args]
  (when (seq args)
    (throw (ex-info "Usage: bb pkgs/rex/update.bb" {})))
  (let [root (output "git" "rev-parse" "--show-toplevel")
        file (str (fs/path root "pkgs/rex/package.nix"))
        release (json/parse-string (:body (http/get release-url)) true)
        bindings (into {"version" (:version release)
                        "rev"     (:commit release)}
                       (map (fn [[system filename]] [system (sri-hash release filename)]))
                       artifacts)
        original (slurp file)
        updated (reduce-kv replace-binding original bindings)]
    (when-not (= original updated)
      (spit file updated))
    (println (str "rex: " (:version release) (if (= original updated) " (unchanged)" " (updated)")))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
