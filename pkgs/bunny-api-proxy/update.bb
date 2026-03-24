#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell sh]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def package-dir
  (str
   (.getParent
    (java.nio.file.Paths/get (System/getProperty "babashka.file")
                             (into-array String [])))))

(defn flake-store-path []
  (let [metadata (-> (sh {:dir package-dir} "nix" "flake" "metadata" "--json" "../..")
                     :out
                     (json/parse-string true))
        path (:path metadata)]
    (when (or (nil? path) (str/blank? path))
      (throw (ex-info "Failed to determine flake store path" {:metadata metadata})))
    path))

(defn -main [& args]
  (let [store-path (flake-store-path)]
    (apply shell
           {:dir package-dir}
           "nix-update"
           "bunny-api-proxy"
           "--flake"
           "-f"
           store-path
           "--override-filename"
           "package.nix"
           args)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
