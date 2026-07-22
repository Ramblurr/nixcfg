#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell]]
            [clojure.string :as str]))

(def package-name "hindsight-cli")
(def package-file "pkgs/hindsight-cli/package.nix")
(def repo-url "https://github.com/vectorize-io/hindsight")

(defn repo-root []
  (-> (shell {:out :string} "git" "rev-parse" "--show-toplevel")
      :out
      str/trim))

(defn -main [& args]
  (let [root (repo-root)]
    (apply shell
           {:dir root}
           "nix-update"
           package-name
           "--flake"
           "-f"
           root
           "--override-filename"
           package-file
           "--version=stable"
           "--use-github-releases"
           "--generate-lockfile"
           "--lockfile-metadata-path"
           "hindsight-cli"
           "--url"
           repo-url
           args)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
