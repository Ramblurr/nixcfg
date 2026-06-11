#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell]]
            [clojure.string :as str]))

(def package-name "spdx-util")
(def package-file "pkgs/spdx-util/package.nix")
(def repo-url "https://github.com/ramblurr/spdx-util")

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
           "--version=branch=main"
           "--url"
           repo-url
           args)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
