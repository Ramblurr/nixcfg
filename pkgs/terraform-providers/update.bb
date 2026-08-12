#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell]]
            [clojure.string :as str]))

(defn repo-root []
  (-> (shell {:out :string} "git" "rev-parse" "--show-toplevel")
      :out
      str/trim))

(defn package-name [script]
  (-> script
      (str/split #"/")
      last
      (str/replace-first #"^\." "")
      (str/replace-first #"-wrapped$" "")
      (str/replace-first #"^update-" "")))

(defn -main [& args]
  (let [package (package-name (System/getProperty "babashka.file"))
        provider (str/replace-first package #"^terraform-provider-" "")
        root (repo-root)]
    (apply shell
           {:dir root}
           "nix-update"
           package
           "--flake"
           "--override-filename"
           (str "pkgs/terraform-providers/" provider ".nix")
           args)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
