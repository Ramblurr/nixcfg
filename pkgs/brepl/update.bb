#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell sh]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def self "pkgs/brepl/package.nix")

(defn -main [& _args]
  ;; Fetch the latest release tag from GitHub
  (println "Fetching latest release from GitHub...")
  (let [release (-> (sh "gh" "api" "repos/licht1stein/brepl/releases/latest" "--jq" ".tag_name")
                    :out
                    str/trim)]
    (println (str "Latest release: " release))

    ;; Prefetch the GitHub repo
    (println "Fetching hash...")
    (let [prefetch (-> (sh "nix-prefetch-github" "licht1stein" "brepl" "--rev" release)
                       :out
                       (json/parse-string true))
          hash (:hash prefetch)]
      (println (str "  Hash: " hash))

      ;; Update the nix file
      (println "\nUpdating package.nix...")

      (println (str "  Updating rev to " release))
      (shell "ast-grep" "run"
             "--pattern" "{rev = $VALUE;}"
             "--selector" "binding"
             "--rewrite" (str "rev = \"" release "\";")
             "--update-all"
             self)

      (println "  Updating hash")
      (shell "ast-grep" "run"
             "--pattern" "{hash = $VALUE;}"
             "--selector" "binding"
             "--rewrite" (str "hash = \"" hash "\";")
             "--update-all"
             self)

      (println "Done!"))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
