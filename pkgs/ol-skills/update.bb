#!/usr/bin/env bb

(ns update
  (:require [babashka.process :refer [shell]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def repo-url "https://git.outskirtslabs.com/casey/skills")

(defn -main [& _args]
  (let [root (-> (shell {:out :string} "git" "rev-parse" "--show-toplevel")
                 :out
                 str/trim)
        package-file (str root "/pkgs/ol-skills/package.nix")
        rev (-> (shell {:out :string} "git" "ls-remote" (str repo-url ".git") "HEAD")
                :out
                (str/split #"\s+")
                first)]
    (when-not (re-matches #"[0-9a-f]{40}" rev)
      (throw (ex-info "Could not resolve the skills repository HEAD" {:rev rev})))
    (let [hash (-> (shell {:out :string} "nix" "store" "prefetch-file" "--json" "--unpack"
                          (str repo-url "/archive/" rev ".tar.gz"))
                   :out
                   (json/parse-string true)
                   :hash)]
      (when (str/blank? hash)
        (throw (ex-info "Prefetch returned no source hash" {})))
      (doseq [[field value] [["rev" rev] ["hash" hash]]]
        (shell "ast-grep" "run"
               "--pattern" (str "{" field " = $VALUE;}")
               "--selector" "binding"
               "--rewrite" (str field " = \"" value "\";")
               "--update-all" package-file))
      (println "Updated ol-skills to" rev))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
