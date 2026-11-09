#!/usr/bin/env bb
(ns scripts.tests.reforge-test
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is run-tests]]))

(load-file (.getCanonicalPath (io/file (.getParentFile (io/file *file*)) "../reforge.clj")))
(require 'scripts.reforge)

(def opts {:source-owner "source" :host "forge.example" :owner "destination"
           :branches "all" :unpublished "private" :exclude #{}})

(defn- with-repo [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "reforge-test" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (process/shell {:out :string :err :string} "git" "init" "-b" "main" (str dir))
      (process/shell {:out :string :err :string} "git" "-C" (str dir)
                     "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                     "-c" "commit.gpgsign=false" "commit" "--allow-empty" "-m" "Initial")
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))] (io/delete-file file))))))

(deftest cli-options
  (is (= {:exclude #{"a" "b"} :rename {"old" "new"} :apply true :branches "all"}
         (#'scripts.reforge/options ["--exclude" "a" "--exclude" "b"
                                     "--rename" "old=new" "--apply" "--branches" "all"])))
  (is (thrown? Exception (#'scripts.reforge/options ["--unknown" "value"])))
  (is (thrown? Exception (#'scripts.reforge/options ["unexpected"]))))

(deftest source-names
  (is (= [".github" "project" "project" nil]
         (mapv #(#'scripts.reforge/github-repo % "source")
               ["https://github.com/source/.github.git"
                "git@github.com:source/project.git"
                "ssh://git@github.com/source/project"
                "https://github.com/another/project.git"]))))

(deftest unpublished-plan-is-private-and-read-only
  (with-repo
    (fn [dir]
      (let [before (slurp (io/file dir ".git/config"))
            {:keys [visibility commands]} (#'scripts.reforge/repo-plan opts dir)
            target (str "https://forge.example/destination/" (.getName dir) ".git")]
        (is (= {:visibility "private"
                :create ["fj" "-H" "forge.example" "org" "repo" "create"
                         "destination" (.getName dir) "--private"]
                :push ["git" "-C" (str dir) "push" "--atomic" target
                       "refs/heads/*:refs/heads/*" "refs/tags/*:refs/tags/*"]}
               {:visibility visibility :create (first commands) :push (second commands)}))
        (is (= before (slurp (io/file dir ".git/config")))))
      (is (thrown-with-msg? Exception #"choose --unpublished"
                            (#'scripts.reforge/repo-plan (dissoc opts :unpublished) dir))))))

(deftest github-visibility-and-remote-order
  (with-repo
    (fn [dir]
      (#'scripts.reforge/git dir "remote" "add" "origin" "https://github.com/source/project.git")
      (let [capture @#'scripts.reforge/capture]
        (doseq [private? [true false]]
          (with-redefs [scripts.reforge/capture
                        (fn [argv]
                          (if (= "gh" (first argv))
                            (json/generate-string {:name "project" :isPrivate private?
                                                   :defaultBranchRef {:name "main"}})
                            (capture argv)))]
            (let [{:keys [repo visibility commands]} (#'scripts.reforge/repo-plan opts dir)]
              (is (= {:repo "project" :visibility (if private? "private" "public")
                      :private-flag private? :operations ["push" "remote" "remote" "ls-remote"]}
                     {:repo repo :visibility visibility
                      :private-flag (= "--private" (last (first commands)))
                      :operations (mapv #(nth % 3) (filter #(= "git" (first %)) commands))})))))))))

(deftest destination-name-override
  (with-repo
    (fn [dir]
      (let [name (.getName dir)
            parsed (#'scripts.reforge/options ["--rename" (str name "renamed=other")
                                               "--rename" (str name "=_github")])
            {:keys [repo commands]} (#'scripts.reforge/repo-plan
                                     (merge opts parsed) dir)]
        (is (= {:repo "_github"
                :create ["fj" "-H" "forge.example" "org" "repo" "create"
                         "destination" "_github" "--private"]
                :target "https://forge.example/destination/_github.git"}
               {:repo repo :create (first commands) :target (nth (second commands) 5)})))))
  (is (thrown? Exception (#'scripts.reforge/options ["--rename" "foo=../bar"]))))

(deftest conflicts-are-rejected
  (with-repo
    (fn [dir]
      (#'scripts.reforge/git dir "remote" "add" "origin" "https://github.com/source/project.git")
      (#'scripts.reforge/git dir "remote" "add" "upstream" "https://github.com/other/project.git")
      (is (thrown-with-msg? Exception #"upstream already exists"
                            (#'scripts.reforge/repo-plan opts dir))))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests 'scripts.tests.reforge-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
