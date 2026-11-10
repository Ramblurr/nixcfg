#!/usr/bin/env bb
(ns scripts.tests.reforge-move-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(load-file (.getCanonicalPath (io/file (.getParentFile (io/file *file*)) "../reforge_move.clj")))
(require '[scripts.reforge-move :as move])

(defn- git [path & args]
  (:out (apply process/shell {:out :string :err :string}
               (into ["git" "-C" (str path) "-c" "commit.gpgsign=false"
                      "-c" "user.name=Test" "-c" "user.email=test@example.invalid"] args))))

(defn- init! [path]
  (fs/create-dirs path)
  (git path "init" "-b" "main")
  (git path "commit" "--allow-empty" "-m" "Initial"))

(defn- fixture [f]
  (let [root (fs/create-temp-dir {:prefix "reforge-move-test-"})
        source (str (fs/path root "source"))
        destination (str (fs/path root "new owner/repository"))]
    (try
      (init! source)
      (f {:root root :source source :destination destination
          :journal (str (fs/path root "journal.jsonl"))})
      (finally (fs/delete-tree root)))))

(deftest dry-run-preserves-checkout
  (fixture
   (fn [{:keys [source destination root] :as entry}]
     (spit (fs/file source "untracked") "precious")
     (let [before (git source "status" "--porcelain")
           inspected (move/plan [entry])
           manifest (fs/file root "manifest.json")]
       (spit manifest (json/generate-string {:moves [(select-keys entry [:source :destination])]}))
       (is (= {:errors [] :active []} (select-keys (first (:moves inspected)) [:errors :active])))
       (is (= "dry-run" (:mode (json/parse-string
                                (with-out-str (move/-main "--manifest" (str manifest))) true))))
       (is (= {:source true :destination false :status before :contents "precious"}
              {:source (fs/exists? source) :destination (fs/exists? destination)
               :status (git source "status" "--porcelain")
               :contents (slurp (fs/file source "untracked"))}))))))

(deftest moves-whole-directory-and-verifies
  (fixture
   (fn [{:keys [source destination journal] :as entry}]
     (spit (fs/file source ".gitignore") "ignored\n")
     (spit (fs/file source "tracked") "before")
     (git source "add" ".gitignore" "tracked")
     (git source "commit" "-m" "Files")
     (spit (fs/file source "tracked") "dirty")
     (spit (fs/file source "untracked") "local")
     (spit (fs/file source "ignored") "cached")
     (move/apply-plan! (move/plan [entry]) journal)
     (is (= {:source false :tracked "dirty" :untracked "local" :ignored "cached"}
            {:source (fs/exists? source)
             :tracked (slurp (fs/file destination "tracked"))
             :untracked (slurp (fs/file destination "untracked"))
             :ignored (slurp (fs/file destination "ignored"))}))
     (is (= ["start" "intent" "renamed" "verified"]
            (mapv #(get (json/parse-string %) "event") (line-seq (io/reader journal))))))))

(deftest repairs-nested-worktrees-and-preserves-missing-registration
  (fixture
   (fn [{:keys [source destination journal root] :as entry}]
     (let [nested (str (fs/path source ".worktrees/nested tree"))
           missing (str (fs/path root "missing"))]
       (git source "worktree" "add" "-b" "nested" nested)
       (git source "worktree" "add" "-b" "missing" missing)
       (fs/delete-tree missing)
       (spit (fs/file nested "local") "keep")
       (move/apply-plan! (move/plan [entry]) journal)
       (is (= "nested\n" (git (fs/path destination ".worktrees/nested tree") "branch" "--show-current")))
       (is (= "keep" (slurp (fs/file destination ".worktrees/nested tree/local"))))
       (is (str/includes? (git destination "worktree" "list" "--porcelain") missing))))))

(deftest preserves-initialized-submodule
  (fixture
   (fn [{:keys [source destination journal root] :as entry}]
     (let [child (str (fs/path root "child"))]
       (init! child)
       (git source "-c" "protocol.file.allow=always" "submodule" "add" child "deps/child")
       (git source "commit" "-am" "Submodule")
       (spit (fs/file source "deps/child/local") "untracked child")
       (let [before (git source "submodule" "status")]
         (move/apply-plan! (move/plan [entry]) journal)
         (is (= {:status before :content "untracked child"}
                {:status (git destination "submodule" "status")
                 :content (slurp (fs/file destination "deps/child/local"))})))))))

(deftest occupied-destination-and-mismatched-remote-refuse-apply
  (fixture
   (fn [{:keys [source destination journal] :as entry}]
     (fs/create-dirs destination)
     (spit (fs/file destination "keep") "existing")
     (is (thrown? Exception (move/apply-plan! (move/plan [entry]) journal)))
     (is (= {:source true :journal false :destination "existing"}
            {:source (fs/exists? source) :journal (fs/exists? journal)
             :destination (slurp (fs/file destination "keep"))}))
     (is (thrown? Exception
                  (move/apply-plan! (move/plan [(assoc entry :remote "origin" :expected_remote "https://invalid/repo")]) journal))))))

(deftest ignores-application-lock-file-but-refuses-git-index-lock
  (fixture
   (fn [{:keys [source] :as entry}]
     (fs/create-dirs (fs/path source ".git/gitbutler"))
     (spit (fs/file source ".git/gitbutler/project.lock") "")
     (is (= [] (:errors (first (:moves (move/plan [entry]))))))
     (spit (fs/file source ".git/index.lock") "")
     (is (= ["Git lock files exist"] (:errors (first (:moves (move/plan [entry])))))))))

(deftest preserves-missing-nested-registration
  (fixture
   (fn [{:keys [source destination journal] :as entry}]
     (let [missing (str (fs/path source ".worktrees/missing"))]
       (git source "worktree" "add" "-b" "missing" missing)
       (fs/delete-tree missing)
       (move/apply-plan! (move/plan [entry]) journal)
       (is (str/includes? (git destination "worktree" "list" "--porcelain") missing))))))

(deftest active-checkout-is-skipped
  (fixture
   (fn [{:keys [source destination journal] :as entry}]
     (let [child (process/process {:dir source :out :string :err :string} "sleep" "60")]
       (try
         (let [inspected (move/plan [entry])]
           (is (seq (:active (first (:moves inspected)))))
           (move/apply-plan! inspected journal)
           (is (= {:source true :destination false}
                  {:source (fs/exists? source) :destination (fs/exists? destination)})))
         (finally (process/destroy-tree child)))))))

(deftest refuses-drift-and-overlapping-manifest
  (fixture
   (fn [{:keys [source destination journal] :as entry}]
     (let [inspected (move/plan [entry])]
       (spit (fs/file source "new-file") "drift")
       (is (thrown? Exception (move/apply-plan! inspected journal)))
       (is (= {:source true :destination false}
              {:source (fs/exists? source) :destination (fs/exists? destination)})))
     (is (thrown? Exception (move/plan [entry entry])))
     (is (thrown? Exception (move/plan [(assoc entry :destination (str source "/nested"))]))))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests 'scripts.tests.reforge-move-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))