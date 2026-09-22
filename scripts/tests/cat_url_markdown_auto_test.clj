#!/usr/bin/env bb
(ns scripts.tests.cat-url-markdown-auto-test
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test])
  (:import [java.nio.file FileAlreadyExistsException Files]
           [java.nio.file.attribute FileAttribute]))

(load-file (str (io/file (.getParentFile (io/file *file*)) "../cat_url_markdown_auto.clj")))
(require '[scripts.cat-url-markdown-auto :as auto])

(test/deftest safe-filenames
  (test/is (= "idiomatic-clojure-errors.md"
              (auto/filename "  idiomatic-clojure-errors\n")))
  (doseq [output ["" "../escape" "/tmp/escape" "Title" "two--hyphens"
                  "slug.md" "slug\nexplanation" "`slug`" (apply str (repeat 121 "a"))]]
    (test/is (thrown? Exception (auto/filename output)))))

(test/deftest saves-once-and-preserves-existing-files
  (let [directory (.toFile (Files/createTempDirectory "markdown-auto-test-"
                                                     (make-array FileAttribute 0)))
        destination (io/file directory "with spaces")
        target (io/file destination "useful-title.md")
        markdown "Title: Useful title\r\n\r\nBody with ünicode.\r\n"
        calls (atom [])]
    (try
      (with-redefs [process/shell
                    (fn [opts command & args]
                      (swap! calls conj (into [command] args))
                      (test/is (= {:out :string :in ""} opts))
                      {:exit 0 :out (case command
                                     "cat-url-markdown" markdown
                                     "pi" "useful-title\n")})]
        (test/is (= (str target) (auto/save-markdown "https://example.org/123" (str destination))))
        (test/is (= markdown (slurp target)))
        (test/is (= ["cat-url-markdown" "pi"] (mapv first @calls)))
        (let [prompt (last (second @calls))]
          (test/is (and (str/includes? prompt "https://example.org/123")
                        (str/includes? prompt "Title: Useful title")
                        (not (str/includes? prompt "Body with")))))
        (test/is (thrown? FileAlreadyExistsException
                         (auto/save-markdown "https://example.org/123" (str destination))))
        (test/is (= markdown (slurp target))))
      (doseq [result ["" "  \n"]]
        (with-redefs [process/shell (fn [& _] {:exit 0 :out result})]
          (test/is (thrown-with-msg? Exception #"empty Markdown"
                                    (auto/save-markdown "https://example.org" (str destination))))))
      (with-redefs [process/shell (fn [& _] (throw (ex-info "subprocess failed" {:exit 1})))]
        (test/is (thrown-with-msg? Exception #"subprocess failed"
                                  (auto/save-markdown "https://example.org" (str destination)))))
      (finally
        (doseq [file (reverse (file-seq directory))]
          (io/delete-file file))))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (test/run-tests 'scripts.tests.cat-url-markdown-auto-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
