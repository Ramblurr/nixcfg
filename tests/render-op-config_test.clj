(load-file (str (.getParent (java.io.File. *file*)) "/../scripts/render-op-config.clj"))

#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns render-op-config-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]
            [user :as sut]))

(def ^:private script
  (str (fs/path (fs/parent *file*) ".." "scripts" "render-op-config.clj")))

(def ^:private fake-reader
  "Fake `op read`: logs each reference and answers from a fixed table. Failure
  cases also write marker text that must never reach logs or exceptions."
  "#!/usr/bin/env bash
echo \"$1\" >> \"$(dirname \"$0\")/calls.log\"
case \"$1\" in
  'op://test/client/credential') echo 'client-key-fixture' ;;
  'op://test/upstream/credential') printf '  upstream-key-fixture\\n\\n' ;;
  'op://test/with space/credential') echo 'spaced-key-fixture' ;;
  'op://test/empty/credential') printf ' \\n\\t\\n' ;;
  *) echo 'LEAKED-STDOUT'; echo 'LEAKED-STDERR' >&2; exit 1 ;;
esac
")

(defn- fixture
  "Returns paths for a scratch directory holding a fake reader and template."
  [dir definition]
  (let [reader (fs/path dir "reader")
        template (fs/path dir "template.json")
        output-dir (fs/create-dirs (fs/path dir "run"))]
    (spit (fs/file reader) fake-reader)
    (fs/set-posix-file-permissions reader "rwx------")
    (spit (fs/file template) (json/generate-string definition))
    {:reader (str reader)
     :template (str template)
     :output-dir output-dir
     :output (str (fs/path output-dir "config.yaml"))
     :calls (fs/path dir "calls.log")}))

(defn- reader-calls [{:keys [calls]}]
  (if (fs/exists? calls) (str/split-lines (slurp (fs/file calls))) []))

(defn- dir-entries [dir]
  (sort (map (comp str fs/file-name) (fs/list-dir dir))))

(deftest resolves-nested-references
  (fs/with-temp-dir [dir {}]
    (let [{:keys [template reader output output-dir] :as f}
          (fixture dir {:settings {:config-version 8
                                   :server {:host "127.0.0.1" :port 8317}
                                   :api-keys ["op://test/client/credential"
                                              "op://test/with space/credential"]
                                   :openai-compatibility
                                   [{:name "openrouter"
                                     :api-key-entries [{:api-key "op://test/upstream/credential"}]
                                     :models [{:name "z-ai/glm-5.3-flash"}]}]
                                   :note "not op:// at start"
                                   :debug false
                                   :proxy nil}})]
      (sut/generate! template reader output)
      (is (= {"config-version" 8
              "server" {"host" "127.0.0.1" "port" 8317}
              "api-keys" ["client-key-fixture" "spaced-key-fixture"]
              "openai-compatibility"
              [{"name" "openrouter"
                "api-key-entries" [{"api-key" "upstream-key-fixture"}]
                "models" [{"name" "z-ai/glm-5.3-flash"}]}]
              "note" "not op:// at start"
              "debug" false
              "proxy" nil}
             (json/parse-string (slurp output))))
      (is (str/ends-with? (slurp output) "\n"))
      (is (= "rw-------" (fs/posix->str (fs/posix-file-permissions output))))
      (is (= ["config.yaml"] (dir-entries output-dir)) "no temporary file remains")
      (is (= #{"op://test/client/credential"
               "op://test/upstream/credential"
               "op://test/with space/credential"}
             (set (reader-calls f)))))))

(defn- assert-rejected
  "Runs `generate!` against a pre-existing config and checks that it fails
  without leaking marker text, touching the config, or leaving temp files."
  [settings expected-message]
  (fs/with-temp-dir [dir {}]
    (let [{:keys [template reader output output-dir]}
          (fixture dir {:settings settings})]
      (spit output "{\"previous\": true}\n")
      (let [error (try (sut/generate! template reader output) nil
                       (catch Exception e e))]
        (is (some? error))
        (is (= expected-message (ex-message error)))
        (is (not-any? #(str/includes? (ex-message error) %)
                      ["LEAKED" "client-key-fixture"])))
      (is (= "{\"previous\": true}\n" (slurp output)))
      (is (= ["config.yaml"] (dir-entries output-dir))))))

(defn- render-skipping
  "Renders `settings` and returns the parsed config and the skip warnings."
  [settings]
  (fs/with-temp-dir [dir {}]
    (let [{:keys [template reader output]}
          (fixture dir {:settings settings})
          warnings (java.io.StringWriter.)]
      (binding [*err* warnings] (sut/generate! template reader output))
      {:config (json/parse-string (slurp output))
       :warnings (str warnings)})))

(deftest skips-failed-and-empty-reads
  (let [client "op://test/client/credential"]
    (doseq [[label reference message]
            [["reader exits non-zero" "op://test/missing/credential"
              "Cannot resolve op://test/missing/credential"]
             ["reader prints only whitespace" "op://test/empty/credential"
              "Empty credential: op://test/empty/credential"]]]
      (testing label
        (let [{:keys [config warnings]}
              (render-skipping {:access {:api-keys [client reference]}})]
          (is (= {"access" {"api-keys" ["client-key-fixture"]}} config))
          (is (= (str "Skipping credential: " message "\n") warnings))
          (is (not (str/includes? warnings "LEAKED")))))))
  (testing "an upstream that loses its only key keeps its other settings"
    (is (= {"access" {"api-keys" ["client-key-fixture"]}
            "openai-compatibility" [{"name" "mistral"
                                     "api-key-entries" []
                                     "models" [{"name" "mistral-large-4"}]}]
            "routing" {}}
           (:config (render-skipping {:access {:api-keys ["op://test/client/credential"]}
                                      :openai-compatibility
                                      [{:name "mistral"
                                        :api-key-entries [{:api-key "op://test/missing/credential"}]
                                        :models [{:name "mistral-large-4"}]}]
                                      :routing {}}))))))

(deftest rejects-a-config-with-no-client-key
  (assert-rejected {:access {:api-keys ["op://test/missing/credential" "op://test/empty/credential"]}}
                   "No client API key resolved")
  (assert-rejected {:access {:api-keys ["   " "\t"]}}
                   "No client API key resolved"))

(deftest command-line-failure-is-quiet
  (fs/with-temp-dir [dir {}]
    (let [{:keys [template reader output]}
          (fixture dir {:settings {:access {:api-keys ["op://test/missing/credential"]}}})
          {:keys [exit out err]} (process/shell {:out :string :err :string :continue true}
                                                "bb" script template reader output)]
      (is (= 1 exit))
      (is (= "" out))
      (is (= (str "Skipping credential: Cannot resolve op://test/missing/credential\n"
                  "Config generation failed: No client API key resolved\n")
             err))
      (is (not (fs/exists? output))))))

(deftest removes-temporary-file-when-replacement-fails
  (fs/with-temp-dir [dir {}]
    (let [output (fs/path dir "config.yaml")]
      ;; A non-empty directory cannot be replaced, so the move throws.
      (fs/create-dirs output)
      (spit (fs/file output "keep") "x")
      (is (thrown? Exception (sut/write-config! {"a" 1} (str output))))
      (is (= ["config.yaml"] (dir-entries dir)))
      (is (= "x" (slurp (fs/file output "keep")))))))

(deftest command-line-success
  (fs/with-temp-dir [dir {}]
    (let [{:keys [template reader output]}
          (fixture dir {:settings {:a "op://test/client/credential"}})
          {:keys [exit out err]} (process/shell {:out :string :err :string :continue true}
                                                "bb" script template reader output)]
      (is (= [0 "" ""] [exit out err]))
      (is (= {"a" "client-key-fixture"} (json/parse-string (slurp output)))))))

(let [{:keys [fail error]} (run-tests 'render-op-config-test)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
