#!/usr/bin/env bb
(ns scripts.tests.generate-readme-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests testing]]))

(def script (.getCanonicalPath (io/file (.getParentFile (io/file *file*)) "../generate-readme.clj")))
(load-file script)
(require '[scripts.generate-readme :as generator])

(def source "Intro\r\n<!-- BEGIN HOSTS -->\r\nold\r\n<!-- END HOSTS -->\r\nFooter\r\n")
(def host {"purpose" "Storage NAS" "channel" "stable" "role" "server" "os" "nixos"})

(deftest rendering-test
  (let [inventory {"zeta" host
                   "alpha" (merge host {"purpose" "Workstation" "board" "Example board"
                                        "cpu" "Example CPU" "ramMiB" 65536 "gpu" "Example GPU"
                                        "channel" "unstable" "role" "desktop"})}
        result (generator/render-readme source inventory)]
    (is (= ["| Hostname | Purpose | Board | CPU | RAM | GPU | Channel | Role | OS |"
            "|---|---|---|---|---|---|---|---|---|"
            "| [alpha](./hosts/alpha/) | Workstation | Example board | Example CPU | 64 GiB | Example GPU | unstable | <span title=\"Desktop\">&#x1F5A5;&#xFE0F;</span> | <span title=\"NixOS\">&#x2744;&#xFE0F;</span> |"
            "| [zeta](./hosts/zeta/) | Storage NAS | Unknown | Unknown | Unknown | Unknown | stable | <span title=\"Bare-metal server\">&#x1F5C4;&#xFE0F;</span> | <span title=\"NixOS\">&#x2744;&#xFE0F;</span> |"]
           (filterv #(str/starts-with? % "|") (str/split-lines result))))
    (is (str/starts-with? result "Intro\r\n<!-- BEGIN HOSTS -->\n\n"))
    (is (str/ends-with? result "<!-- END HOSTS -->\r\nFooter\r\n"))
    (is (= result (generator/render-readme result inventory)))))

(deftest visibility-and-memory-test
  (let [result (generator/render-readme source {"visible" (assoc host "ramMiB" 8000)
                                                "hidden" {"showInReadme" false}})]
    (is (= ["| [visible](./hosts/visible/) | Storage NAS | Unknown | Unknown | 8000 MiB | Unknown | stable | <span title=\"Bare-metal server\">&#x1F5C4;&#xFE0F;</span> | <span title=\"NixOS\">&#x2744;&#xFE0F;</span> |"]
           (filterv #(str/starts-with? % "| [") (str/split-lines result)))))
  (is (not (str/includes? (generator/render-readme source {}) "- &#x2744;"))))

(deftest escaping-test
  (let [result (generator/render-readme source
                                        {"example" (assoc host
                                                          "purpose" "A | B\n<script>[link]*"
                                                          "documentation" "https://example.org/docs_(host)")})]
    (is (str/includes? result "[A &#124; B &lt;script&gt;\\[link\\]\\*](https://example.org/docs_%28host%29)")))
  (is (= "https://example.org/%C3%A9%20%2B" (generator/documentation-url "https://example.org/é +"))))

(deftest invalid-inventory-test
  (doseq [[field value] [["channel" "stabel"] ["role" "sever"] ["os" "unknown"]
                         ["ramMiB" -1] ["ramMiB" 0] ["ramMiB" true] ["ramMiB" 1.5]
                         ["showInReadme" "false"] ["showInReadme" nil]
                         ["documentation" "javascript:alert(1)"]]]
    (testing (str field " " value)
      (is (thrown? Exception (generator/render-readme source {"example" (assoc host field value)})))))
  (is (thrown? Exception (generator/render-readme source {"../escape" host})))
  (is (thrown? Exception (generator/render-readme source {"example" (dissoc host "purpose")}))))

(deftest cli-test
  (let [directory (.toFile (java.nio.file.Files/createTempDirectory
                            "generate-readme-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        readme (io/file directory "README.md")
        inventory (io/file directory "inventory.json")
        command ["bb" script "--readme" (str readme) "--inventory-json" (str inventory)]
        run (fn [& args] (apply shell/sh (concat command args)))]
    (try
      (spit inventory (json/generate-string {"test" host}))
      (spit readme source)
      (let [check (run "--check")]
        (is (= [1 source] [(:exit check) (slurp readme)]))
        (is (str/includes? (:err check) "stale")))
      (is (= 0 (:exit (run))))
      (let [generated (slurp readme)]
        (is (= (generator/render-readme source {"test" host}) generated))
        (is (= [0 generated] [(:exit (run "--check")) (slurp readme)]))
        (spit inventory (json/generate-string {"test" (assoc host "purpose" "Changed")}))
        (is (= [1 generated] [(:exit (run "--check")) (slurp readme)])))
      (doseq [invalid ["No markers"
                       "<!-- END HOSTS -->\n<!-- BEGIN HOSTS -->"
                       "<!-- BEGIN HOSTS --><!-- BEGIN HOSTS --><!-- END HOSTS -->"
                       "<!-- END HOSTS --><!-- BEGIN HOSTS --><!-- END HOSTS -->"]]
        (spit readme invalid)
        (let [result (run)]
          (is (= [1 invalid] [(:exit result) (slurp readme)]))
          (is (str/includes? (:err result) "markers"))))
      (is (= 0 (:exit (run "--help"))))
      (is (= 1 (:exit (run "--unknown"))))
      (finally
        (doseq [file [readme inventory directory]]
          (io/delete-file file true))))))

(let [{:keys [fail error]} (run-tests 'scripts.tests.generate-readme-test)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
