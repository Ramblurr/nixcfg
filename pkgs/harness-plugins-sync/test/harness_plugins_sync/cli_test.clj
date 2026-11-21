(ns harness-plugins-sync.cli-test
  (:require
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [harness-plugins-sync.cli :as sut]
   [harness-plugins-sync.terminal :as term]
   [malli.core :as m]))

(def catalog
  {:name        "tool"
   :description "Manage project build artifacts."
   :examples    ["tool image list"]
   :options     {:project-dir {:alias :C :coerce :string :ref "<path>"
                               :desc "Project directory (default: current directory)"}
                 :name        {:coerce :string :ref "<name>" :require true :desc "Name for the image"}
                 :tag         {:coerce [:string] :ref "<tag>" :desc "Tag to add (repeatable)"}
                 :check       {:coerce :string :schema [:fn string?] :desc "Untranslatable check"}}
   :commands
   [{:path ["image"] :summary "Build and inspect images" :description "Build and inspect project images."}
    {:path        ["image" "build"]
     :summary     "Build an image from the current project"
     :description "Build an image from the selected project and register it by name."
     :options     [:project-dir :name :tag]
     :examples    ["tool image build --name dev" "tool image build -C ./app --name ci"]
     :output      [:map {:registry {::image [:map [:name :string] [:tags [:set :keyword]]]}}
                   [:image [:ref ::image]]]
     :errors      {:image/exists "An image with that name already exists"}
     :handler     :image/build}
    {:path ["image" "list"] :summary "List available images" :options [:check]
     :output [:map [:images [:vector :string]]] :handler :image/list}
    {:path ["exec"] :summary "Run a program" :child-args {:ref "<program> [args...]"}
     :raw-output "Program output, unchanged" :handler :exec}]})

(def calls (atom []))

(def impl
  {:image/build (fn [{:keys [name tag] :as input}]
                  (swap! calls conj input)
                  (if (= "taken" name)
                    (throw (ex-info "image 'taken' already exists" {:code :image/exists :details {:name name}}))
                    {:image {:name name :tags (set (map keyword tag))}}))
   :image/list  (fn [_] (swap! calls conj :list) {:images ["dev"]})
   :exec        (fn [{:keys [argv]}] (swap! calls conj argv) 7)})

(def plain {:color-out? false :color-err? false :animate? false})

(defn invoke
  "Runs `args` as if no stream were a terminal and returns `{:exit :out :err}`."
  [& args]
  (let [err  (java.io.StringWriter.)
        exit (volatile! nil)
        out  (with-out-str
               (binding [*err* err]
                 (vreset! exit (sut/run catalog impl (vec args) {:terminal plain}))))]
    {:exit @exit :out out :err (str err)}))

(deftest help
  (reset! calls [])
  (testing "help forms agree at every level and call no handler"
    (doseq [path [[] ["image"] ["image" "build"]]]
      (let [results (map #(apply invoke %) [(into ["help"] path) (conj path "--help") (conj path "-h")])]
        (is (apply = results) (str path))
        (is (= 0 (:exit (first results)))))))
  (is (= [] @calls))
  (testing "leaf help matches the contract layout"
    (is (= (str "Usage: tool image build [OPTIONS]\n\n"
                "Build an image from the selected project and register it by name.\n\n"
                "Options:\n"
                "  -C, --project-dir <path>  Project directory (default: current directory)\n"
                "      --name <name>         Name for the image (required)\n"
                "      --tag <tag>           Tag to add (repeatable)\n"
                "      --schema <kind>       Describe input, output, error or all contracts\n"
                "      --json                Print results as JSON (schemas: JSON Schema)\n"
                "      --edn                 Print results as EDN (schemas: Malli)\n"
                "  -h, --help                Show this help\n\n"
                "Examples:\n"
                "  tool image build --name dev\n"
                "  tool image build -C ./app --name ci\n")
           (:out (invoke "image" "build" "-h")))))
  (testing "group help lists children and navigation"
    (is (str/includes? (:out (invoke "image"))
                       "Commands:\n  build  Build an image from the current project\n  list   List available images\n"))
    (is (str/ends-with? (:out (invoke "image"))
                        "Run 'tool help image <command>' for more information on a command.\n"))))

(deftest usage-errors
  (reset! calls [])
  (doseq [[args code] [[["image" "nope"] "cli/unknown-command"]
                       [["help" "image" "nope"] "cli/unknown-command"]
                       [["image" "list" "--name" "x"] "cli/unknown-option"]
                       [["image" "list" "help"] "cli/unexpected-argument"]
                       [["image" "list" "--" "x"] "cli/unexpected-argument"]
                       [["image" "build"] "cli/invalid-input"]
                       [["image" "list" "--schema" "everything"] "cli/usage"]]]
    (let [{:keys [exit out]} (apply invoke "--json" args)]
      (is (= 2 exit) (str args))
      (is (= code (get-in (json/read-str out) ["error" "code"])) (str args))))
  (testing "both output flags conflict"
    (let [{:keys [exit out err]} (invoke "--json" "image" "list" "--edn")]
      (is (= [2 ""] [exit out]))
      (is (str/starts-with? err "error: choose either --json or --edn"))))
  (is (= [] @calls) "no handler runs for a usage error"))

(deftest results
  (testing "globals work before and after the path; JSON and EDN agree"
    (let [edn-out  (edn/read-string (:out (invoke "--edn" "image" "build" "--name" "dev" "--tag" "a" "--tag" "b")))
          json-out (json/read-str (:out (invoke "image" "build" "--tag" "a" "--json" "--name" "dev" "--tag" "b")))]
      (is (= {:image {:name "dev" :tags #{:a :b}}} edn-out))
      (is (= (sut/->json-data edn-out) json-out))
      (is (nil? (m/explain (get-in catalog [:commands 1 :output]) edn-out)))))
  (testing "an expected application error is structured and valid"
    (let [{:keys [exit out]} (invoke "image" "build" "--name" "taken" "--edn")
          data               (edn/read-string out)]
      (is (= 1 exit))
      (is (= {:error {:code :image/exists :message "image 'taken' already exists" :details {:name "taken"}}} data))
      (is (nil? (m/explain (sut/error-schema catalog ["image" "build"]) data)))))
  (testing "an earlier failure does not leak into a later run"
    (invoke "image" "list" "--bogus")
    (is (= {:exit 0 :out "images:\n  - dev\n" :err ""} (invoke "image" "list")))))

(deftest child-args
  (reset! calls [])
  (testing "arguments after -- reach the handler unchanged and its status is returned"
    (is (= {:exit 7 :out "" :err ""} (invoke "exec" "--json" "--" "" "--help" "-x" "--schema" "all")))
    (is (= [["" "--help" "-x" "--schema" "all"]] @calls))))

(deftest schema-discovery
  (reset! calls [])
  (testing "works without required options and calls no handler"
    (let [{:keys [exit out]} (invoke "image" "build" "--schema" "all" "--edn")
          data               (edn/read-string out)]
      (is (= 0 exit))
      (is (= [:map
              [:project-dir {:optional true :description "Project directory (default: current directory)"} :string]
              [:name {:description "Name for the image"} :string]
              [:tag {:optional true :description "Tag to add (repeatable)"} [:vector :string]]]
             (get-in data [:input :schema])))
      (is (= #{:input :output :error} (set (filter #{:input :output :error} (keys data)))))))
  (testing "JSON selects JSON Schema with definitions"
    (let [data (json/read-str (:out (invoke "image" "build" "--schema" "output" "--json")))]
      (is (= "json-schema" (get data "schema-language")))
      (is (contains? (get-in data ["output" "schema" "definitions"]) "harness-plugins-sync.cli-test.image"))))
  (testing "untranslatable constraints are listed and the EDN stays readable"
    (let [data (edn/read-string (:out (invoke "image" "list" "--schema" "input" "--edn")))]
      (is (= [{:path [:check] :type :fn
               :reason "Predicate or custom type with no data or JSON Schema translation; validated at runtime only"}]
             (get-in data [:input :limitations])))))
  (testing "groups list subcommands"
    (is (= [{:name "build" :summary "Build an image from the current project"}
            {:name "list" :summary "List available images"}]
           (:subcommands (edn/read-string (:out (invoke "--schema" "input" "--edn" "image")))))))
  (is (= [] @calls)))

(deftest terminal-without-tty
  (testing "captured output has no ANSI escapes or animation; errors propagate"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err term/*terminal* plain]
        (is (= :done (term/with-progress "Work" (term/step "phase 1") :done)))
        (is (thrown-with-msg? Exception #"boom" (term/with-progress "Fail" (throw (ex-info "boom" {}))))))
      (is (re-matches #"(?s)Work\.\.\.\nphase 1\nWork: done \(\d+\.\ds\)\nFail\.\.\.\nFail: failed\n" (str err)))
      (is (not (str/includes? (str err) "\u001b"))))))
