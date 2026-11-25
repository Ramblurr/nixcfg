(ns motd.main
  "CLI entrypoint for motd."
  (:require
   [babashka.cli :as cli]
   [cheshire.core :as json]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [motd.dummy :as dummy]
   [motd.render :as render]))

(def ^:private cli-spec
  {:profile  {:alias   :p
              :default "host"
              :desc    "Dummy data profile: host, microvm, clean"}
   :color    {:coerce :boolean
              :desc   "Force ANSI color output"}
   :no-color {:coerce :boolean
              :desc   "Disable ANSI color output"}
   :ascii    {:coerce :boolean
              :desc   "Use ASCII symbols instead of Unicode"}
   :width    {:coerce :long
              :desc   "Target column width (default: COLUMNS or 76)"}
   :json     {:coerce :boolean
              :desc   "Print raw state data as JSON"}
   :edn      {:coerce :boolean
              :desc   "Print raw state data as EDN"}
   :help     {:alias  :h
              :coerce :boolean
              :desc   "Show this help"}})

(defn- help-text []
  (str/join
   "\n"
   ["Usage: motd [OPTIONS]"
    ""
    "Display host status and curated systemd unit health."
    ""
    "Options:"
    "  -p, --profile <name>  Dummy profile: host, microvm, clean (default: host)"
    "      --color           Force ANSI color output"
    "      --no-color        Disable ANSI color output"
    "      --ascii           Use ASCII symbols instead of Unicode"
    "      --width <n>       Target column width (default: COLUMNS or 76)"
    "      --json            Print state data as JSON"
    "      --edn             Print state data as EDN"
    "  -h, --help            Show this help"
    ""
    "Examples:"
    "  motd"
    "  motd --profile microvm"
    "  NO_COLOR=1 motd"
    "  motd --no-color --ascii"]))

(defn- stdout-tty? []
  (some? (System/console)))

(defn- resolve-color?
  [{:keys [color no-color]}]
  (cond
    no-color                        false
    color                           true
    (some? (System/getenv "NO_COLOR")) false
    (= "dumb" (System/getenv "TERM"))  false
    :else                           (stdout-tty?)))

(defn- resolve-width
  [{:keys [width]}]
  (or width
      (some-> (System/getenv "COLUMNS") parse-long)
      76))

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})]
    (cond
      (:help opts)
      (println (help-text))

      (and (:json opts) (:edn opts))
      (do
        (binding [*out* *err*]
          (println "error: --json and --edn cannot be used together"))
        (System/exit 2))

      :else
      (let [profile-key (keyword (:profile opts))
            state       (get dummy/profiles profile-key)]
        (if-not state
          (do
            (binding [*out* *err*]
              (println (str "error: unknown profile '" (:profile opts) "'"))
              (println "Valid profiles: host, microvm, clean."))
            (System/exit 2))
          (cond
            (:json opts) (println (json/generate-string state {:pretty true}))
            (:edn opts)  (pp/pprint state)
            :else        (print (render/render state
                                               {:color?   (resolve-color? opts)
                                                :unicode? (not (:ascii opts))
                                                :width    (resolve-width opts)}))))))))
