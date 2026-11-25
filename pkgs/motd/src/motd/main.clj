(ns motd.main
  "CLI entrypoint for motd."
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [motd.collect :as collect]
   [motd.dummy :as dummy]
   [motd.render :as render]))

(def ^:private default-config-path "/etc/motd/config.edn")

(def ^:private default-config
  {:role "host" :units [] :disks nil})

(def ^:private cli-spec
  {:config   {:desc "Config file (default: MOTD_CONFIG or /etc/motd/config.edn)"}
   :demo     {:desc "Render dummy data profile: host, microvm, clean"}
   :profile  {:alias :p
              :desc  "Alias for --demo"}
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
    "      --config <path>   Config file (default: MOTD_CONFIG or /etc/motd/config.edn)"
    "      --demo <name>     Render dummy data: host, microvm, clean"
    "  -p, --profile <name>  Alias for --demo"
    "      --color           Force ANSI color output"
    "      --no-color        Disable ANSI color output"
    "      --ascii           Use ASCII symbols instead of Unicode"
    "      --width <n>       Target column width (default: COLUMNS or 76)"
    "      --json            Print state data as JSON"
    "      --edn             Print state data as EDN"
    "  -h, --help            Show this help"
    ""
    "Config file keys (EDN):"
    "  :role   Role label shown in the header, such as \"host\" or \"microvm\""
    "  :units  Curated systemd unit names to show"
    "  :disks  Mount points to show, or nil for all real mounts"
    ""
    "Examples:"
    "  motd"
    "  motd --demo microvm"
    "  NO_COLOR=1 motd"
    "  motd --no-color --ascii"]))

(defn- fail!
  [& lines]
  (binding [*out* *err*]
    (run! println lines))
  (System/exit 2))

(defn- load-config
  "Return the config map. An explicit path must exist; the default path is optional."
  [{:keys [config]}]
  (let [explicit (or config (System/getenv "MOTD_CONFIG"))
        path     (or explicit default-config-path)]
    (cond
      (fs/exists? path)
      (try
        (merge default-config (edn/read-string (slurp path)))
        (catch Exception e
          (fail! (str "error: cannot read config " path ": " (ex-message e)))))

      explicit (fail! (str "error: config file not found: " path))
      :else    default-config)))

(defn- stdout-tty? []
  (some? (System/console)))

(defn- resolve-color?
  [{:keys [color no-color]}]
  (cond
    no-color                           false
    color                              true
    (some? (System/getenv "NO_COLOR")) false
    (= "dumb" (System/getenv "TERM"))  false
    :else                              (stdout-tty?)))

(defn- resolve-width
  [{:keys [width]}]
  (or width
      (some-> (System/getenv "COLUMNS") parse-long)
      76))

(defn- resolve-state
  [opts]
  (if-let [profile (or (:demo opts) (:profile opts))]
    (or (get dummy/profiles (keyword profile))
        (fail! (str "error: unknown profile '" profile "'")
               "Valid profiles: host, microvm, clean."))
    (collect/collect-state (load-config opts))))

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})]
    (cond
      (:help opts)
      (println (help-text))

      (and (:json opts) (:edn opts))
      (fail! "error: --json and --edn cannot be used together")

      :else
      (let [state (resolve-state opts)]
        (cond
          (:json opts) (println (json/generate-string state {:pretty true}))
          (:edn opts)  (pp/pprint state)
          :else        (print (render/render state
                                             {:color?   (resolve-color? opts)
                                              :unicode? (not (:ascii opts))
                                              :width    (resolve-width opts)})))
        (flush)
        (shutdown-agents)))))
