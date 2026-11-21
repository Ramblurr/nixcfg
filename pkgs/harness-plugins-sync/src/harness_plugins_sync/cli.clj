(ns harness-plugins-sync.cli
  "Runs a CLI from a declarative command catalog.

  The catalog is data: command paths, help text, option keys, positional
  arguments, Malli output schemas, error codes and handler keys. [[run]] parses
  arguments with `babashka.cli`, answers help and `--schema` requests without
  calling handlers, validates input, calls the handler and prints its result as
  human text, JSON or EDN. It returns an exit status and never exits the process."
  (:require
   [babashka.cli :as cli]
   [clojure.data.json :as json]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [harness-plugins-sync.terminal :as term]
   [malli.core :as m]
   [malli.error :as me]
   [malli.json-schema :as js]
   [malli.util :as mu]))

(def global-options
  "Options the toolkit adds to every command level."
  {:schema {:coerce :string :ref "<kind>"
            :desc   "Describe input, output, error or all contracts"}
   :json   {:coerce :boolean :desc "Print results as JSON (schemas: JSON Schema)"}
   :edn    {:coerce :boolean :desc "Print results as EDN (schemas: Malli)"}
   :help   {:alias :h :coerce :boolean :desc "Show this help"}})

(def ^:private global-keys [:schema :json :edn :help])

(def usage-errors
  "Error codes the toolkit reports for any command. All exit with status 2."
  {:cli/unknown-command     "The command path does not exist"
   :cli/unknown-option      "An option is not accepted by this command"
   :cli/unexpected-argument "A positional argument or `--` is not accepted"
   :cli/invalid-input       "An option or argument is missing or has an invalid value"
   :cli/usage               "Options conflict or a value such as the schema kind is not valid"})

(defn- command [catalog path]
  (if (= [] path)
    (assoc (select-keys catalog [:description :examples]) :path [])
    (some #(when (= path (:path %)) %) (:commands catalog))))

(defn- children [catalog path]
  (filter #(= path (pop (:path %))) (:commands catalog)))

(defn- group? [catalog path]
  (or (= [] path) (boolean (seq (children catalog path)))))

(defn- option-def [catalog k]
  (or (get global-options k)
      (get-in catalog [:options k])
      (throw (ex-info (str "Catalog has no option " k) {:option k}))))

(defn- option-keys [cmd]
  (into (vec (:options cmd)) global-keys))

(defn- command-name [catalog path]
  (str/join " " (cons (:name catalog) path)))

(defn- value-schema [{:keys [coerce schema]}]
  (or schema
      (let [base {:string :string :boolean :boolean :int :int :long :int
                  :double :double :keyword :keyword :symbol :symbol}]
        (if (vector? coerce)
          [:vector (base (first coerce) :any)]
          (base coerce :any)))))

(defn- entry-props [{:keys [require default desc]} optional?]
  (cond-> {}
    (or optional? (not require)) (assoc :optional true)
    (some? default)              (assoc :default default)
    desc                         (assoc :description desc)))

(defn input-schema
  "Returns the Malli schema of the input map that the command at `path` passes to
  its handler. Groups take no input."
  [catalog path]
  (let [cmd (command catalog path)]
    (into [:map]
          (when-not (group? catalog path)
            (concat
             (for [k (:options cmd)
                   :let [o (option-def catalog k)]]
               [k (entry-props o false) (value-schema o)])
             (for [a (:args cmd)]
               [(:key a) (entry-props (assoc a :require true) (:optional a)) (value-schema (merge {:coerce :string} a))])
             (when-let [c (:child-args cmd)]
               [[(:key c :argv) {:description (:desc c "Arguments for the child program")}
                 (:schema c [:vector :string])]]))))))

(defn- flag [k] (str "--" (name k)))

(defn- cli-mapping [catalog path]
  (let [cmd (command catalog path)]
    (cond-> {:options (vec (for [k (option-keys cmd)
                                 :let [{:keys [alias ref require default coerce desc]} (option-def catalog k)]]
                             (cond-> {:key k :flag (flag k) :description desc}
                               alias              (assoc :alias (str "-" (name alias)))
                               ref                (assoc :value ref)
                               require            (assoc :required true)
                               (some? default)    (assoc :default default)
                               (vector? coerce)   (assoc :repeated true)
                               (get global-options k) (assoc :global true))))}
      (seq (:args cmd))
      (assoc :arguments (vec (map-indexed (fn [i a]
                                            {:key (:key a) :position i :value (:ref a)
                                             :required (not (:optional a)) :description (:desc a)})
                                          (:args cmd))))
      (:child-args cmd)
      (assoc :child-args {:key         (get-in cmd [:child-args :key] :argv)
                          :after       "--"
                          :value       (get-in cmd [:child-args :ref])
                          :description "Every argument after -- is passed unchanged, including empty strings and flag-like strings"}))))

(defn- usage-error [path code message]
  (ex-info message {::error {:code code :message message} ::path path}))

(defn- parse-error [path {:keys [cause msg flag]}]
  (throw (case cause
           :restrict (usage-error path :cli/unknown-option (str "unknown option " flag))
           :coerce   (usage-error path :cli/invalid-input msg)
           (usage-error path :cli/usage (or msg "invalid arguments")))))

(defn error-schema
  "Returns the Malli schema of structured errors for the command at `path`."
  [catalog path]
  (let [codes (concat (keys usage-errors) (keys (:errors (command catalog path))))]
    [:map
     [:error [:map
              [:code (into [:enum] codes)]
              [:message :string]
              [:details {:optional true} [:map-of :keyword :any]]]]]))

(defn- parse-spec [catalog keys]
  ;; Defaults and requirements apply after all chunks are parsed. Passing them here
  ;; would let a later chunk's default overwrite an earlier explicit value.
  (into {} (for [k keys] [k (select-keys (option-def catalog k) [:coerce :alias])])))

(defn- option-token? [t]
  (and (str/starts-with? t "-") (not= "-" t) (not (re-matches #"-\d.*" t))))

(defn- takes-value? [spec t]
  (let [k (if (str/starts-with? t "--")
            (keyword (subs t 2))
            (some (fn [[k o]] (when (= (subs t 1) (some-> (:alias o) name)) k)) spec))]
    (and (not (str/includes? t "=")) (contains? spec k) (not= :boolean (get-in spec [k :coerce])))))

(defn- next-positional
  "Splits `tokens` into `[option-tokens positional remaining]` at the first token
  that is neither an option nor an option's value. babashka.cli keeps parsing
  options after a positional, so each command level parses only its own prefix."
  [spec tokens]
  (loop [i 0]
    (if-let [t (get tokens i)]
      (cond
        (not (option-token? t)) [(subvec tokens 0 i) t (subvec tokens (inc i))]
        (takes-value? spec t)   (recur (+ i 2))
        :else                   (recur (inc i)))
      [tokens nil []])))

(defn- parse-chunk [path spec tokens]
  (:opts (cli/parse-args tokens {:spec spec :restrict true :repeated-opts true
                                 :error-fn #(parse-error path %)})))

(defn- merge-opts [a b]
  (merge-with (fn [x y] (if (vector? x) (into x y) y)) a b))

(defn- defaults [catalog cmd]
  (into {} (for [k (:options cmd)
                 :let [d (:default (option-def catalog k))]
                 :when (some? d)]
             [k d])))

(defn- humanize-input [_catalog cmd explanation]
  (let [label (fn [k]
                (if-let [a (some #(when (= k (:key %)) %) (:args cmd))]
                  (or (:ref a) (name k))
                  (cond (some #{k} (:options cmd))                  (flag k)
                        (= k (get-in cmd [:child-args :key] :argv)) (str "-- " (get-in cmd [:child-args :ref]))
                        :else                                       (name k))))]
    (str/join "; "
              (for [[k msgs] (me/humanize explanation)]
                (str (label k) ": " (str/join ", " (if (sequential? msgs) (flatten msgs) [msgs])))))))

(defn- split-child-args [args]
  (let [[before after] (split-with #(not= "--" %) args)]
    [(vec before) (when (seq after) (vec (rest after)))]))

(defn resolve-invocation
  "Resolves `args` against `catalog` without calling handlers.

  Returns `{:action :help :path p}`, `{:action :schema :path p :kind k}` or
  `{:action :run :path p :input m}`. Throws an `ex-info` whose data has
  `:harness-plugins-sync.cli/error` for usage errors. Only arguments before the
  first `--` are interpreted; later arguments become the child-args input."
  [catalog args]
  (let [[before child] (split-child-args args)]
    (loop [path [] help-path nil opts {} pos [] tokens (vec before)]
      (let [spec           (parse-spec catalog (if help-path global-keys (option-keys (command catalog path))))
            [chunk a more] (next-positional spec tokens)
            opts           (merge-opts opts (parse-chunk path spec chunk))]
        (if a
          (cond
            help-path
            (recur path (conj help-path a) opts pos more)

            (and (= [] path) (= "help" a))
            (recur path [] opts pos more)

            (and (empty? pos) (command catalog (conj path a)))
            (recur (conj path a) nil opts pos more)

            (group? catalog path)
            (throw (usage-error path :cli/unknown-command
                                (str "unknown command '" a "' for '" (command-name catalog path) "'")))

            :else
            (recur path nil opts (conj pos a) more))
          (let [cmd            (command catalog path)
                {:keys [schema]} opts]
            (when (and (:json opts) (:edn opts))
              (throw (usage-error path :cli/usage "choose either --json or --edn, not both")))
            (when (and schema (not (#{"input" "output" "error" "all"} schema)))
              (throw (usage-error path :cli/usage (str "unknown schema kind '" schema "'; use input, output, error or all"))))
            (cond
              help-path
              (if (command catalog help-path)
                {:action :help :path help-path}
                (throw (usage-error path :cli/unknown-command
                                    (str "unknown help path '" (str/join " " help-path) "'"))))

              (:help opts)   {:action :help :path path}
              schema         {:action :schema :path path :kind (keyword schema)}
              (group? catalog path)
              (if child
                (throw (usage-error path :cli/unexpected-argument
                                    (str "'" (command-name catalog path) "' does not accept arguments after --")))
                {:action :help :path path})

              :else
              (let [arg-defs (:args cmd)]
                (when (> (count pos) (count arg-defs))
                  (throw (usage-error path :cli/unexpected-argument
                                      (str "unexpected argument '" (nth pos (count arg-defs)) "' for '"
                                           (command-name catalog path) "'"))))
                (when (and child (not (:child-args cmd)))
                  (throw (usage-error path :cli/unexpected-argument
                                      (str "'" (command-name catalog path) "' does not accept arguments after --"))))
                (let [input (cond-> (merge (defaults catalog cmd)
                                           (apply dissoc opts global-keys)
                                           (zipmap (map :key arg-defs) pos))
                              (:child-args cmd) (assoc (get-in cmd [:child-args :key] :argv) (or child [])))]
                  (when-let [explanation (m/explain (input-schema catalog path) input)]
                    (throw (usage-error path :cli/invalid-input (humanize-input catalog cmd explanation))))
                  {:action :run :path path :input input})))))))))

(defn- table [rows]
  (let [width (apply max (map (comp count first) rows))]
    (str/join "\n" (for [[a b] rows]
                     (str "  " a (apply str (repeat (- (+ 2 width) (count a)) " ")) b)))))

(defn- option-row [k {:keys [alias ref desc require default default-desc]}]
  ;; Rendered here rather than with cli/format-opts, whose layout differs between
  ;; babashka.cli versions.
  [(str (if alias (str "-" (name alias) ", ") "    ") (flag k) (when ref (str " " ref)))
   (str desc
        (when require " (required)")
        (when-let [d (or default-desc default)] (str " (default: " d ")")))])

(defn- usage-line [catalog path]
  (let [cmd (command catalog path)]
    (or (:usage cmd)
        (str/join " "
                  (concat [(command-name catalog path)]
                          (when (group? catalog path) ["<command>"])
                          (for [a (:args cmd)] (if (:optional a) (str "[" (:ref a) "]") (:ref a)))
                          ["[OPTIONS]"]
                          (when-let [c (:child-args cmd)] ["--" (:ref c "<command> [args...]")]))))))

(defn help-text
  "Returns the help text for the command at `path`."
  [catalog path]
  (let [cmd      (command catalog path)
        group?   (group? catalog path)
        kids     (cond-> (mapv (fn [c] [(peek (:path c)) (:summary c)]) (children catalog path))
                   (= [] path) (conj ["help" "Show help for a command or subcommand"]))
        options  (for [k (option-keys cmd)] (option-row k (option-def catalog k)))
        sections (cond-> [(str "Usage: " (usage-line catalog path))
                          (or (:description cmd) (:summary cmd))]
                   group?             (conj (str "Commands:\n" (table kids)))
                   true               (conj (str "Options:\n" (table options)))
                   (seq (:examples cmd)) (conj (str "Examples:\n" (str/join "\n" (map #(str "  " %) (:examples cmd)))))
                   group?             (conj (format "Run '%s help %s<command>' for more information on a command."
                                                    (:name catalog)
                                                    (str/join (map #(str % " ") path)))))]
    (str (str/join "\n\n" sections) "\n")))

(defn- export-form
  "Returns readable EDN for schema `s`: regexes become strings (valid for `:re`)
  and functions become the symbol `untranslatable-fn`."
  [s]
  (walk/postwalk #(cond (instance? java.util.regex.Pattern %) (str %)
                        (fn? %) 'untranslatable-fn
                        :else %)
                 (m/form (m/schema s))))

(defn- limitations [s]
  (vec (for [{:keys [path schema]} (mu/subschemas s)
             :let  [t (m/type schema)]
             :when (or (= :fn t) (and (not= :any t) (= {} (js/transform schema))))]
         {:path path :type t
          :reason "Predicate or custom type with no data or JSON Schema translation; validated at runtime only"})))

(defn- contract [s fmt]
  (cond-> {:schema (if (= :json fmt) (js/transform s) (export-form s))}
    (seq (limitations s)) (assoc :limitations (limitations s))))

(def json-encoding
  "Rules used to write EDN data as JSON results and errors."
  "Keywords and symbols are strings without a leading colon, keeping any namespace (:pack/not-found is \"pack/not-found\"). Sets are arrays, sorted when their elements are comparable. Map keys are strings.")

(defn schema-export
  "Returns the discovery data for `kind` (:input, :output, :error or :all) of the
  command at `path`. `fmt` :json selects JSON Schema; anything else selects Malli."
  [catalog path kind fmt]
  (let [cmd    (command catalog path)
        input  (merge (contract (input-schema catalog path) fmt)
                      {:cli (cli-mapping catalog path)})
        output (cond
                 (group? catalog path) {:raw "Help text for this command group"}
                 (:raw-output cmd)     {:raw (:raw-output cmd)}
                 :else                 (contract (:output cmd :any) fmt))
        error  (merge (contract (error-schema catalog path) fmt)
                      {:codes (merge usage-errors (:errors cmd))
                       :exit  {:usage 2 :application 1}})
        parts  {:input input :output output :error error}]
    (cond-> {:command          (command-name catalog path)
             :contract-version (:version catalog "1")
             :schema-language  (if (= :json fmt) :json-schema :malli)}
      (= :json fmt)         (assoc :json-encoding json-encoding)
      (= :all kind)         (merge parts)
      (not= :all kind)      (assoc :kind kind kind (kind parts))
      (group? catalog path) (assoc :subcommands (vec (for [c (children catalog path)]
                                                       {:name (peek (:path c)) :summary (:summary c)}))))))

(defn ->json-data
  "Converts EDN data to the JSON wire form described by [[json-encoding]]."
  [x]
  (walk/postwalk #(cond (keyword? %) (subs (str %) 1)
                        (symbol? %) (str %)
                        (instance? java.util.regex.Pattern %) (str %)
                        (set? %) (try (vec (sort %)) (catch Exception _ (vec %)))
                        :else %)
                 x))

(defn- print-data [fmt x]
  (if (= :json fmt)
    (println (json/write-str (->json-data x) :indent true :escape-slash false :escape-unicode false))
    (binding [*print-namespace-maps* false]
      (pprint/pprint x))))

(defn- render-human
  "Default human renderer: maps as `key: value` lines, sequences as `- item`."
  ([x] (render-human x ""))
  ([x pad]
   (cond
     (map? x)
     (doseq [[k v] x]
       (let [label (str pad (term/style :heading (name k)) ":")]
         (if (and (coll? v) (seq v))
           (do (println label) (render-human v (str pad "  ")))
           (println label (cond (nil? v) "none" (coll? v) "none" (keyword? v) (name v) :else v)))))

     (sequential? x)
     (if (seq x)
       (doseq [v x]
         (if (coll? v)
           (do (println (str pad "-")) (render-human v (str pad "  ")))
           (println (str pad "- " (if (keyword? v) (name v) v)))))
       (println (str pad "none")))

     (some? x) (println (str pad x)))))

(defn- report-error [catalog path fmt {:keys [code] :as err}]
  (if fmt
    (print-data fmt {:error err})
    (binding [*out* *err*]
      (term/error (:message err))
      (when (contains? usage-errors code)
        (println (format "Run '%s help%s' for usage." (:name catalog)
                         (str/join (map #(str " " %) path)))))))
  (if (contains? usage-errors code) 2 1))

(defn- output-format [args]
  (let [before (first (split-child-args args))
        json?  (some #{"--json"} before)
        edn?   (some #{"--edn"} before)]
    (cond (and json? edn?) nil json? :json edn? :edn)))

(defn- execute [catalog impl {:keys [path input]} fmt]
  (let [cmd                   (command catalog path)
        entry                 (get impl (:handler cmd))
        _                     (when-not entry
                                (throw (ex-info (str "No implementation for handler " (:handler cmd)) {:path path})))
        {f :run human :human} (if (map? entry) entry {:run entry})]
    (try
      (let [result (f input)]
        (cond
          (:raw-output cmd) (if (int? result) result 0)
          fmt               (do (print-data fmt result) 0)
          :else             (do ((or human render-human) result) 0)))
      (catch clojure.lang.ExceptionInfo e
        (let [{:keys [code details]} (ex-data e)]
          (if (contains? (:errors cmd) code)
            (report-error catalog path fmt (cond-> {:code code :message (ex-message e)}
                                             details (assoc :details details)))
            (throw e)))))))

(defn run
  "Runs `args` against `catalog` and returns an exit status.

  `impl` maps each handler key to a function of the input map, or to
  `{:run f :human g}` where `g` prints the result for humans. A handler signals an
  expected failure by throwing `ex-info` with a `:code` declared in the command's
  `:errors`, and optional `:details`. Other exceptions propagate.

  `opts` may contain `:terminal`, a map that replaces
  [[harness-plugins-sync.terminal/detect]] (for tests)."
  ([catalog impl args] (run catalog impl args {}))
  ([catalog impl args {:keys [terminal]}]
   (let [fmt (output-format args)]
     (binding [term/*terminal* (or terminal (term/detect (some? fmt)))]
       (let [inv (try (resolve-invocation catalog args)
                      (catch clojure.lang.ExceptionInfo e
                        (if (::error (ex-data e)) (ex-data e) (throw e))))]
         (if-let [err (::error inv)]
           (report-error catalog (::path inv) fmt err)
           (case (:action inv)
             :help   (do (print (help-text catalog (:path inv))) (flush) 0)
             :schema (do (print-data fmt (schema-export catalog (:path inv) (:kind inv) fmt)) 0)
             :run    (execute catalog impl inv fmt))))))))
