(ns harness-plugins-sync.config
  "Loads `ol-plugins-sync` configuration and discovers harness profiles.

  Configuration is read as EDN data with `clojure.edn/read-string` and never
  evaluated. Profiles come from declared `:profiles` merged with T3 Code's
  `providerInstances` in `settings.json`."
  (:require
   [babashka.fs :as fs]
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [malli.core :as m]
   [malli.error :as me])
  (:import
   java.security.MessageDigest))

(def app-dir
  "XDG subdirectory name shared by config and state paths."
  "ol-plugins-sync")

(def default-pack-url
  "Default Git repository URL for the `harness-plugins` pack."
  "https://git.outskirtslabs.com/casey/skills.git")

(def default-plugins
  "Default plugin selectors bound to profiles that do not override `:plugins`."
  ["my@harness-plugins"
   "pstack@harness-plugins"
   "tstack@harness-plugins"])

(defn home-dir
  "Returns the effective user home directory from `env` or the process."
  ([] (home-dir {}))
  ([env]
   (or (get env "HOME")
       (System/getenv "HOME")
       (System/getProperty "user.home"))))

(defn xdg-config-home
  "Returns the XDG config root from `env` or the process."
  ([] (xdg-config-home {}))
  ([env]
   (let [v (or (get env "XDG_CONFIG_HOME") (System/getenv "XDG_CONFIG_HOME"))]
     (if (str/blank? v)
       (str (fs/path (home-dir env) ".config"))
       v))))

(defn xdg-data-home
  "Returns the XDG data root from `env` or the process."
  ([] (xdg-data-home {}))
  ([env]
   (let [v (or (get env "XDG_DATA_HOME") (System/getenv "XDG_DATA_HOME"))]
     (if (str/blank? v)
       (str (fs/path (home-dir env) ".local" "share"))
       v))))

(defn default-config-path
  "Returns the default `config.edn` path under `ol-plugins-sync`."
  ([] (default-config-path {}))
  ([env]
   (str (fs/path (xdg-config-home env) app-dir "config.edn"))))

(defn default-state-dir
  "Returns the default state directory under `ol-plugins-sync`."
  ([] (default-state-dir {}))
  ([env]
   (str (fs/path (xdg-data-home env) app-dir))))

(defn default-t3-settings-path
  "Returns the default T3 Code `settings.json` path."
  ([] (default-t3-settings-path {}))
  ([env]
   (let [t3-home (or (get env "T3CODE_HOME")
                     (System/getenv "T3CODE_HOME")
                     (str (fs/path (xdg-config-home env) "t3")))]
     (str (fs/path t3-home "userdata" "settings.json")))))

(defn expand-path
  "Expands leading `~`, `$HOME`, `$XDG_CONFIG_HOME`, and `$XDG_DATA_HOME` in `s`."
  ([s] (expand-path s {}))
  ([s env]
   (when s
     (let [home     (home-dir env)
           cfg-home (xdg-config-home env)
           dat-home (xdg-data-home env)
           expanded (-> (str s)
                        (str/replace #"^~(?=/|$)" (java.util.regex.Matcher/quoteReplacement home))
                        (str/replace "$HOME" home)
                        (str/replace "$XDG_CONFIG_HOME" cfg-home)
                        (str/replace "$XDG_DATA_HOME" dat-home))]
       (str (fs/normalize expanded))))))

(defn sha256-hex
  "Returns the lowercase hex SHA-256 digest of UTF-8 string `s`."
  [s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes  (.digest digest (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

(defn strip-json5
  "Removes JSON5 comments and trailing commas from `s` while keeping strings intact."
  [s]
  (let [n  (count s)
        sb (StringBuilder. n)]
    (loop [i 0 in-str? false esc? false]
      (if (>= i n)
        (let [no-comments (str sb)]
          (str/replace no-comments #",(?=\s*[\}\]])" ""))
        (let [c  (.charAt ^String s i)
              c2 (when (< (inc i) n) (.charAt ^String s (inc i)))]
          (cond
            in-str?
            (do (.append sb c)
                (recur (inc i) (if (and (not esc?) (= c \")) false true)
                       (and (not esc?) (= c \\))))

            (= c \")
            (do (.append sb c)
                (recur (inc i) true false))

            (and (= c \/) (= c2 \/))
            (let [nl (.indexOf ^String s "\n" (+ i 2))]
              (if (neg? nl)
                (recur n false false)
                (recur nl false false)))

            (and (= c \/) (= c2 \*))
            (let [end (.indexOf ^String s "*/" (+ i 2))]
              (if (neg? end)
                (recur n false false)
                (recur (+ end 2) false false)))

            :else
            (do (.append sb c)
                (recur (inc i) false false))))))))

(defn parse-json5
  "Parses JSON or JSON5 text `s` with keyword keys."
  [s]
  (json/read-str (strip-json5 s) :key-fn keyword))

(def harness-enum
  "Supported agent harness identifiers."
  [:enum :claude :codex :pi :antigravity])

(def pack-schema
  "Malli schema for one pack entry in `config.edn`."
  [:map
   [:url :string]
   [:branch {:optional true} :string]])

(def profile-config-schema
  "Malli schema for one declared profile entry in `config.edn`."
  [:map
   [:harness {:optional true} harness-enum]
   [:dir {:optional true} :string]
   [:link-dir {:optional true} :string]
   [:enabled {:optional true} :boolean]
   [:plugins {:optional true} [:vector :string]]
   [:extra-plugins {:optional true} [:vector :string]]
   [:exclude-plugins {:optional true} [:vector :string]]])

(def config-schema
  "Malli schema for `ol-plugins-sync` configuration EDN."
  [:map
   [:state-dir {:optional true} :string]
   [:t3-settings {:optional true} [:maybe :string]]
   [:discover-t3 {:optional true} :boolean]
   [:include-antigravity-global {:optional true} :boolean]
   [:codex-bin {:optional true} :string]
   [:ssh-bin {:optional true} :string]
   [:retain-revs {:optional true} [:int {:min 0}]]
   [:peers {:optional true} [:vector :string]]
   [:packs {:optional true} [:map-of [:or :string :keyword] pack-schema]]
   [:defaults {:optional true} [:map [:plugins {:optional true} [:vector :string]]]]
   [:profiles {:optional true} [:map-of [:or :string :keyword] profile-config-schema]]])

(defn parse-selector
  "Parses `plugin@pack` selector `s` into `{:plugin :pack :selector}`.
  Throws `:config/invalid` on malformed selectors."
  [s]
  (let [[plugin pack extra] (str/split (str s) #"@" 3)]
    (when (or extra (str/blank? plugin) (str/blank? pack))
      (throw (ex-info (str "invalid plugin selector '" s "'; expected <plugin>@<pack>")
                      {:code :config/invalid :details {:selector s}})))
    {:plugin plugin :pack pack :selector (str plugin "@" pack)}))

(defn parse-local-override
  "Parses a `--local <pack>=<path>` flag into `[pack abs-path]`."
  ([spec] (parse-local-override spec {}))
  ([spec env]
   (let [idx (str/index-of (str spec) "=")]
     (when (or (nil? idx) (zero? idx) (= idx (dec (count spec))))
       (throw (ex-info (str "invalid --local value '" spec "'; expected <pack>=<path>")
                       {:code :config/invalid :details {:local spec}})))
     (let [pack (subs spec 0 idx)
           path (expand-path (subs spec (inc idx)) env)]
       [pack (str (fs/absolutize path))]))))

(defn- normalize-key-map [m]
  (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) v]) m)))

(defn default-config
  "Returns built-in defaults used when `config.edn` is absent."
  ([] (default-config {}))
  ([env]
   {:state-dir                  (default-state-dir env)
    :t3-settings                (default-t3-settings-path env)
    :discover-t3                true
    :include-antigravity-global true
    :codex-bin                  "codex"
    :ssh-bin                    "ssh"
    :retain-revs                2
    :peers                      []
    :packs                      {"harness-plugins" {:url default-pack-url :branch "main"}}
    :defaults                   {:plugins default-plugins}
    :profiles                   {}}))

(defn load-config
  "Loads and validates configuration from `config-path` (or the default path).

  When `config-path` is nil and the default file does not exist, returns built-in
  defaults with `:config-exists? false`. When `config-path` is explicit and
  missing, throws `:config/not-found`."
  ([config-path] (load-config config-path {}))
  ([config-path env]
   (let [explicit? (some? config-path)
         path      (expand-path (or config-path (default-config-path env)) env)
         exists?   (fs/exists? path)]
     (when (and explicit? (not exists?))
       (throw (ex-info (str "config file not found: " path)
                       {:code :config/not-found :details {:path path}})))
     (let [raw (if exists?
                 (try
                   (edn/read-string (slurp path))
                   (catch Exception e
                     (throw (ex-info (str "cannot read EDN config " path ": " (ex-message e))
                                     {:code :config/invalid :details {:path path}}))))
                 {})]
       (when-not (map? raw)
         (throw (ex-info (str "config file " path " must contain an EDN map")
                         {:code :config/invalid :details {:path path}})))
       (when-let [expl (m/explain config-schema raw)]
         (throw (ex-info (str "invalid config in " path ": " (pr-str (me/humanize expl)))
                         {:code :config/invalid :details {:path path :errors (me/humanize expl)}})))
       (let [base     (default-config env)
             merged   (merge base raw)
             packs    (into {}
                            (for [[k v] (normalize-key-map (or (:packs raw) (:packs base)))]
                              [k (merge {:branch "main"} v)]))
             profiles (normalize-key-map (:profiles merged))]
         (assoc merged
                :config-path path
                :config-exists? exists?
                :state-dir (expand-path (:state-dir merged) env)
                :t3-settings (when-let [p (:t3-settings merged)] (expand-path p env))
                :packs packs
                :defaults {:plugins (vec (or (get-in raw [:defaults :plugins])
                                             (get-in base [:defaults :plugins])))}
                :profiles profiles))))))

(defn- env-entry-val [inst var-name]
  (some #(when (= var-name (:name %)) (:value %)) (:environment inst)))

(defn- driver->harness [driver]
  (case driver
    "claudeAgent" :claude
    "codex"       :codex
    "pi"          :pi
    "antigravity" :antigravity
    nil))

(defn- resolve-t3-profile-dir [harness instance-id inst t3-userdata-dir env]
  (let [cfg-home (xdg-config-home env)]
    (case harness
      :claude      (expand-path (or (get-in inst [:config :homePath])
                                    (env-entry-val inst "CLAUDE_CONFIG_DIR")
                                    (str (fs/path cfg-home "claude")))
                                env)
      :codex       (expand-path (or (get-in inst [:config :homePath])
                                    (env-entry-val inst "CODEX_HOME")
                                    (str (fs/path cfg-home "codex")))
                                env)
      :pi          (expand-path (or (env-entry-val inst "PI_CODING_AGENT_DIR")
                                    (get-in inst [:config :agentDir])
                                    (str (fs/path cfg-home "pi" "agent")))
                                env)
      :antigravity (str (fs/path t3-userdata-dir "providers" "antigravity" (sha256-hex instance-id))))))

(defn discover-t3-profiles
  "Reads `providerInstances` from T3 Code's `settings.json` and returns a map of
  profile id to profile descriptor. Returns `{}` when the file is absent."
  ([t3-settings-path] (discover-t3-profiles t3-settings-path {}))
  ([t3-settings-path env]
   (if (or (nil? t3-settings-path) (not (fs/exists? t3-settings-path)))
     {}
     (let [data            (try
                             (parse-json5 (slurp t3-settings-path))
                             (catch Exception e
                               (throw (ex-info (str "cannot parse T3 settings " t3-settings-path ": " (ex-message e))
                                               {:code :config/invalid :details {:path t3-settings-path}}))))
           t3-userdata-dir (str (fs/parent t3-settings-path))
           instances       (:providerInstances data)]
       (into {}
             (for [[id-k inst] instances
                   :let [id      (name id-k)
                         harness (driver->harness (:driver inst))]
                   :when (and harness (not= false (:enabled inst)))]
               [id {:id      id
                    :harness harness
                    :dir     (resolve-t3-profile-dir harness id inst t3-userdata-dir env)
                    :source  :t3}]))))))

(defn- apply-plugin-modifiers [base-plugins {:keys [plugins extra-plugins exclude-plugins]}]
  (let [excl  (set exclude-plugins)
        start (if (some? plugins) (vec plugins) (vec base-plugins))
        added (into start (remove (set start)) (or extra-plugins []))]
    (vec (remove excl added))))

(defn- ensure-profile-complete [id p]
  (when-not (:harness p)
    (throw (ex-info (str "profile '" id "' has no :harness and did not match a T3 provider instance")
                    {:code :config/invalid :details {:profile id}})))
  (when-not (:dir p)
    (throw (ex-info (str "profile '" id "' has no :dir and did not match a T3 provider instance")
                    {:code :config/invalid :details {:profile id}})))
  p)

(defn resolve-profiles
  "Resolves all enabled profiles from `cfg`, combining T3 discovery, optional
  standalone Antigravity global config, and declared `:profiles`.

  Deduplicates entries that resolve to the same `[:harness dir]`, preferring
  explicit `:profiles` declarations."
  ([cfg] (resolve-profiles cfg {}))
  ([cfg env]
   (let [t3-profs   (if (:discover-t3 cfg)
                      (discover-t3-profiles (:t3-settings cfg) env)
                      {})
         agy-global (when (:include-antigravity-global cfg)
                      {"antigravity-global"
                       {:id      "antigravity-global"
                        :harness :antigravity
                        :dir     (str (fs/path (home-dir env) ".gemini"))
                        :source  :default}})
         base-map   (merge agy-global t3-profs)
         declared   (:profiles cfg)
         all-ids    (distinct (concat (keys base-map) (keys declared)))
         defaults   (get-in cfg [:defaults :plugins] [])
         resolved   (for [id all-ids
                          :let [base (get base-map id)
                                decl (get declared id)
                                merged (merge base decl {:id id})
                                enabled? (if (contains? merged :enabled) (:enabled merged) true)]
                          :when enabled?
                          :let [dir     (some-> (:dir merged) (expand-path env))
                                link-dir (some-> (:link-dir merged) (expand-path env))
                                plugins (apply-plugin-modifiers defaults merged)
                                prof    (cond-> (assoc merged
                                                       :dir dir
                                                       :plugins (mapv parse-selector plugins)
                                                       :source (cond
                                                                 (and base decl) :merged
                                                                 decl            :config
                                                                 :else           (:source base)))
                                          link-dir (assoc :link-dir link-dir))]]
                      (ensure-profile-complete id prof))]
     (->> resolved
          (reduce (fn [acc p]
                    (let [k [(:harness p) (:dir p)]]
                      (if (contains? acc k)
                        (if (#{:config :merged} (:source p))
                          (assoc acc k p)
                          acc)
                        (assoc acc k p))))
                  {})
          vals
          (sort-by :id)
          vec))))
