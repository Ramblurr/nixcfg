(ns harness-plugins-sync.main
  "Entrypoint and command catalog for `harness-plugins-sync`."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [harness-plugins-sync.bind :as bind]
   [harness-plugins-sync.cli :as cli]
   [harness-plugins-sync.config :as cfg]
   [harness-plugins-sync.packs :as packs]
   [harness-plugins-sync.remote :as remote]
   [harness-plugins-sync.terminal :as term]))

(def ^:private shared-registry
  {::pack-pull      [:map
                     [:pack :string]
                     [:url :string]
                     [:branch :string]
                     [:rev {:optional true} [:maybe :string]]
                     [:previous-rev {:optional true} [:maybe :string]]
                     [:changed? {:optional true} :boolean]
                     [:status [:enum :fetched :unchanged :dry-run :failed]]
                     [:error {:optional true} :string]]
   ::pack-error     [:map
                     [:pack :string]
                     [:error :string]]
   ::pack-promote   [:map
                     [:pack :string]
                     [:source [:enum :git :local]]
                     [:path :string]
                     [:active :string]
                     [:rev [:maybe :string]]
                     [:digest :string]
                     [:previous-rev [:maybe :string]]
                     [:changed? :boolean]
                     [:status [:enum :promoted :unchanged :dry-run]]]
   ::pack-status    [:map
                     [:pack :string]
                     [:url :string]
                     [:branch :string]
                     [:source [:enum :git :local :missing]]
                     [:active-path :string]
                     [:resolved-path [:maybe :string]]
                     [:active-rev [:maybe :string]]
                     [:staged-rev [:maybe :string]]
                     [:pending-promote :boolean]]
   ::loads          [:map-of :string :string]
   ::targets        [:map-of :string :string]
   ::changes        [:map
                     [:added [:vector :string]]
                     [:updated [:vector :string]]
                     [:removed [:vector :string]]
                     [:unchanged [:vector :string]]]
   ::profile-bind   [:map
                     [:profile :string]
                     [:harness [:enum :claude :codex :pi :antigravity]]
                     [:dir :string]
                     [:link-dir {:optional true} :string]
                     [:status [:enum :updated :unchanged :would-update :failed]]
                     [:error {:optional true} :string]
                     [:plugins [:vector :string]]
                     [:changes [:ref ::changes]]
                     [:loads [:ref ::loads]]
                     [:in-sync? :boolean]
                     [:drift [:vector :string]]
                     [:warnings [:vector :string]]]
   ::profile-status [:map
                     [:profile :string]
                     [:harness [:enum :claude :codex :pi :antigravity]]
                     [:dir :string]
                     [:link-dir {:optional true} :string]
                     [:source [:enum :t3 :config :merged :default]]
                     [:plugins [:vector :string]]
                     [:in-sync? :boolean]
                     [:drift [:vector :string]]
                     [:loads [:ref ::loads]]
                     [:warnings [:vector :string]]]
   ::peer-result    [:map
                     [:host :string]
                     [:status [:enum :ok :unreachable :auth-required :error]]
                     [:auth-url {:optional true} :string]
                     [:message {:optional true} :string]
                     [:result {:optional true} :any]]
   ::doctor-check   [:map
                     [:name :string]
                     [:ok? :boolean]
                     [:detail :string]
                     [:fix {:optional true} :string]]
   ::pack-spec      [:map
                     [:url :string]
                     [:branch :string]]
   ::resolved-prof  [:map
                     [:id :string]
                     [:harness [:enum :claude :codex :pi :antigravity]]
                     [:dir :string]
                     [:link-dir {:optional true} :string]
                     [:source [:enum :t3 :config :merged :default]]
                     [:plugins [:vector :string]]]})

(def ^:private root-footer
  (str/join
   "\n"
   ["Architecture & workflow:"
    "  Solves plugin sync across 3 axes: harnesses, provider accounts/profiles, and machines."
    ""
    "  Where pieces live:"
    "    Pack repos   Git repos (default: harness-plugins -> git.outskirtslabs.com/casey/skills);"
    "                 additional host- or env-specific packs can be declared in config.edn."
    "    Config       $XDG_CONFIG_HOME/ol-plugins-sync/config.edn (EDN; usually Nix-managed,"
    "                 defaults apply if absent). Declares packs, peers, defaults, and overrides."
    "    State        $XDG_DATA_HOME/ol-plugins-sync/ — per-pack bare repo, revs/<sha>, staged,"
    "                 and atomic active symlink; per-profile state and Pi package.json under profiles/."
    "    Bindings     Discovered from T3 settings.json (providerInstances) + declared :profiles:"
    "                   claude       <claudeHome>/skills/<plugin> symlink (live, namespaced)"
    "                   codex        codex plugin marketplace/add per CODEX_HOME, one generated"
    "                                marketplace per pack, named after its config key"
    "                   pi           <state>/profiles/<id>/package.json referenced by settings.json"
    "                   antigravity  <profileDir>/config/plugins/<plugin> (per T3 sha256 dir + ~/.gemini)"
    "    Machines     Each host pulls from Git independently (timer or manual). --all / --host"
    "                 fans out over SSH to configured peers; offline hosts warn and catch up later."
    ""
    "  Workflow:"
    "    1. Standard update: commit and push changes to the pack repo's branch (default: main),"
    "       then run 'harness-plugins-sync sync --all'. No Nix rebuild is needed for plugin edits."
    "    2. Local WIP test: 'harness-plugins-sync apply --local <pack>=<dir>' (next 'sync' resets)."
    "    3. New T3 account/profile: run 'harness-plugins-sync apply' to discover and bind it."
    "    4. Verify: 'harness-plugins-sync status --all'. Running sessions do not reload on their own"
    "       (run /clear in T3, or start a new thread)."]))

(def ^:private common-errors
  {:config/not-found       "The specified configuration file does not exist"
   :config/invalid         "Configuration, selector, or local override is invalid"
   :pack/not-found         "A referenced plugin pack or local directory does not exist"
   :pack/not-fetched       "A plugin pack has no fetched snapshot yet"
   :pack/plugin-not-found  "A requested plugin directory does not exist inside its pack"
   :pack/git-failed        "A Git or archive subprocess failed while fetching a pack"
   :bind/plugin-collision  "Two packs provide the same plugin name for one profile"
   :bind/path-conflict     "A target binding path already exists as a non-symlink"
   :bind/codex-missing     "The codex executable was not found on PATH"
   :bind/codex-failed      "The codex CLI exited with an error while binding plugins"})

(def catalog
  "Declarative CLI catalog for `harness-plugins-sync`."
  {:name        "harness-plugins-sync"
   :description "Sync agent plugin packs and bind them natively across harnesses and profiles."
   :version     "1"
   :footer      root-footer
   :examples    ["harness-plugins-sync sync"
                 "harness-plugins-sync sync --all"
                 "harness-plugins-sync apply --local harness-plugins=~/src/git.outskirtslabs.com/casey/skills"
                 "harness-plugins-sync status --json"
                 "harness-plugins-sync config"]
   :options     {:config  {:alias :c :coerce :string :ref "<path>"
                           :desc "Config EDN path (default: $XDG_CONFIG_HOME/ol-plugins-sync/config.edn)"}
                 :all     {:alias :a :coerce :boolean
                           :desc "Run locally and fan out over SSH to configured peers"}
                 :host    {:alias :H :coerce [:string] :ref "<host>"
                           :desc "Target remote host over SSH (repeatable)"}
                 :pack    {:alias :p :coerce [:string] :ref "<name>"
                           :desc "Limit fetch/promote to one pack without unbinding others (repeatable)"}
                 :profile {:alias :P :coerce [:string] :ref "<id>"
                           :desc "Limit binding to one profile (repeatable)"}
                 :local   {:alias :L :coerce [:string] :ref "<pack=path>"
                           :desc "Bind pack to a local directory instead of Git snapshot (repeatable)"}
                 :force   {:alias :f :coerce :boolean
                           :desc "Re-bind profiles even when revisions and state are unchanged"}
                 :dry-run {:alias :n :coerce :boolean
                           :desc "Show planned changes without modifying files"}}
   :commands
   [{:path        ["sync"]
     :summary     "Fetch Git packs, promote active snapshots, and bind all profiles"
     :description (str "Fetch configured Git packs into staged snapshots, promote them to active, and\n"
                       "bind every discovered harness profile. Resets any previous --local override back\n"
                       "to Git unless --local is passed. With --all, unreachable SSH peers warn and are\n"
                       "skipped.")
     :options     [:config :all :host :pack :profile :local :force :dry-run]
     :examples    ["harness-plugins-sync sync"
                   "harness-plugins-sync sync --all"
                   "harness-plugins-sync sync --dry-run"]
     :output      [:map {:registry shared-registry}
                   [:host :string]
                   [:user :string]
                   [:local? :boolean]
                   [:dry-run :boolean]
                   [:targets [:ref ::targets]]
                   [:pulled [:vector [:ref ::pack-pull]]]
                   [:packs [:vector [:ref ::pack-promote]]]
                   [:pack-errors [:vector [:ref ::pack-error]]]
                   [:profiles [:vector [:ref ::profile-bind]]]
                   [:peers [:vector [:ref ::peer-result]]]]
     :errors      common-errors
     :handler     :cmd/sync}
    {:path        ["pull"]
     :summary     "Fetch Git packs into staged snapshots without affecting active sessions"
     :description "Fetch configured Git packs and unpack new revisions into staged snapshots. Does not change active links or harness bindings."
     :options     [:config :all :host :pack :dry-run]
     :examples    ["harness-plugins-sync pull"
                   "harness-plugins-sync pull --pack harness-plugins"]
     :output      [:map {:registry shared-registry}
                   [:host :string]
                   [:dry-run :boolean]
                   [:pulled [:vector [:ref ::pack-pull]]]
                   [:peers [:vector [:ref ::peer-result]]]]
     :errors      (select-keys common-errors [:config/not-found :config/invalid :pack/not-found :pack/git-failed])
     :handler     :cmd/pull}
    {:path        ["apply"]
     :summary     "Promote staged snapshots (or --local) and bind profiles without fetching"
     :description "Promote staged pack snapshots (or --local overrides) to active and bind every discovered harness profile without running git fetch."
     :options     [:config :all :host :pack :profile :local :force :dry-run]
     :examples    ["harness-plugins-sync apply"
                   "harness-plugins-sync apply --local harness-plugins=~/src/git.outskirtslabs.com/casey/skills"
                   "harness-plugins-sync apply --profile pi-t3 --force"]
     :output      [:map {:registry shared-registry}
                   [:host :string]
                   [:user :string]
                   [:local? :boolean]
                   [:dry-run :boolean]
                   [:targets [:ref ::targets]]
                   [:packs [:vector [:ref ::pack-promote]]]
                   [:pack-errors [:vector [:ref ::pack-error]]]
                   [:profiles [:vector [:ref ::profile-bind]]]
                   [:peers [:vector [:ref ::peer-result]]]]
     :errors      common-errors
     :handler     :cmd/apply}
    {:path        ["status"]
     :summary     "Show config/state paths, active revisions, profiles, and binding drift"
     :description "Inspect resolved config and state paths, active and staged pack revisions, discovered profiles, and binding drift without making changes."
     :options     [:config :all :host :pack :profile]
     :examples    ["harness-plugins-sync status"
                   "harness-plugins-sync status --json"]
     :output      [:map {:registry shared-registry}
                   [:host :string]
                   [:user :string]
                   [:local? :boolean]
                   [:config-path :string]
                   [:config-exists? :boolean]
                   [:state-dir :string]
                   [:targets [:ref ::targets]]
                   [:packs [:vector [:ref ::pack-status]]]
                   [:profiles [:vector [:ref ::profile-status]]]
                   [:peers [:vector [:ref ::peer-result]]]]
     :errors      (select-keys common-errors [:config/not-found :config/invalid :pack/not-found])
     :handler     :cmd/status}
    {:path        ["config"]
     :summary     "Show resolved configuration, config file path, and T3 discovery settings"
     :description (str "Print the resolved configuration, config file path (and whether it exists),\n"
                       "state directory, and discovered profile sources without making changes. Use\n"
                       "--schema output to inspect the configuration contract.")
     :options     [:config]
     :examples    ["harness-plugins-sync config"
                   "harness-plugins-sync config --edn"
                   "harness-plugins-sync config --schema output --edn"]
     :output      [:map {:registry shared-registry}
                   [:host :string]
                   [:config-path :string]
                   [:config-exists? :boolean]
                   [:state-dir :string]
                   [:t3-settings [:maybe :string]]
                   [:discover-t3 :boolean]
                   [:include-antigravity-global :boolean]
                   [:codex-bin :string]
                   [:ssh-bin :string]
                   [:retain-revs :int]
                   [:peers [:vector :string]]
                   [:packs [:map-of :string [:ref ::pack-spec]]]
                   [:defaults [:map [:plugins [:vector :string]]]]
                   [:resolved-profiles [:vector [:ref ::resolved-prof]]]]
     :errors      (select-keys common-errors [:config/not-found :config/invalid])
     :handler     :cmd/config}
    {:path        ["doctor"]
     :summary     "Check prerequisites, file locations, and configuration health"
     :description "Run read-only checks on required tools, configuration, T3 settings, and pack manifests. Never modifies state or starts services."
     :options     [:config]
     :examples    ["harness-plugins-sync doctor"
                   "harness-plugins-sync doctor --json"]
     :output      [:map {:registry shared-registry}
                   [:host :string]
                   [:ok? :boolean]
                   [:config-path :string]
                   [:state-dir :string]
                   [:checks [:vector [:ref ::doctor-check]]]]
     :errors      (select-keys common-errors [:config/not-found :config/invalid])
     :handler     :cmd/doctor}]})

(defn- filter-packs [all-packs pack-filter]
  (if (seq pack-filter)
    (do
      (doseq [p pack-filter
              :when (not (contains? all-packs p))]
        (throw (ex-info (str "unknown pack '" p "'; configured packs: "
                             (str/join ", " (sort (keys all-packs))))
                        {:code :pack/not-found :details {:pack p}})))
      (select-keys all-packs pack-filter))
    all-packs))

(defn- filter-profiles [profiles profile-filter]
  (if (seq profile-filter)
    (let [known (set (map :id profiles))]
      (doseq [id profile-filter
              :when (not (contains? known id))]
        (throw (ex-info (str "unknown profile '" id "'; discovered profiles: "
                             (str/join ", " (sort known)))
                        {:code :config/invalid :details {:profile id}})))
      (vec (filter #(contains? (set profile-filter) (:id %)) profiles)))
    profiles))

(defn- parse-locals [local-args env]
  (into {} (map #(cfg/parse-local-override % env) local-args)))

(defn- run-peers [conf subcmd input remotes]
  (mapv (fn [h]
          (term/step (str "Waiting for " h))
          (remote/run-remote (:ssh-bin conf) h subcmd input))
        remotes))

(defn- progress-label [verb plan]
  (let [n (+ (if (:local? plan) 1 0) (count (:remotes plan)))]
    (str verb " " n (if (= 1 n) " host" " hosts"))))

(defn- pack-target [{:keys [source rev]}]
  (if (= :local source) "local" rev))

(defn- bind-or-fail
  "Binds `prof` and returns its result with the disk check of [[bind/profile-status]]:
  the revisions a new session loads and any drift. Reports a binding error as
  `:status :failed` so other profiles still bind."
  [conf prof active-map status-map input]
  (let [result (try
                 (bind/bind-profile! (:state-dir conf) prof active-map
                                     {:codex-bin (:codex-bin conf)
                                      :force     (boolean (:force input))
                                      :dry-run   (boolean (:dry-run input))})
                 (catch Exception e
                   {:profile  (:id prof)
                    :harness  (:harness prof)
                    :dir      (str (:dir prof))
                    :status   :failed
                    :error    (ex-message e)
                    :plugins  (mapv :selector (:plugins prof))
                    :changes  {:added [] :updated [] :removed [] :unchanged []}
                    :warnings []}))]
    (merge result (select-keys (bind/profile-status (:state-dir conf) prof status-map)
                               [:loads :in-sync? :drift]))))

(defn- pull-local
  "Fetches each selected pack. Reports a fetch error as `:status :failed` so
  other packs still update and bind; the failed pack keeps its last snapshot."
  [conf input]
  (let [selected (filter-packs (:packs conf) (:pack input))
        locals   (parse-locals (:local input) {})]
    (vec (for [[pack-name spec] (sort-by key selected)
               :when (not (contains? locals pack-name))]
           (try
             (packs/pull-pack! (:state-dir conf) pack-name spec
                               {:retain-revs (:retain-revs conf)
                                :dry-run     (boolean (:dry-run input))})
             (catch clojure.lang.ExceptionInfo e
               {:pack   pack-name
                :url    (:url spec)
                :branch (:branch spec "main")
                :status :failed
                :error  (ex-message e)}))))))

(defn- apply-local [conf input]
  (let [locals         (parse-locals (:local input) {})
        all-pack-names (vec (distinct (concat (keys (:packs conf)) (keys locals))))
        selected-set   (if (seq (:pack input))
                         (do (doseq [p (:pack input)
                                     :when (not (contains? (set all-pack-names) p))]
                               (throw (ex-info (str "unknown pack '" p "'")
                                               {:code :pack/not-found :details {:pack p}})))
                             (set (:pack input)))
                         (set all-pack-names))
        all-profiles   (cfg/resolve-profiles conf)
        needed         (update-vals (group-by :pack (mapcat :plugins all-profiles))
                                    #(vec (distinct (map :plugin %))))
        ;; A candidate snapshot that fails its check never becomes active: its pack
        ;; keeps the last good snapshot, or has none and is left out of binding.
        attempts       (for [pack-name (sort selected-set)]
                         (try
                           {:pack     pack-name
                            :promoted (packs/promote-pack!
                                       (:state-dir conf) pack-name
                                       {:local-path (get locals pack-name)
                                        :dry-run    (boolean (:dry-run input))
                                        :check      #(bind/check-pack! pack-name % (get needed pack-name))})}
                           (catch clojure.lang.ExceptionInfo e
                             (case (:code (ex-data e))
                               :pack/not-fetched {:pack pack-name}
                               :pack/rejected    {:pack pack-name :error (ex-message e)}
                               (throw e)))))
        promoted       (mapv #(dissoc % :error) (keep :promoted attempts))
        pack-errors    (vec (for [{:keys [pack promoted error]} attempts
                                  :let [error (or error (:error promoted))]
                                  :when error]
                              {:pack pack :error error}))
        unchanged-pks  (for [pack-name (sort (remove selected-set all-pack-names))
                             :let [st (packs/pack-status (:state-dir conf) pack-name (get (:packs conf) pack-name {}))]
                             :when (not= :missing (:source st))]
                         {:pack     pack-name
                          :source   (:source st)
                          :path     (:resolved-path st)
                          :active   (:active-path st)
                          :rev      (:active-rev st)
                          :digest   (if (= :local (:source st))
                                      (packs/content-digest (:resolved-path st))
                                      (:active-rev st))
                          :changed? false
                          :status   :unchanged})
        active-map     (into {} (map (juxt :pack identity) (concat promoted unchanged-pks)))
        profiles       (cond->> (filter-profiles all-profiles (:profile input))
                         (seq (:pack input))
                         (filterv #(some (comp selected-set :pack) (:plugins %))))
        status-map     (into {} (for [pack-name all-pack-names]
                                  [pack-name (packs/pack-status (:state-dir conf) pack-name
                                                                (get (:packs conf) pack-name {}))]))
        bound          (mapv #(bind-or-fail conf % active-map status-map input) profiles)]
    {:packs       promoted
     :pack-errors pack-errors
     :targets     (update-vals active-map pack-target)
     :profiles    bound}))

(defn handle-pull
  "Handler for `harness-plugins-sync pull`."
  [input]
  (let [conf  (cfg/load-config (:config input))
        host  (remote/local-hostname)
        plan  (remote/target-plan (:peers conf) (:all input) (:host input) host)
        pulled (if (:local? plan)
                 (term/with-progress "Pulling packs" (pull-local conf input))
                 [])
        peers  (run-peers conf "pull" input (:remotes plan))]
    {:host    host
     :dry-run (boolean (:dry-run input))
     :pulled  pulled
     :peers   peers}))

(defn handle-apply
  "Handler for `harness-plugins-sync apply`."
  [input]
  (let [conf  (cfg/load-config (:config input))
        host  (remote/local-hostname)
        plan  (remote/target-plan (:peers conf) (:all input) (:host input) host)
        [local peers]
        (term/with-progress (progress-label "Applying" plan)
          [(if (:local? plan)
             (apply-local conf input)
             {:packs [] :pack-errors [] :targets {} :profiles []})
           (run-peers conf "apply" input (:remotes plan))])]
    {:host     host
     :user     (remote/local-username)
     :local?   (:local? plan)
     :dry-run  (boolean (:dry-run input))
     :targets  (:targets local)
     :packs    (:packs local)
     :pack-errors (:pack-errors local)
     :profiles (:profiles local)
     :peers    peers}))

(defn handle-sync
  "Handler for `harness-plugins-sync sync`."
  [input]
  (let [conf   (cfg/load-config (:config input))
        host   (remote/local-hostname)
        plan   (remote/target-plan (:peers conf) (:all input) (:host input) host)
        [pulled local peers]
        (term/with-progress (progress-label "Syncing" plan)
          (if (:local? plan)
            [(do (term/step "Pulling packs") (pull-local conf input))
             (do (term/step "Binding profiles") (apply-local conf input))
             (run-peers conf "sync" input (:remotes plan))]
            [[] {:packs [] :pack-errors [] :targets {} :profiles []}
             (run-peers conf "sync" input (:remotes plan))]))]
    {:host     host
     :user     (remote/local-username)
     :local?   (:local? plan)
     :dry-run  (boolean (:dry-run input))
     :targets  (:targets local)
     :pulled   pulled
     :packs    (:packs local)
     :pack-errors (:pack-errors local)
     :profiles (:profiles local)
     :peers    peers}))

(defn handle-status
  "Handler for `harness-plugins-sync status`."
  [input]
  (let [conf          (cfg/load-config (:config input))
        host          (remote/local-hostname)
        plan          (remote/target-plan (:peers conf) (:all input) (:host input) host)
        selected-pks  (filter-packs (:packs conf) (:pack input))
        pack-statuses (if (:local? plan)
                        (mapv (fn [[k spec]] (packs/pack-status (:state-dir conf) k spec))
                              (sort-by key selected-pks))
                        [])
        status-map    (into {} (map (juxt :pack identity) pack-statuses))
        profiles      (if (:local? plan)
                        (filter-profiles (cfg/resolve-profiles conf) (:profile input))
                        [])
        prof-statuses (mapv #(bind/profile-status (:state-dir conf) % status-map) profiles)
        peers         (term/with-progress (progress-label "Checking" plan)
                        (run-peers conf "status" input (:remotes plan)))]
    {:host           host
     :user           (remote/local-username)
     :local?         (:local? plan)
     :config-path    (:config-path conf)
     :config-exists? (:config-exists? conf)
     :state-dir      (:state-dir conf)
     :targets        (into {}
                           (for [{:keys [pack source active-rev]} pack-statuses
                                 :when (not= :missing source)]
                             [pack (pack-target {:source source :rev active-rev})]))
     :packs          pack-statuses
     :profiles       prof-statuses
     :peers          peers}))

(defn handle-config
  "Handler for `harness-plugins-sync config`."
  [input]
  (let [conf     (cfg/load-config (:config input))
        profiles (mapv (fn [p]
                         (cond-> {:id      (:id p)
                                  :harness (:harness p)
                                  :dir     (:dir p)
                                  :source  (:source p)
                                  :plugins (mapv :selector (:plugins p))}
                           (:link-dir p) (assoc :link-dir (:link-dir p))))
                       (cfg/resolve-profiles conf))]
    {:host                       (remote/local-hostname)
     :config-path                (:config-path conf)
     :config-exists?             (:config-exists? conf)
     :state-dir                  (:state-dir conf)
     :t3-settings                (:t3-settings conf)
     :discover-t3                (boolean (:discover-t3 conf))
     :include-antigravity-global (boolean (:include-antigravity-global conf))
     :codex-bin                  (:codex-bin conf)
     :ssh-bin                    (:ssh-bin conf)
     :retain-revs                (:retain-revs conf)
     :peers                      (vec (:peers conf))
     :packs                      (:packs conf)
     :defaults                   (:defaults conf)
     :resolved-profiles          profiles}))

(defn handle-doctor
  "Handler for `harness-plugins-sync doctor`."
  [input]
  (let [conf     (cfg/load-config (:config input))
        host     (remote/local-hostname)
        bin-chk  (fn [label bin fix]
                   (if-let [found (fs/which bin)]
                     {:name label :ok? true :detail (str found)}
                     {:name label :ok? false :detail (str bin " not found on PATH") :fix fix}))
        t3-path  (:t3-settings conf)
        t3-chk   (cond
                   (not (:discover-t3 conf))
                   {:name "t3-settings" :ok? true :detail "T3 discovery disabled"}

                   (and t3-path (fs/exists? t3-path))
                   {:name "t3-settings" :ok? true :detail t3-path}

                   :else
                   {:name   "t3-settings"
                    :ok?    true
                    :detail (str "not present at " t3-path " (using declared/default profiles only)")})
        cfg-chk  (if (:config-exists? conf)
                   {:name "config" :ok? true :detail (:config-path conf)}
                   {:name   "config"
                    :ok?    true
                    :detail (str "using built-in defaults (" (:config-path conf) " not found)")})
        checks   [cfg-chk
                  {:name   "state-dir"
                   :ok?    true
                   :detail (str (:state-dir conf) (if (fs/exists? (:state-dir conf)) " (exists)" " (will be created)"))}
                  t3-chk
                  (bin-chk "git" "git" "Install git and ensure it is on PATH.")
                  (bin-chk "tar" "tar" "Install GNU tar and ensure it is on PATH.")
                  (bin-chk "codex" (:codex-bin conf) "Install codex or set :codex-bin in config.edn.")
                  (bin-chk "ssh" (:ssh-bin conf) "Install openssh if using --all or --host.")]]
    {:host        host
     :ok?         (every? :ok? checks)
     :config-path (:config-path conf)
     :state-dir   (:state-dir conf)
     :checks      checks}))

(defn- short-rev [rev]
  (if (and rev (> (count rev) 7)) (subs rev 0 7) (or rev "-")))

(defn- col-width [xs]
  (if (seq xs) (+ 2 (apply max (map #(count (str %)) xs))) 2))

(defn- pad [w s]
  (let [s (str s)]
    (str s (apply str (repeat (max 1 (- w (count s))) " ")))))

(defn- render-peers [peers]
  (when (seq peers)
    (let [hw (col-width (map :host peers))]
      (println)
      (println (term/style :heading "Peers"))
      (doseq [{:keys [host status message]} peers]
        (let [role (case status :ok :ok :unreachable :warn :auth-required :warn :error :error)]
          (println (str "  " (pad hw host)
                        (term/style role (name status))
                        (when message (str "  " (term/style :dim message))))))))))

(defn- render-pull [{:keys [pulled peers]}]
  (println (term/style :heading "Pulled"))
  (if (seq pulled)
    (let [pw (col-width (map :pack pulled))
          sw (col-width (map (comp name :status) pulled))]
      (doseq [{:keys [pack rev status error]} pulled]
        (println (str "  " (pad pw pack)
                      (pad sw (name status))
                      (if error
                        (term/style :error error)
                        (term/style :dim (short-rev rev)))))))
    (println "  none"))
  (render-peers peers)
  (doseq [{:keys [host result]} peers
          {:keys [pack status error]} (:pulled result)
          :when (= "failed" (name status))]
    (println (str "  " (term/style :error (str host ": fetch " pack " failed: " error))))))

(defn- kw [x]
  (when x (keyword (name x))))

(defn- string-keys [m]
  (update-keys (or m {}) name))

(defn- selector-pack [selector]
  (second (str/split selector #"@" 2)))

(defn- host-entries
  "Returns one entry per host from a local result and its peer results. Peer
  results arrive as JSON, so this normalizes their keys and enum values."
  [result]
  (concat
   (when (:local? result)
     [{:name (:host result) :user (:user result) :local? true :status :ok :result result}])
   (for [{:keys [host status message auth-url result]} (:peers result)
         :let [[user host-name] (if (str/includes? host "@") (str/split host #"@" 2) [nil host])]]
     {:name     (or (:host result) host-name)
      :user     (or (:user result) user)
      :local?   false
      :status   (kw status)
      :message  message
      :auth-url auth-url
      :result   result})))

(defn- profile-row [targets dry-run? {:keys [profile harness status error loads plugins in-sync? drift warnings]}]
  (let [loads      (string-keys loads)
        packs-used (vec (distinct (map selector-pack plugins)))
        status     (kw status)
        current?   (and (not= :failed status)
                        (not (false? in-sync?))
                        (every? #(and (get loads %) (= (get loads %) (get targets %))) packs-used))]
    {:profile  profile
     :harness  (name harness)
     :revs     (mapv #(get loads % "none") packs-used)
     :current? current?
     :answer   (cond current? "yes" (and dry-run? (not= :failed status)) "pending" :else "NO")
     :note     (cond error                                          error
                     (and (= :updated status) (= :codex (kw harness))) "(plugin cache refreshed)"
                     (= :updated status)                            "(bindings updated)"
                     (= :would-update status)                       "(would update)")
     :details  (concat (map #(str "drift: " %) drift)
                       (map #(str "warning: " %) warnings))}))

(defn host-report
  "Returns `{:targets :previous :dry-run? :hosts}` for a sync, apply, or status
  result. Each host holds rows that tell whether a new session of each profile
  loads the target revision of its packs. The target is the local host's active
  revision, or the first reached peer's when the local host did not run."
  [result]
  (let [entries  (host-entries result)
        dry-run? (boolean (:dry-run result))
        source   (some #(when (= :ok (:status %)) (:result %)) entries)
        targets  (string-keys (:targets source))
        previous (into {} (for [{:keys [pack previous-rev]} (:packs source)
                                :when (contains? targets (name pack))]
                            [(name pack) previous-rev]))]
    {:targets  targets
     :previous previous
     :dry-run? dry-run?
     :hosts    (vec (for [{:keys [status result] :as entry} entries
                          :let [host-targets (merge (string-keys (:targets result)) targets)
                                rows         (when (= :ok status)
                                               (mapv #(profile-row host-targets dry-run? %) (:profiles result)))
                                pack-errors  (vec (concat
                                                   (for [{:keys [pack error] :as pulled} (:pulled result)
                                                         :when (= :failed (kw (:status pulled)))]
                                                     (str "fetch " pack " failed: " error))
                                                   (for [{:keys [pack error]} (:pack-errors result)]
                                                     (str "pack " pack " rejected its new snapshot: " error))))]]
                      (assoc entry
                             :rows rows
                             :pack-errors pack-errors
                             :current? (and (= :ok status)
                                            (empty? pack-errors)
                                            (every? #(or (:current? %) (= "pending" (:answer %))) rows)))))}))

(defn- pull-exit-status
  "Returns 1 when any pack failed to fetch, on this host or a reached peer."
  [{:keys [pulled peers]}]
  (if (some #(= :failed (kw (:status %)))
            (concat pulled (mapcat (comp :pulled :result) peers)))
    1
    0))

(defn- host-exit-status [result]
  (if (every? :current? (:hosts (host-report result))) 0 1))

(defn- render-pack-lines [{:keys [targets previous]} transition?]
  (let [pw (col-width (keys targets))]
    (doseq [[pack target] (sort-by key targets)
            :let [prev (get previous pack)]]
      (println (str (term/style :bold (pad pw pack))
                    (cond
                      (not transition?)            (short-rev target)
                      (and prev (not= prev target)) (str (short-rev prev) " -> " (short-rev target))
                      :else                         (str (short-rev target) " " (term/style :dim "(no change)"))))))))

(defn- render-host [{host-name :name :keys [user local? status message auth-url rows current? pack-errors]} widths]
  (let [[pw hw rw aw] widths]
    (println)
    (println (str (term/style (if current? :ok :error) (if current? "✓" "✗")) " "
                  (term/style :bold host-name)
                  (term/style :dim (str " (" (str/join ", " (remove nil? [user (when local? "local")])) ")"))))
    (doseq [e pack-errors]
      (println (str "    " (term/style :error e))))
    (case status
      :ok
      (if (empty? rows)
        (println (str "    " (term/style :dim "no profiles")))
        (doseq [{:keys [profile harness revs current? answer note details]} rows]
          (println (str "    " (pad pw profile) (pad hw harness)
                        (pad rw (str/join "," (map short-rev revs)))
                        (term/style (cond current? :ok (= "pending" answer) :info :else :error)
                                    (if note (pad aw answer) answer))
                        (when note (term/style (if current? :dim :error) note))))
          (doseq [d details]
            (println (str "      " (term/style :warn d))))))

      :auth-required
      (do (println (str "    " (term/style :warn "not reached: Tailscale SSH auth required")))
          (println (str "    " auth-url)))

      :error
      (println (str "    " (term/style :error (str "failed: " message))))

      (println (str "    " (term/style :warn (str "not reached: " message)))))))

(defn- render-summary [{:keys [hosts]}]
  (let [reached  (filter #(= :ok (:status %)) hosts)
        rows     (mapcat :rows reached)
        behind   (count (remove #(or (:current? %) (= "pending" (:answer %))) rows))
        pending  (count (filter #(= "pending" (:answer %)) rows))
        missed   (- (count hosts) (count reached))
        current  (count (filter :current? hosts))
        plural   (fn [n word] (str n " " word (when (not= 1 n) "s")))
        parts    (cond-> [(if (= current (count hosts))
                            (str "all " (plural (count hosts) "host") " current")
                            (str current " of " (plural (count hosts) "host") " current"))]
                   (pos? behind)  (conj (str (plural behind "profile") " behind"))
                   (pos? pending) (conj (str (plural pending "profile") " would update"))
                   (pos? missed)  (conj (str (plural missed "host") " not reached")))]
    (println)
    (println (str (term/style :heading "Result: ") (str/join ", " parts) "."))))

(defn- render-hosts [result transition?]
  (let [report (host-report result)
        rows   (mapcat :rows (:hosts report))
        widths [(col-width (map :profile rows))
                (col-width (map :harness rows))
                (col-width (map #(str/join "," (map short-rev (:revs %))) rows))
                (col-width (map :answer rows))]]
    (render-pack-lines report transition?)
    (doseq [h (:hosts report)]
      (render-host h widths))
    (render-summary report)
    (when (and transition? (not (:dry-run? report)))
      (println (term/style :dim "Sessions already running keep their loaded skills until /clear (T3) or a new thread.")))))

(defn- render-apply [result]
  (render-hosts result true))

(defn- render-sync [result]
  (render-hosts result true))

(defn- render-status [{:keys [config-path config-exists? state-dir] :as result}]
  (println (str (term/style :heading "Config: ") config-path
                (when-not config-exists? (term/style :dim " (defaults)"))))
  (println (str (term/style :heading "State:  ") state-dir))
  (println)
  (render-hosts result false))

(defn- render-config [{:keys [config-path config-exists? state-dir t3-settings discover-t3
                              peers packs defaults resolved-profiles]}]
  (println (str (term/style :heading "Config: ") config-path
                (if config-exists? "" (term/style :dim " (not found; using built-in defaults)"))))
  (println (str (term/style :heading "State:  ") state-dir))
  (println (str (term/style :heading "T3:     ") (or t3-settings "none")
                (when-not discover-t3 (term/style :dim " (discovery disabled)"))))
  (println (str (term/style :heading "Peers:  ") (if (seq peers) (str/join ", " peers) "none")))
  (println (str (term/style :heading "Defaults: ") (str/join ", " (:plugins defaults))))
  (println)
  (println (term/style :heading "Packs"))
  (let [pw (col-width (keys packs))]
    (doseq [[k {:keys [url branch]}] (sort-by key packs)]
      (println (str "  " (pad pw k) url " " (term/style :dim (str "(" branch ")"))))))
  (println)
  (println (term/style :heading "Resolved profiles"))
  (let [pw (col-width (map :id resolved-profiles))
        hw (col-width (map (comp name :harness) resolved-profiles))
        sw (col-width (map (comp name :source) resolved-profiles))]
    (doseq [{:keys [id harness source dir plugins]} resolved-profiles]
      (println (str "  " (pad pw id) (pad hw (name harness)) (pad sw (name source)) dir))
      (println (str "    " (term/style :dim (str/join ", " plugins)))))))

(defn- render-doctor [{:keys [checks]}]
  (let [nw (col-width (map :name checks))]
    (println (term/style :heading "Doctor"))
    (doseq [{:keys [name ok? detail fix]} checks]
      (println (str "  " (term/style (if ok? :ok :error) (if ok? "ok  " "fail"))
                    " " (pad nw name) detail))
      (when fix
        (println (str "       " (term/style :dim (str "fix: " fix))))))))

(def impl
  "Command handler and human renderer map for [[catalog]]."
  {:cmd/sync   {:run handle-sync   :human render-sync   :exit host-exit-status}
   :cmd/pull   {:run handle-pull   :human render-pull   :exit pull-exit-status}
   :cmd/apply  {:run handle-apply  :human render-apply  :exit host-exit-status}
   :cmd/status {:run handle-status :human render-status :exit host-exit-status}
   :cmd/config {:run handle-config :human render-config}
   :cmd/doctor {:run handle-doctor :human render-doctor}})

(defn -main
  "CLI entrypoint."
  [& args]
  (System/exit (cli/run catalog impl (vec args))))
