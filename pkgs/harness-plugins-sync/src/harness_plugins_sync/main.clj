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
                     [:status [:enum :fetched :unchanged :dry-run]]]
   ::pack-promote   [:map
                     [:pack :string]
                     [:source [:enum :git :local]]
                     [:path :string]
                     [:active :string]
                     [:rev [:maybe :string]]
                     [:digest :string]
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
                     [:status [:enum :updated :unchanged :would-update]]
                     [:plugins [:vector :string]]
                     [:changes [:ref ::changes]]
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
                     [:warnings [:vector :string]]]
   ::peer-result    [:map
                     [:host :string]
                     [:status [:enum :ok :unreachable :error]]
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
    "                   codex        codex plugin marketplace/add per CODEX_HOME (cached per rev)"
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
    "    4. Verify: 'harness-plugins-sync status --json'. Running sessions do not reload on their own"
    "       (start a new session, or run /reload-plugins in Claude / /reload in Pi CLI)."]))

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
                   [:dry-run :boolean]
                   [:pulled [:vector [:ref ::pack-pull]]]
                   [:packs [:vector [:ref ::pack-promote]]]
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
                   [:dry-run :boolean]
                   [:packs [:vector [:ref ::pack-promote]]]
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
                   [:config-path :string]
                   [:config-exists? :boolean]
                   [:state-dir :string]
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
          (let [res (remote/run-remote (:ssh-bin conf) h subcmd input)]
            (when (= :unreachable (:status res))
              (term/warn (str "peer " h " unreachable: " (:message res))))
            (when (= :error (:status res))
              (term/warn (str "peer " h " failed: " (:message res))))
            res))
        remotes))

(defn- pull-local [conf input]
  (let [selected (filter-packs (:packs conf) (:pack input))
        locals   (parse-locals (:local input) {})]
    (vec (for [[pack-name spec] (sort-by key selected)
               :when (not (contains? locals pack-name))]
           (packs/pull-pack! (:state-dir conf) pack-name spec
                             {:retain-revs (:retain-revs conf)
                              :dry-run     (boolean (:dry-run input))})))))

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
        promoted       (mapv (fn [pack-name]
                               (packs/promote-pack! (:state-dir conf) pack-name
                                                    {:local-path (get locals pack-name)
                                                     :dry-run    (boolean (:dry-run input))}))
                             (sort selected-set))
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
        all-profiles   (cfg/resolve-profiles conf)
        profiles       (cond->> (filter-profiles all-profiles (:profile input))
                         (seq (:pack input))
                         (filterv #(some (comp selected-set :pack) (:plugins %))))
        bound          (mapv (fn [prof]
                               (bind/bind-profile! (:state-dir conf) prof active-map
                                                   {:codex-bin (:codex-bin conf)
                                                    :force     (boolean (:force input))
                                                    :dry-run   (boolean (:dry-run input))}))
                             profiles)]
    {:packs promoted :profiles bound}))

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
        local (if (:local? plan)
                (term/with-progress "Binding profiles" (apply-local conf input))
                {:packs [] :profiles []})
        peers (run-peers conf "apply" input (:remotes plan))]
    {:host     host
     :dry-run  (boolean (:dry-run input))
     :packs    (:packs local)
     :profiles (:profiles local)
     :peers    peers}))

(defn handle-sync
  "Handler for `harness-plugins-sync sync`."
  [input]
  (let [conf   (cfg/load-config (:config input))
        host   (remote/local-hostname)
        plan   (remote/target-plan (:peers conf) (:all input) (:host input) host)
        pulled (if (:local? plan)
                 (term/with-progress "Pulling packs" (pull-local conf input))
                 [])
        local  (if (:local? plan)
                 (term/with-progress "Binding profiles" (apply-local conf input))
                 {:packs [] :profiles []})
        peers  (run-peers conf "sync" input (:remotes plan))]
    {:host     host
     :dry-run  (boolean (:dry-run input))
     :pulled   pulled
     :packs    (:packs local)
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
        peers         (run-peers conf "status" input (:remotes plan))]
    {:host           host
     :config-path    (:config-path conf)
     :config-exists? (:config-exists? conf)
     :state-dir      (:state-dir conf)
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
  (if (and rev (> (count rev) 10)) (subs rev 0 10) (or rev "-")))

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
        (let [role (case status :ok :ok :unreachable :warn :error :error)]
          (println (str "  " (pad hw host)
                        (term/style role (name status))
                        (when message (str "  " (term/style :dim message))))))))))

(defn- render-packs-promote [packs]
  (when (seq packs)
    (let [pw (col-width (map :pack packs))
          sw (col-width (map (comp name :status) packs))]
      (println (term/style :heading "Packs"))
      (doseq [{:keys [pack source rev path status]} packs]
        (let [ref (if (= :local source) (str "local:" path) (short-rev rev))]
          (println (str "  " (pad pw pack)
                        (pad sw (name status))
                        (term/style :dim ref))))))))

(defn- render-profiles-bind [profiles]
  (when (seq profiles)
    (let [pw (col-width (map :profile profiles))
          hw (col-width (map (comp name :harness) profiles))
          sw (col-width (map (comp name :status) profiles))]
      (println)
      (println (term/style :heading "Profiles"))
      (doseq [{:keys [profile harness status plugins warnings]} profiles]
        (let [role (case status :updated :ok :would-update :info :unchanged :dim)]
          (println (str "  " (pad pw profile)
                        (pad hw (name harness))
                        (term/style role (pad sw (name status)))
                        (term/style :dim (str (count plugins) " plugins"))))
          (doseq [w warnings]
            (println (str "    " (term/style :warn "warning:") " " w))))))))

(defn- render-pull [{:keys [pulled peers]}]
  (println (term/style :heading "Pulled"))
  (if (seq pulled)
    (let [pw (col-width (map :pack pulled))
          sw (col-width (map (comp name :status) pulled))]
      (doseq [{:keys [pack rev status]} pulled]
        (println (str "  " (pad pw pack)
                      (pad sw (name status))
                      (term/style :dim (short-rev rev))))))
    (println "  none"))
  (render-peers peers))

(defn- render-apply [{:keys [packs profiles peers]}]
  (render-packs-promote packs)
  (render-profiles-bind profiles)
  (render-peers peers))

(defn- render-sync [{:keys [packs profiles peers]}]
  (render-packs-promote packs)
  (render-profiles-bind profiles)
  (render-peers peers))

(defn- render-status [{:keys [config-path config-exists? state-dir packs profiles peers]}]
  (println (str (term/style :heading "Config: ") config-path
                (when-not config-exists? (term/style :dim " (defaults)"))))
  (println (str (term/style :heading "State:  ") state-dir))
  (println)
  (println (term/style :heading "Packs"))
  (let [pw (col-width (map :pack packs))
        sw (col-width (map (comp name :source) packs))]
    (doseq [{:keys [pack source active-rev staged-rev resolved-path pending-promote]} packs]
      (let [ref (case source
                  :missing "not fetched"
                  :local   (str "local:" resolved-path)
                  :git     (short-rev active-rev))]
        (println (str "  " (pad pw pack)
                      (pad sw (name source))
                      (term/style :dim ref)
                      (when pending-promote
                        (str "  " (term/style :warn (str "staged:" (short-rev staged-rev))))))))))
  (println)
  (println (term/style :heading "Profiles"))
  (let [pw (col-width (map :profile profiles))
        hw (col-width (map (comp name :harness) profiles))
        sw (col-width ["in-sync" "drift"])]
    (doseq [{:keys [profile harness in-sync? drift plugins warnings]} profiles]
      (println (str "  " (pad pw profile)
                    (pad hw (name harness))
                    (term/style (if in-sync? :ok :warn)
                                (pad sw (if in-sync? "in-sync" "drift")))
                    (term/style :dim (str/join ", " plugins))))
      (doseq [d drift]
        (println (str "    " (term/style :warn "drift:") " " d)))
      (doseq [w warnings]
        (println (str "    " (term/style :warn "warning:") " " w)))))
  (render-peers peers))

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
  {:cmd/sync   {:run handle-sync   :human render-sync}
   :cmd/pull   {:run handle-pull   :human render-pull}
   :cmd/apply  {:run handle-apply  :human render-apply}
   :cmd/status {:run handle-status :human render-status}
   :cmd/config {:run handle-config :human render-config}
   :cmd/doctor {:run handle-doctor :human render-doctor}})

(defn -main
  "CLI entrypoint."
  [& args]
  (System/exit (cli/run catalog impl (vec args))))
