(ns harness-plugins-sync.bind
  "Binds active plugin packs into each harness profile and checks for drift.

  Supports `:claude`, `:codex`, `:pi`, and `:antigravity` while preserving
  native plugin layouts and namespacing. Tracks managed entries per profile in
  `<state-dir>/profiles/<id>/state.edn` so removed plugins are pruned safely
  without touching unmanaged files."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as proc]
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [harness-plugins-sync.packs :as packs])
  (:import
   java.nio.file.Files))

(defn profile-state-dir
  "Returns the managed state directory for `profile-id`."
  [state-dir profile-id]
  (str (fs/path state-dir "profiles" profile-id)))

(defn profile-state-file
  "Returns the `state.edn` path for `profile-id`."
  [state-dir profile-id]
  (str (fs/path (profile-state-dir state-dir profile-id) "state.edn")))

(defn read-profile-state
  "Reads the last recorded binding state for `profile-id`, or `{}`."
  [state-dir profile-id]
  (let [p (profile-state-file state-dir profile-id)]
    (if (fs/exists? p)
      (try
        (edn/read-string (slurp p))
        (catch Exception _ {}))
      {})))

(defn- write-profile-state! [state-dir profile-id state]
  (let [p (profile-state-file state-dir profile-id)]
    (fs/create-dirs (fs/parent p))
    (spit p (str (pr-str state) "\n"))))

(defn- resolve-plugin-dir [pack-info plugin-name]
  (let [active-root (:active pack-info)
        real-root   (:path pack-info)
        real-dir    (str (fs/path real-root "plugins" plugin-name))
        link-dir    (str (fs/path active-root "plugins" plugin-name))]
    (when-not (fs/directory? real-dir)
      (throw (ex-info (str "plugin '" plugin-name "' not found in pack '" (:pack pack-info)
                           "' at " real-dir)
                      {:code    :pack/plugin-not-found
                       :details {:pack (:pack pack-info) :plugin plugin-name :path real-dir}})))
    {:plugin    plugin-name
     :pack      (:pack pack-info)
     :selector  (str plugin-name "@" (:pack pack-info))
     :link-dir  link-dir
     :real-dir  real-dir
     :pack-info pack-info}))

(defn- resolve-profile-plugins [profile active-packs]
  (let [resolved (mapv (fn [{:keys [plugin pack selector]}]
                         (let [info (get active-packs pack)]
                           (when-not info
                             (throw (ex-info (str "profile '" (:id profile) "' references unknown pack '" pack "'")
                                             {:code    :pack/not-found
                                              :details {:profile (:id profile) :pack pack :selector selector}})))
                           (resolve-plugin-dir info plugin)))
                       (:plugins profile))]
    (when (#{:claude :pi :antigravity} (:harness profile))
      (let [dups (->> resolved (group-by :plugin) (filter #(> (count (val %)) 1)))]
        (when-let [[plugin-name entries] (first dups)]
          (throw (ex-info (str "profile '" (:id profile) "' has conflicting plugin name '" plugin-name
                               "' from packs: " (str/join ", " (map :pack entries)))
                          {:code    :bind/plugin-collision
                           :details {:profile (:id profile)
                                     :plugin  plugin-name
                                     :packs   (mapv :pack entries)}})))))
    resolved))

(defn- sync-symlink-dir!
  "Synchronizes managed plugin symlinks inside `target-dir` and returns
  `{:added :updated :removed :unchanged}`."
  [target-dir desired-map managed-prev dry-run]
  (when-not dry-run
    (fs/create-dirs target-dir))
  (let [added     (atom [])
        updated   (atom [])
        unchanged (atom [])
        removed   (atom [])]
    (doseq [[plugin-name target] (sort-by key desired-map)]
      (let [link-path (str (fs/path target-dir plugin-name))
            current   (packs/read-symlink-target link-path)]
        (cond
          (= current target)
          (swap! unchanged conj plugin-name)

          (and (nil? current) (fs/exists? link-path))
          (throw (ex-info (str "refusing to overwrite non-symlink path: " link-path)
                          {:code :bind/path-conflict :details {:path link-path}}))

          (nil? current)
          (do
            (when-not dry-run
              (packs/atomic-symlink! link-path target))
            (swap! added conj plugin-name))

          :else
          (do
            (when-not dry-run
              (packs/atomic-symlink! link-path target))
            (swap! updated conj plugin-name)))))
    (doseq [stale (sort (remove #(contains? desired-map %) managed-prev))]
      (let [link-path (str (fs/path target-dir stale))]
        (when (Files/isSymbolicLink (fs/path link-path))
          (when-not dry-run
            (fs/delete link-path))
          (swap! removed conj stale))))
    {:added     @added
     :updated   @updated
     :removed   @removed
     :unchanged @unchanged}))

(defn- bind-claude! [state-dir profile resolved {:keys [dry-run]}]
  (let [skills-dir  (str (fs/path (:dir profile) "skills"))
        prev-state  (read-profile-state state-dir (:id profile))
        desired     (into {} (map (fn [{:keys [plugin link-dir]}] [plugin link-dir]) resolved))
        changes     (sync-symlink-dir! skills-dir desired (:managed-plugins prev-state []) dry-run)
        changed?    (boolean (or (seq (:added changes)) (seq (:updated changes)) (seq (:removed changes))))]
    (when-not dry-run
      (write-profile-state! state-dir (:id profile)
                            {:harness         :claude
                             :dir             (:dir profile)
                             :managed-plugins (vec (sort (keys desired)))
                             :plugins         (mapv :selector resolved)}))
    {:profile  (:id profile)
     :harness  :claude
     :dir      (:dir profile)
     :status   (cond dry-run (if changed? :would-update :unchanged)
                     changed? :updated
                     :else :unchanged)
     :plugins  (mapv :selector resolved)
     :changes  changes
     :warnings []}))

(defn- has-antigravity-manifest? [real-dir]
  (boolean
   (some #(fs/exists? (fs/path real-dir %))
         [".antigravity-plugin/plugin.json"
          "plugin.json"
          ".gemini-plugin/plugin.json"])))

(defn- bind-antigravity! [state-dir profile resolved {:keys [dry-run]}]
  (let [plugins-dir (str (fs/path (:dir profile) "config" "plugins"))
        prev-state  (read-profile-state state-dir (:id profile))
        desired     (into {} (map (fn [{:keys [plugin link-dir]}] [plugin link-dir]) resolved))
        changes     (sync-symlink-dir! plugins-dir desired (:managed-plugins prev-state []) dry-run)
        changed?    (boolean (or (seq (:added changes)) (seq (:updated changes)) (seq (:removed changes))))
        warnings    (vec (for [{:keys [selector real-dir]} resolved
                               :when (not (has-antigravity-manifest? real-dir))]
                           (str selector " has no .antigravity-plugin/plugin.json or plugin.json")))]
    (when-not dry-run
      (write-profile-state! state-dir (:id profile)
                            {:harness         :antigravity
                             :dir             (:dir profile)
                             :managed-plugins (vec (sort (keys desired)))
                             :plugins         (mapv :selector resolved)}))
    {:profile  (:id profile)
     :harness  :antigravity
     :dir      (:dir profile)
     :status   (cond dry-run (if changed? :would-update :unchanged)
                     changed? :updated
                     :else :unchanged)
     :plugins  (mapv :selector resolved)
     :changes  changes
     :warnings warnings}))

(defn- normalize-rel-entry [plugin-name rel]
  (let [clean (-> (str rel)
                  (str/replace #"^\./" ""))]
    (str "./plugins/" plugin-name "/" clean)))

(defn- pi-plugin-contributions [{:keys [plugin real-dir]}]
  (let [pkg-path (fs/path real-dir "package.json")]
    (if (fs/exists? pkg-path)
      (let [pkg (json/read-str (slurp (str pkg-path)) :key-fn keyword)
            pi  (:pi pkg)]
        {:skills     (mapv #(normalize-rel-entry plugin %) (:skills pi))
         :extensions (mapv #(normalize-rel-entry plugin %) (:extensions pi))
         :prompts    (mapv #(normalize-rel-entry plugin %) (:prompts pi))
         :themes     (mapv #(normalize-rel-entry plugin %) (:themes pi))})
      {:skills     (if (fs/directory? (fs/path real-dir "skills"))
                     [(str "./plugins/" plugin "/skills")]
                     [])
       :extensions []
       :prompts    []
       :themes     []})))

(defn- pi-aggregate-manifest [profile-id resolved]
  (let [contribs (map pi-plugin-contributions resolved)
        merge-k  (fn [k] (vec (distinct (mapcat k contribs))))
        pi-map   (into {}
                       (for [k [:skills :extensions :prompts :themes]
                             :let [xs (merge-k k)]
                             :when (seq xs)]
                         [k xs]))]
    {:name        (str "@ol-plugins-sync/profile-" profile-id)
     :private     true
     :description (str "Managed Pi plugin package for profile " profile-id)
     :type        "module"
     :keywords    ["pi-package" "skills"]
     :pi          pi-map}))

(defn- read-pi-settings [settings-path]
  (when (fs/exists? settings-path)
    (try
      (json/read-str (slurp settings-path) :key-fn keyword)
      (catch Exception _ nil))))

(defn- pi-settings-wired? [settings proj-dir]
  (boolean (some #(= proj-dir (if (map? %) (:source %) (str %)))
                 (:packages settings))))

(defn- wire-pi-settings! [profile proj-dir dry-run]
  (let [settings-path (str (fs/path (:dir profile) "settings.json"))
        symlink?      (Files/isSymbolicLink (fs/path settings-path))
        exists?       (fs/exists? settings-path)
        settings      (read-pi-settings settings-path)]
    (cond
      (and exists? (pi-settings-wired? settings proj-dir))
      {:wired? true :updated? false :warnings []}

      (and exists? (or symlink? (not (fs/writable? settings-path))))
      {:wired?   false
       :updated? false
       :warnings [(str settings-path " is read-only and does not include " proj-dir " in packages")]}

      exists?
      (let [updated (update settings :packages #(conj (vec %) proj-dir))]
        (when-not dry-run
          (spit settings-path (str (json/write-str updated :indent true :escape-slash false) "\n")))
        {:wired? true :updated? true :warnings []})

      :else
      {:wired?   false
       :updated? false
       :warnings [(str settings-path " does not exist; add " proj-dir " to Pi settings.json packages")]})))

(defn- bind-pi! [state-dir profile resolved {:keys [dry-run]}]
  (let [proj-dir      (or (:link-dir profile) (profile-state-dir state-dir (:id profile)))
        plugins-dir   (str (fs/path proj-dir "plugins"))
        pkg-path      (str (fs/path proj-dir "package.json"))
        prev-state    (read-profile-state state-dir (:id profile))
        desired       (into {} (map (fn [{:keys [plugin link-dir]}] [plugin link-dir]) resolved))
        changes       (sync-symlink-dir! plugins-dir desired (:managed-plugins prev-state []) dry-run)
        manifest      (pi-aggregate-manifest (:id profile) resolved)
        manifest-json (str (json/write-str manifest :indent true :escape-slash false) "\n")
        manifest-diff (not= manifest-json (when (fs/exists? pkg-path) (slurp pkg-path)))
        _             (when (and manifest-diff (not dry-run))
                        (fs/create-dirs proj-dir)
                        (spit pkg-path manifest-json))
        wiring        (wire-pi-settings! profile proj-dir dry-run)
        changed?      (boolean (or (seq (:added changes))
                                   (seq (:updated changes))
                                   (seq (:removed changes))
                                   manifest-diff
                                   (:updated? wiring)))]
    (when-not dry-run
      (write-profile-state! state-dir (:id profile)
                            {:harness         :pi
                             :dir             (:dir profile)
                             :link-dir        proj-dir
                             :managed-plugins (vec (sort (keys desired)))
                             :plugins         (mapv :selector resolved)}))
    {:profile  (:id profile)
     :harness  :pi
     :dir      (:dir profile)
     :link-dir proj-dir
     :status   (cond dry-run (if changed? :would-update :unchanged)
                     changed? :updated
                     :else :unchanged)
     :plugins  (mapv :selector resolved)
     :changes  changes
     :warnings (:warnings wiring)}))

(defn- codex-marketplace-name [pack-info]
  (let [mkt-path (fs/path (:path pack-info) ".agents" "plugins" "marketplace.json")]
    (if (fs/exists? mkt-path)
      (or (:name (json/read-str (slurp (str mkt-path)) :key-fn keyword))
          (:pack pack-info))
      (:pack pack-info))))

(defn- run-codex! [codex-bin codex-home args]
  (let [res (proc/sh (into [codex-bin] args)
                     {:extra-env {"CODEX_HOME" codex-home}
                      :out       :string
                      :err       :string
                      :continue  true})]
    res))

(defn- ensure-codex-ok! [res args]
  (when-not (zero? (:exit res))
    (throw (ex-info (str "codex command failed: " (str/join " " args)
                         (when-not (str/blank? (:err res))
                           (str ": " (str/trim (:err res)))))
                    {:code    :bind/codex-failed
                     :details {:args args :exit (:exit res) :stderr (str/trim (:err res))}}))))

(defn- desired-codex-stamp [resolved]
  (into {}
        (for [[pack entries] (group-by :pack resolved)
              :let [info (:pack-info (first entries))
                    mkt  (codex-marketplace-name info)]]
          [pack {:marketplace mkt
                 :root        (:active info)
                 :source      (:source info)
                 :digest      (:digest info)
                 :plugins     (vec (sort (map :plugin entries)))}])))

(defn- codex-installed-selectors [stamp]
  (set (for [[_ {:keys [marketplace plugins]}] stamp
             p plugins]
         (str p "@" marketplace))))

(defn- bind-codex! [state-dir profile resolved {:keys [codex-bin force dry-run]}]
  (let [codex-home (str (:dir profile))
        prev-state (read-profile-state state-dir (:id profile))
        prev-stamp (:codex-packs prev-state {})
        next-stamp (desired-codex-stamp resolved)
        prev-sels  (codex-installed-selectors prev-stamp)
        next-sels  (codex-installed-selectors next-stamp)
        removed    (vec (sort (remove next-sels prev-sels)))
        changed?   (or force (not= prev-stamp next-stamp))]
    (if (not changed?)
      {:profile  (:id profile)
       :harness  :codex
       :dir      codex-home
       :status   :unchanged
       :plugins  (mapv :selector resolved)
       :changes  {:added [] :updated [] :removed [] :unchanged (vec (sort next-sels))}
       :warnings []}
      (if dry-run
        {:profile  (:id profile)
         :harness  :codex
         :dir      codex-home
         :status   :would-update
         :plugins  (mapv :selector resolved)
         :changes  {:added     (vec (sort (remove prev-sels next-sels)))
                    :updated   (vec (sort (filter prev-sels next-sels)))
                    :removed   removed
                    :unchanged []}
         :warnings []}
        (do
          (when-not (fs/which codex-bin)
            (throw (ex-info (str "codex binary not found: " codex-bin)
                            {:code :bind/codex-missing :details {:codex-bin codex-bin}})))
          (fs/create-dirs codex-home)
          (doseq [[pack {:keys [marketplace root plugins] :as next-pack}] next-stamp
                  :let [prev-pack (get prev-stamp pack)]]
            (when (or force (not= prev-pack next-pack))
              (when (or force (not= (:root prev-pack) root))
                ;; Replace any previous store or local marketplace registration.
                (run-codex! codex-bin codex-home ["plugin" "marketplace" "remove" marketplace])
                (ensure-codex-ok!
                 (run-codex! codex-bin codex-home ["plugin" "marketplace" "add" root "--json"])
                 ["plugin" "marketplace" "add" root "--json"]))
              (doseq [p plugins
                      :let [sel (str p "@" marketplace)]]
                (ensure-codex-ok!
                 (run-codex! codex-bin codex-home ["plugin" "add" sel "--json"])
                 ["plugin" "add" sel "--json"]))))
          (doseq [sel removed]
            (run-codex! codex-bin codex-home ["plugin" "remove" sel "--json"]))
          (write-profile-state! state-dir (:id profile)
                                {:harness     :codex
                                 :dir         codex-home
                                 :codex-packs next-stamp
                                 :plugins     (mapv :selector resolved)})
          {:profile  (:id profile)
           :harness  :codex
           :dir      codex-home
           :status   :updated
           :plugins  (mapv :selector resolved)
           :changes  {:added     (vec (sort (remove prev-sels next-sels)))
                      :updated   (vec (sort (filter prev-sels next-sels)))
                      :removed   removed
                      :unchanged []}
           :warnings []})))))

(defn bind-profile!
  "Binds `profile` against `active-packs` using the appropriate harness adapter."
  [state-dir profile active-packs opts]
  (let [resolved (resolve-profile-plugins profile active-packs)]
    (case (:harness profile)
      :claude      (bind-claude! state-dir profile resolved opts)
      :antigravity (bind-antigravity! state-dir profile resolved opts)
      :pi          (bind-pi! state-dir profile resolved opts)
      :codex       (bind-codex! state-dir profile resolved opts))))

(defn- symlink-drift [target-dir desired-map managed-prev]
  (let [missing (for [[k v] desired-map
                      :let [cur (packs/read-symlink-target (str (fs/path target-dir k)))]
                      :when (not= cur v)]
                  k)
        stale   (for [k (remove #(contains? desired-map %) managed-prev)
                      :when (Files/isSymbolicLink (fs/path target-dir k))]
                  k)]
    {:missing (vec (sort missing))
     :stale   (vec (sort stale))}))

(defn- binding-root
  "Returns the directory that holds the plugin symlinks of `profile`, or nil for Codex."
  [state-dir profile]
  (case (:harness profile)
    :claude      (str (fs/path (:dir profile) "skills"))
    :antigravity (str (fs/path (:dir profile) "config" "plugins"))
    :pi          (str (fs/path (or (:link-dir profile) (profile-state-dir state-dir (:id profile))) "plugins"))
    :codex       nil))

(defn- linked-rev
  "Returns the revision that the plugin link at `link-path` resolves to: the
  `revs/<sha>` name, `\"local\"` for a local checkout, or nil for a missing link."
  [state-dir pack link-path]
  (when (fs/exists? link-path)
    (let [revs (fs/path (packs/pack-dir state-dir pack) "revs")
          real (fs/real-path link-path)]
      (if (and (fs/exists? revs) (.startsWith real (fs/real-path revs)))
        (str (first (.relativize (fs/real-path revs) real)))
        "local"))))

(defn- profile-loads
  "Returns `{pack rev}` with the revision of each pack that a new session of
  `profile` loads. A rev is a commit, `\"local\"`, or `\"mixed\"` when plugins of
  one pack resolve to different revisions. A pack with a missing binding is absent."
  [state-dir profile]
  (let [packs-used (set (map :pack (:plugins profile)))]
    (if (= :codex (:harness profile))
      (into {}
            (for [[pack {:keys [source digest]}] (:codex-packs (read-profile-state state-dir (:id profile)))
                  :when (contains? packs-used pack)]
              [pack (if (= :local source) "local" digest)]))
      (let [root (binding-root state-dir profile)]
        (into {}
              (for [[pack items] (group-by :pack (:plugins profile))
                    :let [revs (set (map #(linked-rev state-dir pack (str (fs/path root (:plugin %)))) items))]
                    :when (not (contains? revs nil))]
                [pack (if (= 1 (count revs)) (first revs) "mixed")]))))))

(defn profile-status
  "Inspects `profile` against current pack statuses without modifying disk."
  [state-dir profile pack-statuses]
  (let [prev-state (read-profile-state state-dir (:id profile))
        selectors  (mapv :selector (:plugins profile))
        missing-pk (vec (distinct (for [{:keys [pack]} (:plugins profile)
                                        :when (= :missing (:source (get pack-statuses pack :missing)))]
                                    pack)))]
    (assoc
     (if (seq missing-pk)
       {:profile  (:id profile)
        :harness  (:harness profile)
        :dir      (:dir profile)
        :source   (:source profile)
        :plugins  selectors
        :in-sync? false
        :drift    [(str "unfetched packs: " (str/join ", " missing-pk))]
        :warnings []}
       (let [desired (into {}
                           (for [{:keys [plugin pack]} (:plugins profile)]
                             [plugin (str (fs/path (packs/active-path state-dir pack) "plugins" plugin))]))]
         (case (:harness profile)
           :claude
           (let [{:keys [missing stale]} (symlink-drift (str (fs/path (:dir profile) "skills"))
                                                        desired
                                                        (:managed-plugins prev-state []))
                 drift                   (cond-> []
                                           (seq missing) (conj (str "unbound: " (str/join ", " missing)))
                                           (seq stale)   (conj (str "stale: " (str/join ", " stale))))]
             {:profile  (:id profile)
              :harness  :claude
              :dir      (:dir profile)
              :source   (:source profile)
              :plugins  selectors
              :in-sync? (empty? drift)
              :drift    drift
              :warnings []})

           :antigravity
           (let [{:keys [missing stale]} (symlink-drift (str (fs/path (:dir profile) "config" "plugins"))
                                                        desired
                                                        (:managed-plugins prev-state []))
                 drift                   (cond-> []
                                           (seq missing) (conj (str "unbound: " (str/join ", " missing)))
                                           (seq stale)   (conj (str "stale: " (str/join ", " stale))))
                 warnings                (vec (for [{:keys [plugin pack selector]} (:plugins profile)
                                                    :let [root (:resolved-path (get pack-statuses pack))
                                                          real (when root (str (fs/path root "plugins" plugin)))]
                                                    :when (and real (fs/directory? real)
                                                               (not (has-antigravity-manifest? real)))]
                                                (str selector " has no .antigravity-plugin/plugin.json")))]
             {:profile  (:id profile)
              :harness  :antigravity
              :dir      (:dir profile)
              :source   (:source profile)
              :plugins  selectors
              :in-sync? (empty? drift)
              :drift    drift
              :warnings warnings})

           :pi
           (let [proj-dir                (or (:link-dir profile) (profile-state-dir state-dir (:id profile)))
                 {:keys [missing stale]} (symlink-drift (str (fs/path proj-dir "plugins"))
                                                        desired
                                                        (:managed-plugins prev-state []))
                 pkg-ok?                 (fs/exists? (fs/path proj-dir "package.json"))
                 settings                (read-pi-settings (str (fs/path (:dir profile) "settings.json")))
                 wired?                  (pi-settings-wired? settings proj-dir)
                 drift                   (cond-> []
                                           (seq missing)  (conj (str "unbound: " (str/join ", " missing)))
                                           (seq stale)    (conj (str "stale: " (str/join ", " stale)))
                                           (not pkg-ok?)  (conj "missing package.json")
                                           (not wired?)   (conj (str "settings.json does not reference " proj-dir)))]
             {:profile  (:id profile)
              :harness  :pi
              :dir      (:dir profile)
              :link-dir proj-dir
              :source   (:source profile)
              :plugins  selectors
              :in-sync? (empty? drift)
              :drift    drift
              :warnings []})

           :codex
           (let [prev-packs (:codex-packs prev-state {})
                 expected   (into {}
                                  (for [[pack items] (group-by :pack (:plugins profile))
                                        :let [st (get pack-statuses pack)]]
                                    [pack {:root    (:active-path st)
                                           :source  (:source st)
                                           :digest  (if (= :local (:source st))
                                                      (packs/content-digest (:resolved-path st))
                                                      (:active-rev st))
                                           :plugins (vec (sort (map :plugin items)))}]))
                 drift      (vec (for [[pack exp] expected
                                       :let [cur (get prev-packs pack)]
                                       :when (not= (select-keys cur [:root :source :digest :plugins]) exp)]
                                   (str "pack " pack " needs codex plugin refresh")))]
             {:profile  (:id profile)
              :harness  :codex
              :dir      (:dir profile)
              :source   (:source profile)
              :plugins  selectors
              :in-sync? (empty? drift)
              :drift    drift
              :warnings []}))))
     :loads (profile-loads state-dir profile))))
