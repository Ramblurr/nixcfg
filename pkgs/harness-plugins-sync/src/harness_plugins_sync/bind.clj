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

(defn- marketplace-error [plugin-name mkt msg]
  (ex-info (str "plugin '" plugin-name "' " msg)
           {:code    :pack/invalid-marketplace
            :details {:plugin plugin-name :marketplace (some-> mkt str)}}))

(defn- marketplace-entry
  "Returns the entry for `plugin-name` in the `.agents/plugins/marketplace.json`
  of `pack-root`, or nil when the pack has no marketplace or does not list it.
  Throws an `ex-info` for a marketplace it cannot read, so callers fail only
  this pack."
  [pack-root plugin-name]
  (let [mkt   (fs/path pack-root ".agents" "plugins" "marketplace.json")
        mkt-v (when (fs/exists? mkt)
                (try
                  (json/read-str (slurp (str mkt)) :key-fn keyword)
                  (catch Exception e
                    (throw (marketplace-error plugin-name mkt
                                              (str "has an unreadable marketplace " mkt ": " (ex-message e)))))))]
    (when (and (map? mkt-v) (sequential? (:plugins mkt-v)))
      (some #(when (and (map? %) (= plugin-name (:name %))) %) (:plugins mkt-v)))))

(defn- entry-subpath
  "Returns the path relative to its pack root that the marketplace `entry` of
  `plugin-name` names, or `plugins/<name>` when the marketplace does not list
  it. A pack such as a single-plugin repository lists its plugin at `./`."
  [plugin-name entry]
  (let [source (:source entry)
        fail   #(throw (marketplace-error plugin-name nil %))
        rel    (fs/normalize
                (fs/path (cond
                           (nil? entry)     (str "plugins/" plugin-name)
                           (string? source) source
                           (and (map? source) (= "local" (:source source)) (string? (:path source)))
                           (:path source)
                           :else            (fail (str "has a marketplace source that is not a local path: "
                                                       (pr-str source))))))]
    (when (or (fs/absolute? rel) (= ".." (str (first rel))))
      (fail (str "source path leaves its pack: " rel)))
    (str rel)))

(defn- plugin-subpath [pack-root plugin-name]
  (entry-subpath plugin-name (marketplace-entry pack-root plugin-name)))

(defn- normalize-rel-entry [plugin-name rel]
  (let [clean (-> (str rel)
                  (str/replace #"^\./" ""))]
    (str "./plugins/" plugin-name "/" clean)))

(defn- pi-plugin-contributions
  "Returns the Pi resources of a plugin: its `package.json` `pi` manifest, or
  its `skills/` directory when it has no manifest. A `package.json` written for
  another tool, such as a VS Code manifest, counts as no manifest."
  [{:keys [plugin real-dir]}]
  (let [pkg-path (fs/path real-dir "package.json")
        pi       (when (fs/exists? pkg-path)
                   (:pi (json/read-str (slurp (str pkg-path)) :key-fn keyword)))]
    (if pi
      {:skills     (mapv #(normalize-rel-entry plugin %) (:skills pi))
       :extensions (mapv #(normalize-rel-entry plugin %) (:extensions pi))
       :prompts    (mapv #(normalize-rel-entry plugin %) (:prompts pi))
       :themes     (mapv #(normalize-rel-entry plugin %) (:themes pi))}
      {:skills     (if (fs/directory? (fs/path real-dir "skills"))
                     [(str "./plugins/" plugin "/skills")]
                     [])
       :extensions []
       :prompts    []
       :themes     []})))

(defn- plugin-dir [root subpath]
  (str (fs/normalize (fs/path root subpath))))

(defn- resolve-plugin-dir [pack-info plugin-name]
  (let [entry    (marketplace-entry (:path pack-info) plugin-name)
        subpath  (entry-subpath plugin-name entry)
        real-dir (plugin-dir (:path pack-info) subpath)
        link-dir (plugin-dir (:active pack-info) subpath)]
    (when-not (fs/directory? real-dir)
      (throw (ex-info (str "plugin '" plugin-name "' not found in pack '" (:pack pack-info)
                           "' at " real-dir)
                      {:code    :pack/plugin-not-found
                       :details {:pack (:pack pack-info) :plugin plugin-name :path real-dir}})))
    (when-not (.startsWith (fs/real-path real-dir) (fs/real-path (:path pack-info)))
      (throw (ex-info (str "plugin '" plugin-name "' source path leaves its pack: " subpath
                           " resolves to " (fs/real-path real-dir))
                      {:code    :pack/invalid-marketplace
                       :details {:pack (:pack pack-info) :plugin plugin-name :path real-dir}})))
    ;; Parsed here so a broken package.json fails this plugin, not the whole Pi profile.
    (let [pi (try
               (pi-plugin-contributions {:plugin plugin-name :real-dir real-dir})
               (catch Exception e
                 (throw (ex-info (str "plugin '" plugin-name "' has an unreadable package.json: " (ex-message e))
                                 {:code    :pack/invalid-plugin
                                  :details {:pack (:pack pack-info) :plugin plugin-name :path real-dir}}))))]
      {:plugin    plugin-name
       :pack      (:pack pack-info)
       :selector  (str plugin-name "@" (:pack pack-info))
       :link-dir  link-dir
       :real-dir  real-dir
       :entry     entry
       :pi        pi
       :pack-info pack-info})))

(defn check-pack!
  "Throws an `ex-info` unless each of `plugin-names` is a directory inside the
  pack snapshot at `pack-root`. Run before a snapshot becomes `active`."
  [pack-name pack-root plugin-names]
  (doseq [p plugin-names]
    (resolve-plugin-dir {:pack pack-name :path pack-root :active pack-root} p)))

(defn- resolve-profile-plugins
  "Returns `[resolved warnings]` for `profile`. A plugin whose pack has no
  active snapshot, or that its snapshot cannot provide, is left out with a
  warning so the profile still binds its other plugins."
  [profile active-packs]
  (let [results  (for [{:keys [plugin pack selector]} (:plugins profile)]
                   (if-let [info (get active-packs pack)]
                     (try
                       {:resolved (resolve-plugin-dir info plugin)}
                       (catch clojure.lang.ExceptionInfo e
                         {:warning (str selector " is not bound: " (ex-message e))}))
                     {:warning (str "pack " pack " has no active snapshot; " selector " is not bound")}))
        resolved (vec (keep :resolved results))]
    (when (#{:claude :pi :antigravity} (:harness profile))
      (let [dups (->> resolved (group-by :plugin) (filter #(> (count (val %)) 1)))]
        (when-let [[plugin-name entries] (first dups)]
          (throw (ex-info (str "profile '" (:id profile) "' has conflicting plugin name '" plugin-name
                               "' from packs: " (str/join ", " (map :pack entries)))
                          {:code    :bind/plugin-collision
                           :details {:profile (:id profile)
                                     :plugin  plugin-name
                                     :packs   (mapv :pack entries)}})))))
    [resolved (vec (keep :warning results))]))

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

(defn- pi-aggregate-manifest [profile-id resolved]
  (let [contribs (map :pi resolved)
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

(defn- codex-root
  "Returns the directory of the marketplace generated for `pack` at `digest`
  with `plugins`. Any change to either gets a new directory, which makes Codex
  register it again; Codex resolves a marketplace path only when it is added."
  [state-dir profile-id pack digest plugins]
  (let [md  (java.security.MessageDigest/getInstance "SHA-256")
        key (->> (.digest md (.getBytes (pr-str [digest plugins]) "UTF-8"))
                 (take 8)
                 (map #(format "%02x" (bit-and % 0xff)))
                 (apply str))]
    (str (fs/path (profile-state-dir state-dir profile-id) "codex" pack key))))

(defn- desired-codex-stamp
  "Returns `{pack stamp}`. Each pack registers in Codex as a marketplace named
  after its config key, from a generated copy of its plugins. Codex keys
  marketplaces by name and reads the copy for as long as it is registered, so
  neither an upstream marketplace name nor a pruned snapshot can reach
  another pack."
  [state-dir profile-id resolved]
  (into {}
        (for [[pack entries] (group-by :pack resolved)
              :let [info    (:pack-info (first entries))
                    plugins (vec (sort (map :plugin entries)))]]
          [pack {:marketplace pack
                 :root        (codex-root state-dir profile-id pack (:digest info) plugins)
                 :source      (:source info)
                 :digest      (:digest info)
                 :plugins     plugins}])))

(defn- write-codex-marketplace!
  "Writes the marketplace for `pack` at `root` from its resolved `entries`,
  unless a previous run already wrote it."
  [root pack entries]
  (when-not (fs/exists? root)
    (let [tmp (str root ".tmp." (System/nanoTime))]
      (try
        (fs/create-dirs (fs/path tmp ".agents" "plugins"))
        (doseq [{:keys [plugin real-dir]} entries]
          (fs/copy-tree real-dir (fs/path tmp "plugins" plugin)))
        (spit (str (fs/path tmp ".agents" "plugins" "marketplace.json"))
              (json/write-str {:name    pack
                               :plugins (vec (for [{:keys [plugin entry]} (sort-by :plugin entries)]
                                               (assoc entry
                                                      :name plugin
                                                      :source {:source "local" :path (str "./plugins/" plugin)})))}
                              :escape-slash false))
        (fs/move tmp root)
        (catch Exception e
          (fs/delete-tree tmp)
          (throw e))))))

(defn- rollback-codex-pack!
  "Undoes a registration of `next-pack` that failed part way. Its new plugins
  and its marketplace go, and `prev-pack`, when its generated copy still
  exists, is registered again, so Codex never points at a deleted directory."
  [codex-bin codex-home prev-pack next-pack]
  (doseq [p (remove (set (:plugins prev-pack)) (:plugins next-pack))]
    (run-codex! codex-bin codex-home ["plugin" "remove" (str p "@" (:marketplace next-pack)) "--json"]))
  (run-codex! codex-bin codex-home ["plugin" "marketplace" "remove" (:marketplace next-pack)])
  (when (and prev-pack (fs/exists? (:root prev-pack)))
    (run-codex! codex-bin codex-home ["plugin" "marketplace" "add" (:root prev-pack) "--json"])
    (doseq [p (:plugins prev-pack)]
      (run-codex! codex-bin codex-home ["plugin" "add" (str p "@" (:marketplace prev-pack)) "--json"]))))

(defn- codex-installed-selectors [stamp]
  (set (for [[_ {:keys [marketplace plugins]}] stamp
             p plugins]
         (str p "@" marketplace))))

(defn- bind-codex! [state-dir profile resolved {:keys [codex-bin force dry-run]}]
  (let [codex-home (str (:dir profile))
        prev-state (read-profile-state state-dir (:id profile))
        prev-stamp (:codex-packs prev-state {})
        next-stamp (desired-codex-stamp state-dir (:id profile) resolved)
        prev-sels  (codex-installed-selectors prev-stamp)
        next-sels  (codex-installed-selectors next-stamp)
        changed?   (or force (not= prev-stamp next-stamp))]
    (cond
      (not changed?)
      {:profile  (:id profile)
       :harness  :codex
       :dir      codex-home
       :status   :unchanged
       :plugins  (mapv :selector resolved)
       :changes  {:added [] :updated [] :removed [] :unchanged (vec (sort next-sels))}
       :warnings []}

      dry-run
      {:profile  (:id profile)
       :harness  :codex
       :dir      codex-home
       :status   :would-update
       :plugins  (mapv :selector resolved)
       :changes  {:added     (vec (sort (remove prev-sels next-sels)))
                  :updated   (vec (sort (filter prev-sels next-sels)))
                  :removed   (vec (sort (remove next-sels prev-sels)))
                  :unchanged []}
       :warnings []}

      :else
      (do
        (when-not (fs/which codex-bin)
          (throw (ex-info (str "codex binary not found: " codex-bin)
                          {:code :bind/codex-missing :details {:codex-bin codex-bin}})))
        (fs/create-dirs codex-home)
        ;; Each pack registers on its own: a pack Codex rejects keeps its previous
        ;; stamp, so the next run retries it, and the other packs still install.
        (let [by-pack    (group-by :pack resolved)
              failures   (into {}
                               (for [[pack {:keys [marketplace root plugins] :as next-pack}] next-stamp
                                     :let [prev-pack (get prev-stamp pack)]
                                     :when (or force (not= prev-pack next-pack))
                                     :let [error (try
                                                   (when (or force (not= (:root prev-pack) root))
                                                     (write-codex-marketplace! root pack (get by-pack pack))
                                                     (run-codex! codex-bin codex-home ["plugin" "marketplace" "remove" marketplace])
                                                     (ensure-codex-ok!
                                                      (run-codex! codex-bin codex-home ["plugin" "marketplace" "add" root "--json"])
                                                      ["plugin" "marketplace" "add" root "--json"]))
                                                   (doseq [p plugins
                                                           :let [sel (str p "@" marketplace)]]
                                                     (ensure-codex-ok!
                                                      (run-codex! codex-bin codex-home ["plugin" "add" sel "--json"])
                                                      ["plugin" "add" sel "--json"]))
                                                   nil
                                                   (catch Exception e
                                                     (rollback-codex-pack! codex-bin codex-home prev-pack next-pack)
                                                     (or (ex-message e) (str e))))]
                                     :when error]
                                 [pack error]))
              applied    (reduce-kv (fn [stamp pack _]
                                      (if-let [prev-pack (get prev-stamp pack)]
                                        (assoc stamp pack prev-pack)
                                        (dissoc stamp pack)))
                                    next-stamp
                                    failures)
              ;; A rejected pack keeps its previous stamp, so its installed plugins stay.
              uninstall  (vec (sort (remove (codex-installed-selectors applied) prev-sels)))
              live-mkts  (set (map :marketplace (vals applied)))
              stale-mkts (sort (distinct (remove live-mkts (map :marketplace (vals prev-stamp)))))
              live-roots (set (map :root (vals applied)))
              codex-dir  (fs/path (profile-state-dir state-dir (:id profile)) "codex")]
          (doseq [sel uninstall]
            (run-codex! codex-bin codex-home ["plugin" "remove" sel "--json"]))
          ;; Unregister before deleting, so Codex never points at a missing marketplace.
          (doseq [mkt stale-mkts]
            (run-codex! codex-bin codex-home ["plugin" "marketplace" "remove" mkt]))
          (when (fs/directory? codex-dir)
            (doseq [pack-dir  (fs/list-dir codex-dir)
                    generated (fs/list-dir pack-dir)
                    :when (not (contains? live-roots (str generated)))]
              (fs/delete-tree generated)))
          (write-profile-state! state-dir (:id profile)
                                {:harness     :codex
                                 :dir         codex-home
                                 :codex-packs applied
                                 :plugins     (mapv :selector resolved)})
          {:profile  (:id profile)
           :harness  :codex
           :dir      codex-home
           :status   :updated
           :plugins  (mapv :selector resolved)
           :changes  {:added     (vec (sort (remove prev-sels next-sels)))
                      :updated   (vec (sort (filter prev-sels next-sels)))
                      :removed   uninstall
                      :unchanged []}
           :warnings (vec (for [[pack error] (sort failures)]
                            (str "codex could not register pack " pack ": " error)))})))))

(defn bind-profile!
  "Binds `profile` against `active-packs` using the appropriate harness adapter.
  Plugins that cannot be resolved are left out and named in the warnings."
  [state-dir profile active-packs opts]
  (let [[resolved warnings] (resolve-profile-plugins profile active-packs)
        result              (case (:harness profile)
                              :claude      (bind-claude! state-dir profile resolved opts)
                              :antigravity (bind-antigravity! state-dir profile resolved opts)
                              :pi          (bind-pi! state-dir profile resolved opts)
                              :codex       (bind-codex! state-dir profile resolved opts))]
    (update result :warnings into warnings)))

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
                                    pack)))
        ;; An unreadable pack marketplace is drift of this profile, not a failed command.
        desired    (when (empty? missing-pk)
                     (try
                       (into {}
                             (for [{:keys [plugin pack]} (:plugins profile)
                                   :let [active (packs/active-path state-dir pack)]]
                               [plugin (plugin-dir active (plugin-subpath active plugin))]))
                       (catch clojure.lang.ExceptionInfo e e)))
        unbound    (fn [drift]
                     {:profile  (:id profile)
                      :harness  (:harness profile)
                      :dir      (:dir profile)
                      :source   (:source profile)
                      :plugins  selectors
                      :in-sync? false
                      :drift    [drift]
                      :warnings []})]
    (assoc
     (cond
       (seq missing-pk)
       (unbound (str "unfetched packs: " (str/join ", " missing-pk)))

       (instance? Exception desired)
       (unbound (ex-message desired))

       :else
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
                                                        real (when root (plugin-dir root (plugin-subpath root plugin)))]
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
                                      :let [st      (get pack-statuses pack)
                                            digest  (if (= :local (:source st))
                                                      (packs/content-digest (:resolved-path st))
                                                      (:active-rev st))
                                            plugins (vec (sort (map :plugin items)))]]
                                  [pack {:marketplace pack
                                         :root        (codex-root state-dir (:id profile) pack digest plugins)
                                         :source      (:source st)
                                         :digest      digest
                                         :plugins     plugins}]))
               drift      (vec (for [[pack exp] expected
                                     :let [cur (get prev-packs pack)]
                                     :when (not= (select-keys cur [:marketplace :root :source :digest :plugins]) exp)]
                                 (str "pack " pack " needs codex plugin refresh")))]
           {:profile  (:id profile)
            :harness  :codex
            :dir      (:dir profile)
            :source   (:source profile)
            :plugins  selectors
            :in-sync? (empty? drift)
            :drift    drift
            :warnings []})))
     :loads (profile-loads state-dir profile))))
