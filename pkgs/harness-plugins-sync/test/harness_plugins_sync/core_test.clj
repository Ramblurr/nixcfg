(ns harness-plugins-sync.core-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :as proc]
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [harness-plugins-sync.cli :as cli]
   [harness-plugins-sync.config :as cfg]
   [harness-plugins-sync.main :as main]
   [harness-plugins-sync.packs :as packs]
   [harness-plugins-sync.remote :as remote]
   [harness-plugins-sync.terminal :as term]
   [malli.core :as m]))

(def ^:private plain {:color-out? false :color-err? false :animate? false})

(defn- invoke [& args]
  (let [err  (java.io.StringWriter.)
        exit (volatile! nil)
        out  (with-out-str
               (binding [*err* err]
                 (vreset! exit (cli/run main/catalog main/impl (vec args) {:terminal plain}))))]
    {:exit @exit :out out :err (str err)}))

(defn- git! [dir & args]
  (let [res (proc/sh (into ["git" "-C" (str dir)] args)
                     {:out :string :err :string :continue true})]
    (when-not (zero? (:exit res))
      (throw (ex-info (str "git failed: " (:err res)) res)))
    (str/trim (:out res))))

(defn- write-plugin-repo! [repo-dir marketplace-name plugins]
  (fs/create-dirs repo-dir)
  (when-not (fs/exists? (fs/path repo-dir ".git"))
    (git! repo-dir "init" "-b" "main")
    (git! repo-dir "config" "user.name" "Test")
    (git! repo-dir "config" "user.email" "test@example.test"))
  (let [mkt-file (fs/path repo-dir ".agents" "plugins" "marketplace.json")]
    (fs/create-dirs (fs/parent mkt-file))
    (spit (str mkt-file)
          (json/write-str {:name marketplace-name
                           :plugins (mapv (fn [[p _]] {:name p :source {:source "local" :path (str "./plugins/" p)}})
                                          plugins)})))
  (doseq [[p {:keys [skill-body extension? agy-manifest?]}] plugins
          :let [pdir (fs/path repo-dir "plugins" p)]]
    (fs/create-dirs (fs/path pdir "skills" "demo"))
    (spit (str (fs/path pdir "skills" "demo" "SKILL.md"))
          (str "---\nname: demo-" p "\ndescription: Demo\n---\n" (or skill-body "v1") "\n"))
    (spit (str (fs/path pdir "package.json"))
          (json/write-str (cond-> {:name (str "@test/" p)
                                   :pi   (cond-> {:skills ["./skills"]}
                                           extension? (assoc :extensions ["./extensions/start.ts"]))})))
    (when extension?
      (fs/create-dirs (fs/path pdir "extensions"))
      (spit (str (fs/path pdir "extensions" "start.ts")) "export default function() {}\n"))
    (when agy-manifest?
      (fs/create-dirs (fs/path pdir ".antigravity-plugin"))
      (spit (str (fs/path pdir ".antigravity-plugin" "plugin.json"))
            (json/write-str {:name p :version "0.1.0"}))))
  (git! repo-dir "add" "-A")
  (git! repo-dir "commit" "-m" "update" "--allow-empty")
  (git! repo-dir "rev-parse" "HEAD"))

(defn- write-fake-codex! [bin-path log-path]
  (spit (str bin-path)
        (str "#!/bin/sh\n"
             "printf '%s|%s\\n' \"$CODEX_HOME\" \"$*\" >> " (pr-str (str log-path)) "\n"
             "printf '{}\\n'\n"))
  (fs/set-posix-file-permissions bin-path "rwxr-xr-x"))

(defn- write-fake-ssh! [bin-path log-path behind-json-path]
  (spit (str bin-path)
        (str "#!/bin/sh\n"
             "printf '%s\\n' \"$*\" >> " (pr-str (str log-path)) "\n"
             "for a in \"$@\"; do\n"
             "  case \"$a\" in\n"
             "    behind-peer) cat " (pr-str (str behind-json-path)) "; exit 1 ;;\n"
             "    offline-peer) echo 'ssh: connect failed' >&2; exit 255 ;;\n"
             "    auth-peer) printf 'To authenticate, visit:\\nhttps://login.tailscale.com/a/test12345\\n' >&2; sleep 30; exit 255 ;;\n"
             "    hang-peer) sleep 30; exit 255 ;;\n"
             "  esac\n"
             "done\n"
             "printf '{\"host\":\"remote\",\"dry-run\":false,\"pulled\":[],\"packs\":[],\"profiles\":[],\"peers\":[]}\\n'\n"))
  (fs/set-posix-file-permissions bin-path "rwxr-xr-x"))

(deftest help-and-schema-contracts
  (testing "help agrees across help, --help, and -h without side effects"
    (doseq [path [[] ["sync"] ["pull"] ["apply"] ["status"] ["config"] ["doctor"]]]
      (let [res (map #(apply invoke %) [(into ["help"] path) (conj path "--help") (conj path "-h")])]
        (is (apply = res) (str path))
        (is (= 0 (:exit (first res)))))))
  (testing "root help includes architecture and workflow guide"
    (let [out (:out (invoke "help"))]
      (is (str/includes? out "Architecture & workflow:"))
      (is (str/includes? out "ol-plugins-sync/config.edn"))))
  (testing "schema exports are self-contained EDN and JSON Schema"
    (doseq [cmd ["sync" "pull" "apply" "status" "config" "doctor"]]
      (let [edn-res  (invoke cmd "--schema" "all" "--edn")
            json-res (invoke cmd "--schema" "all" "--json")
            edn-data (edn/read-string (:out edn-res))
            js-data  (json/read-str (:out json-res))]
        (is (= [0 0] [(:exit edn-res) (:exit json-res)]) cmd)
        (is (= :malli (:schema-language edn-data)) cmd)
        (is (= "json-schema" (get js-data "schema-language")) cmd)
        (is (nil? (get-in edn-data [:input :limitations])) cmd)
        (is (nil? (get-in edn-data [:output :limitations])) cmd))))
  (testing "unknown commands, options, extra args, and flag conflicts exit 2"
    (doseq [[args code] [[["bogus"] "cli/unknown-command"]
                         [["sync" "--nope"] "cli/unknown-option"]
                         [["status" "extra"] "cli/unexpected-argument"]
                         [["sync" "--" "x"] "cli/unexpected-argument"]
                         [["sync" "--schema" "bogus"] "cli/usage"]]]
      (let [{:keys [exit out]} (apply invoke "--json" args)]
        (is (= 2 exit) (str args))
        (is (= code (get-in (json/read-str out) ["error" "code"])) (str args))))
    (let [{:keys [exit err]} (invoke "--json" "status" "--edn")]
      (is (= 2 exit))
      (is (str/includes? err "choose either --json or --edn")))))

(deftest json5-and-t3-discovery
  (let [tmp (fs/create-temp-dir {:prefix "hps-t3-"})]
    (try
      (let [t3-dir (fs/path tmp "t3" "userdata")
            t3-cfg (fs/path t3-dir "settings.json")
            cfg-f  (fs/path tmp "config.edn")]
        (fs/create-dirs t3-dir)
        (spit (str t3-cfg)
              (str "{\n"
                   "  // T3 Code settings with JSON5 comments and trailing commas\n"
                   "  \"providerInstances\": {\n"
                   "    \"pi-t3\": { \"driver\": \"pi\", \"enabled\": true, \"environment\": [{\"name\": \"PI_CODING_AGENT_DIR\", \"value\": \"" (fs/path tmp "pi-t3/agent") "\"}] },\n"
                   "    \"claude-work\": { \"driver\": \"claudeAgent\", \"enabled\": true, \"config\": {\"homePath\": \"" (fs/path tmp "claude-work") "\"} },\n"
                   "    \"codex\": { \"driver\": \"codex\", \"enabled\": true, \"config\": {\"homePath\": \"" (fs/path tmp "codex") "\"} },\n"
                   "    \"antigravity\": { \"driver\": \"antigravity\", \"enabled\": true },\n"
                   "    \"disabled-pi\": { \"driver\": \"pi\", \"enabled\": false },\n"
                   "  },\n"
                   "}\n"))
        (spit (str cfg-f)
              (pr-str {:state-dir                  (str (fs/path tmp "state"))
                       :t3-settings                (str t3-cfg)
                       :include-antigravity-global false
                       :defaults                   {:plugins ["my@harness-plugins" "pstack@harness-plugins"]}
                       :profiles                   {"pi-cli"      {:harness :pi :dir (str (fs/path tmp "pi-cli/agent"))}
                                                    "claude-work" {:exclude-plugins ["pstack@harness-plugins"]}}}))
        (let [loaded   (cfg/load-config (str cfg-f))
              profiles (cfg/resolve-profiles loaded)
              by-id    (into {} (map (juxt :id identity) profiles))]
          (is (= #{"antigravity" "claude-work" "codex" "pi-cli" "pi-t3"} (set (keys by-id))))
          (is (= ["my@harness-plugins"] (mapv :selector (:plugins (get by-id "claude-work")))))
          (is (= (str (fs/path t3-dir "providers" "antigravity" (cfg/sha256-hex "antigravity")))
                 (:dir (get by-id "antigravity"))))))
      (finally
        (fs/delete-tree tmp)))))

(deftest end-to-end-pull-apply-sync-and-pruning
  (let [tmp (fs/create-temp-dir {:prefix "hps-e2e-"})]
    (try
      (let [repo-a    (fs/path tmp "repo-skills")
            repo-b    (fs/path tmp "repo-local-pack")
            rev1      (write-plugin-repo! repo-a "harness-plugins"
                                          {"my"     {:skill-body "my-v1" :agy-manifest? true}
                                           "pstack" {:skill-body "pstack-v1" :extension? true :agy-manifest? true}
                                           "tstack" {:skill-body "tstack-v1" :agy-manifest? false}})
            _         (write-plugin-repo! repo-b "host-extra"
                                          {"moot" {:skill-body "moot-v1" :agy-manifest? true}})
            state-dir (fs/path tmp "state")
            claude-d  (fs/path tmp "claude")
            codex-d   (fs/path tmp "codex")
            pi-d      (fs/path tmp "pi" "agent")
            pi-t3-d   (fs/path tmp "pi-t3" "agent")
            agy-d     (fs/path tmp "agy")
            codex-log (fs/path tmp "codex.log")
            codex-bin (fs/path tmp "fake-codex")
            cfg-f     (fs/path tmp "config.edn")]
        (doseq [d [pi-d pi-t3-d]]
          (fs/create-dirs d)
          (spit (str (fs/path d "settings.json")) (json/write-str {:packages []})))
        (write-fake-codex! codex-bin codex-log)
        (spit (str cfg-f)
              (pr-str {:state-dir                  (str state-dir)
                       :discover-t3                false
                       :include-antigravity-global false
                       :codex-bin                  (str codex-bin)
                       :packs                      {"harness-plugins" {:url (str repo-a) :branch "main"}
                                                    "host-extra"      {:url (str repo-b) :branch "main"}}
                       :defaults                   {:plugins ["my@harness-plugins" "pstack@harness-plugins" "tstack@harness-plugins"]}
                       :profiles                   {"claude" {:harness :claude :dir (str claude-d) :extra-plugins ["moot@host-extra"]}
                                                    "codex"  {:harness :codex :dir (str codex-d)}
                                                    "pi"     {:harness :pi :dir (str pi-d)}
                                                    "pi-t3"  {:harness :pi :dir (str pi-t3-d) :plugins ["my@harness-plugins"]}
                                                    "agy"    {:harness :antigravity :dir (str agy-d)}}}))
        (testing "pull fetches staged snapshots without activating or binding profiles"
          (let [res  (invoke "pull" "--config" (str cfg-f) "--edn")
                data (edn/read-string (:out res))]
            (is (= 0 (:exit res)))
            (is (nil? (m/explain (:output (some #(when (= ["pull"] (:path %)) %) (:commands main/catalog))) data)))
            (is (not (fs/exists? (packs/active-path (str state-dir) "harness-plugins"))))
            (is (not (fs/exists? (fs/path claude-d "skills" "my"))))))

        (testing "apply promotes staged snapshots and binds all 4 harnesses natively"
          (let [res  (invoke "apply" "--config" (str cfg-f) "--edn")
                data (edn/read-string (:out res))
                js   (json/read-str (:out (invoke "status" "--config" (str cfg-f) "--json")))]
            (is (= 0 (:exit res)))
            (is (nil? (m/explain (:output (some #(when (= ["apply"] (:path %)) %) (:commands main/catalog))) data)))
            (is (= (str "revs/" rev1) (packs/read-symlink-target (packs/active-path (str state-dir) "harness-plugins"))))
            (is (fs/exists? (fs/path claude-d "skills" "my" "skills" "demo" "SKILL.md")))
            (is (fs/exists? (fs/path claude-d "skills" "moot" "skills" "demo" "SKILL.md")))
            (is (fs/exists? (fs/path agy-d "config" "plugins" "my" ".antigravity-plugin" "plugin.json")))
            ;; Pi aggregate package.json carries both skills and pstack extension
            (let [pi-pkg (json/read-str (slurp (str (fs/path state-dir "profiles" "pi" "package.json"))) :key-fn keyword)
                  pi-set (json/read-str (slurp (str (fs/path pi-d "settings.json"))) :key-fn keyword)]
              (is (= ["./plugins/pstack/extensions/start.ts"] (get-in pi-pkg [:pi :extensions])))
              (is (some #{(str (fs/path state-dir "profiles" "pi"))} (:packages pi-set))))
            ;; Codex ran marketplace add + 3 plugin adds
            (let [lines (str/split-lines (slurp (str codex-log)))]
              (is (some #(str/includes? % "plugin marketplace add") lines))
              (is (some #(str/includes? % "plugin add pstack@harness-plugins") lines)))
            ;; Antigravity warning emitted for tstack lacking .antigravity-plugin/plugin.json
            (is (some #(str/includes? % "tstack@harness-plugins")
                      (:warnings (some #(when (= "agy" (:profile %)) %) (:profiles data)))))
            ;; Status JSON agrees and shows claude/codex/agy/pi in-sync
            (is (= true (get (some #(when (= "claude" (get % "profile")) %) (get js "profiles")) "in-sync?")))))

        (testing "second apply is idempotent and skips codex refresh"
          (let [before (count (str/split-lines (slurp (str codex-log))))
                res    (invoke "apply" "--config" (str cfg-f) "--edn")
                data   (edn/read-string (:out res))
                after  (count (str/split-lines (slurp (str codex-log))))]
            (is (= 0 (:exit res)))
            (is (= before after))
            (is (every? #(= :unchanged (:status %)) (:profiles data)))))

        (testing "new git commit + sync refreshes active rev and codex cache"
          (let [rev2   (write-plugin-repo! repo-a "harness-plugins"
                                           {"my"     {:skill-body "my-v2" :agy-manifest? true}
                                            "pstack" {:skill-body "pstack-v2" :extension? true :agy-manifest? true}
                                            "tstack" {:skill-body "tstack-v2" :agy-manifest? true}})
                before (count (str/split-lines (slurp (str codex-log))))
                res    (invoke "sync" "--config" (str cfg-f) "--edn")
                data   (edn/read-string (:out res))
                after  (count (str/split-lines (slurp (str codex-log))))]
            (is (= 0 (:exit res)))
            (is (nil? (m/explain (:output (some #(when (= ["sync"] (:path %)) %) (:commands main/catalog))) data)))
            (is (= (str "revs/" rev2) (packs/read-symlink-target (packs/active-path (str state-dir) "harness-plugins"))))
            (is (str/includes? (slurp (str (fs/path claude-d "skills" "my" "skills" "demo" "SKILL.md"))) "my-v2"))
            (is (> after before))
            (is (= rev1 (:previous-rev (some #(when (= "harness-plugins" (:pack %)) %) (:packs data)))))
            (is (= #{rev2} (set (map #(get (:loads %) "harness-plugins") (:profiles data))))
                "every profile, Codex included, loads the new revision")
            (let [out (binding [term/*terminal* plain]
                        (with-out-str (#'main/render-sync data)))]
              (is (str/includes? out (str (subs rev1 0 7) " -> " (subs rev2 0 7))))
              (is (str/includes? out (str "✓ " (:host data))))
              (is (str/includes? out "(plugin cache refreshed)"))
              (is (str/includes? out "/clear (T3)"))
              (is (not (str/includes? out "NO"))))))

        (testing "--local overrides active to local checkout and --pack does not prune other packs"
          (spit (str (fs/path repo-a "plugins" "my" "skills" "demo" "SKILL.md"))
                "---\nname: demo-my\ndescription: Demo\n---\nlocal-uncommitted\n")
          (let [res (invoke "apply" "--config" (str cfg-f)
                            "--pack" "harness-plugins"
                            "--local" (str "harness-plugins=" repo-a)
                            "--edn")]
            (is (= 0 (:exit res)))
            (is (= (str repo-a) (packs/read-symlink-target (packs/active-path (str state-dir) "harness-plugins"))))
            (is (str/includes? (slurp (str (fs/path claude-d "skills" "my" "skills" "demo" "SKILL.md")))
                               "local-uncommitted"))
            ;; moot@host-extra symlink on claude must still exist
            (is (fs/exists? (fs/path claude-d "skills" "moot")))))

        (testing "a failed profile is reported as behind and does not stop other profiles"
          (fs/delete (fs/path claude-d "skills" "tstack"))
          (fs/create-dirs (fs/path claude-d "skills" "tstack"))
          (let [res      (invoke "sync" "--config" (str cfg-f) "--edn")
                data     (edn/read-string (:out res))
                by-id    (into {} (map (juxt :profile identity) (:profiles data)))
                human    (invoke "status" "--config" (str cfg-f))]
            (is (= 1 (:exit res)))
            (is (= :failed (get-in by-id ["claude" :status])))
            (is (str/includes? (get-in by-id ["claude" :error]) "refusing to overwrite"))
            (is (not-any? #(= :failed (get-in by-id [% :status])) ["codex" "pi" "pi-t3" "agy"]))
            (is (= 1 (:exit human)))
            (is (str/includes? (:out human) "✗ "))
            (is (re-find #"claude\s+claude\s+\S+\s+NO" (:out human))))))
      (finally
        (fs/delete-tree tmp)))))

(deftest remote-fanout-and-doctor
  (let [tmp (fs/create-temp-dir {:prefix "hps-rem-"})]
    (try
      (let [ssh-log (fs/path tmp "ssh.log")
            ssh-bin (fs/path tmp "fake-ssh")
            behind  (fs/path tmp "behind.json")
            cfg-f   (fs/path tmp "config.edn")]
        (spit (str behind)
              (json/write-str {:host     "behind"
                               :user     "bob"
                               :local?   true
                               :dry-run  false
                               :targets  {"harness-plugins" "bbbbbbbbbb"}
                               :packs    []
                               :profiles [{:profile  "claude"
                                           :harness  "claude"
                                           :plugins  ["my@harness-plugins"]
                                           :status   "unchanged"
                                           :loads    {"harness-plugins" "aaaaaaaaaa"}
                                           :in-sync? true
                                           :drift    []
                                           :warnings []}]
                               :peers    []}))
        (write-fake-ssh! ssh-bin ssh-log behind)
        (spit (str cfg-f)
              (pr-str {:state-dir                  (str (fs/path tmp "state"))
                       :discover-t3                false
                       :include-antigravity-global false
                       :ssh-bin                    (str ssh-bin)
                       :peers                      ["good-peer" "offline-peer"]
                       :profiles                   {}}))
        (testing "remote fanout aggregates reachable and unreachable peers without aborting"
          (let [res  (invoke "status" "--config" (str cfg-f) "--host" "good-peer" "--host" "offline-peer" "--edn")
                data (edn/read-string (:out res))]
            (is (= 1 (:exit res)) "an unreached host is not current")
            (is (= [{:host "good-peer" :status :ok}
                    {:host "offline-peer" :status :unreachable}]
                   (mapv #(select-keys % [:host :status]) (:peers data))))
            (is (not (str/includes? (:out res) "\u001b")))))
        (testing "remote fanout fails fast on Tailscale SSH auth URL and on timeout"
          (let [start (System/nanoTime)
                res   (invoke "status" "--config" (str cfg-f) "--host" "auth-peer" "--edn")
                ms    (/ (- (System/nanoTime) start) 1e6)
                data  (edn/read-string (:out res))]
            (is (= 1 (:exit res)))
            (is (< ms 5000) "should fail fast without waiting for sleep 30")
            (is (= [{:host     "auth-peer"
                     :status   :auth-required
                     :auth-url "https://login.tailscale.com/a/test12345"
                     :message  "Tailscale SSH auth required: https://login.tailscale.com/a/test12345"}]
                   (:peers data))))
          (let [res (remote/run-remote (str ssh-bin) "hang-peer" "status" {} 300)]
            (is (= :unreachable (:status res)))
            (is (str/includes? (:message res) "timed out"))))
        (testing "a peer that exits 1 with a result shows its profiles as behind"
          (let [res  (invoke "status" "--config" (str cfg-f) "--host" "behind-peer" "--edn")
                data (edn/read-string (:out res))
                out  (:out (invoke "status" "--config" (str cfg-f) "--host" "behind-peer"))]
            (is (= 1 (:exit res)))
            (is (= {:host "behind-peer" :status :ok} (select-keys (first (:peers data)) [:host :status])))
            (is (str/includes? out "✗ behind (bob)"))
            (is (re-find #"claude\s+claude\s+aaaaaaa\s+NO" out))))
        (testing "target-plan matches user@host peers against local user and host"
          (is (= {:local? true :remotes ["ramblurr@witt" "hirde@quine"]}
                 (remote/target-plan ["ramblurr@quine" "ramblurr@witt" "hirde@quine"]
                                     true [] "quine" "ramblurr"))))
        (testing "doctor runs read-only and matches its schema"
          (let [res  (invoke "doctor" "--config" (str cfg-f) "--edn")
                data (edn/read-string (:out res))]
            (is (= 0 (:exit res)))
            (is (nil? (m/explain (:output (some #(when (= ["doctor"] (:path %)) %) (:commands main/catalog))) data)))
            (is (not (fs/exists? (fs/path tmp "state"))))))
        (testing "config runs read-only and matches its schema"
          (let [res  (invoke "config" "--config" (str cfg-f) "--edn")
                data (edn/read-string (:out res))]
            (is (= 0 (:exit res)))
            (is (= true (:config-exists? data)))
            (is (= ["good-peer" "offline-peer"] (:peers data)))
            (is (nil? (m/explain (:output (some #(when (= ["config"] (:path %)) %) (:commands main/catalog))) data))))))
      (finally
        (fs/delete-tree tmp)))))
