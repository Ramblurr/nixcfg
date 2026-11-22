(ns ts-broker.dev
  "Runs the broker against the fake Connect server with a seeded database,
  for driving the UI by hand."
  (:require
   [babashka.fs :as fs]
   [ts-broker.domain :as domain]
   [ts-broker.fake-connect :as fake-connect]
   [ts-broker.main :as main]
   [ts-broker.store :as store]))

(def ^:private agents-vault "vlt0000000000000000000000a")
(def ^:private infra-vault "vlt0000000000000000000000b")

(def ^:private headers
  {"GITHUB_TOKEN" "Authorization" "ANTHROPIC_API_KEY" "X-Api-Key" "HETZNER_DNS" "Auth-Api-Token"})

(defn- request [identity groups credential host]
  {:command :decide
   :request (domain/parse-request
             {:op "decide" :identity identity :node (subs identity 4) :groups groups
              :modes ["inject"] :credential credential :host host :header (headers credential)
              :mode "inject"})})

(def seed-commands
  (let [actor "casey@github"
        save (fn [c] {:command :credential/create :actor actor :credential c})]
    [(save {:name "GITHUB_TOKEN" :vault agents-vault :item "itm0000000000000000000000a"
            :field "credential" :field-label "credential" :header "Authorization"
            :hosts ["api.github.com" "*.githubusercontent.com"]})
     (save {:name "ANTHROPIC_API_KEY" :vault agents-vault :item "itm0000000000000000000000b"
            :field "credential" :field-label "credential" :header "x-api-key"
            :hosts ["api.anthropic.com"]})
     (save {:name "HETZNER_DNS" :vault infra-vault :item "itm0000000000000000000000c"
            :field "password" :field-label "password" :header "Auth-API-Token"
            :hosts ["dns.hetzner.com"]})
     {:command :assignment/assign :credentials ["GITHUB_TOKEN"] :subjects ["group:agents"]
      :mode "inject" :approval "auto" :actor actor}
     {:command :assignment/assign :credentials ["ANTHROPIC_API_KEY" "HETZNER_DNS"]
      :subjects ["group:agents" "tag:t3-bob"] :mode "inject" :approval "ask" :actor actor}
     (request "tag:t3-alice" ["agents"] "GITHUB_TOKEN" "api.github.com")
     (request "tag:t3-alice" ["agents"] "ANTHROPIC_API_KEY" "api.anthropic.com")
     (request "tag:t3-bob" ["agents" "github-readers"] "HETZNER_DNS" "dns.hetzner.com")
     (request "tag:t3-bob" ["agents" "github-readers"] "ANTHROPIC_API_KEY" "api.anthropic.com")
     (request "tag:t3-carol" ["agents"] "HETZNER_DNS" "dns.hetzner.com")
     (request "tag:t3-dave" [] "HETZNER_DNS" "dns.hetzner.com")
     (request "tag:t3-alice" ["agents"] "GITHUB_TOKEN" "evil.example.com")
     {:command :approval/event :id 2 :event :approve :grant "24h" :actor actor}
     {:command :approval/event :id 3 :event :block :actor actor}
     (request "tag:t3-bob" ["agents" "github-readers"] "ANTHROPIC_API_KEY" "api.anthropic.com")]))

(defn start!
  "Starts a broker in directory `dir` with a fresh seeded database and a
  fake Connect server. Returns the system with `:fake` added."
  [dir port]
  (fs/create-dirs dir)
  (doseq [f ["broker.db" "broker.db-wal" "broker.db-shm"]]
    (fs/delete-if-exists (fs/path dir f)))
  (let [fake (fake-connect/start!)
        token-file (str (fs/path dir "token"))
        _ (spit token-file fake-connect/token)
        system (main/start! {:db                 (str (fs/path dir "broker.db"))
                             :socket             (str (fs/path dir "decide.sock"))
                             :port               port
                             :connect-url        (:url fake)
                             :connect-token-file token-file})]
    (doseq [cmd seed-commands]
      (store/write! (:store system) cmd))
    (assoc system :fake fake)))

(defn stop!
  "Stops a system from [[start!]]."
  [system]
  (main/stop! system)
  ((get-in system [:fake :stop])))
