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

(def ^:private github "BRK_GA7KQ2XM")
(def ^:private anthropic "BRK_N3W5RBTE")
(def ^:private hetzner "BRK_Q4ZD6HUY")

(def ^:private headers
  {github "Authorization" anthropic "X-Api-Key" hetzner "Auth-Api-Token"})

(defn- request [identity groups credential host]
  {:command :decide
   :request (domain/parse-request
             {:op "decide" :identity identity :node (subs identity 4) :groups groups
              :modes ["inject"] :credential credential :host host :header (headers credential)
              :mode "inject"})})

(def seed-commands
  (let [actor "casey@github"
        save (fn [c] {:command :credential/create :actor actor :credential c})]
    [(save {:name github :vault agents-vault :item "itm0000000000000000000000a" :field "credential"
            :vault-title "Agents" :item-title "GitHub token" :field-label "credential"
            :header "Authorization" :hosts ["api.github.com" "*.githubusercontent.com"]})
     (save {:name anthropic :vault agents-vault :item "itm0000000000000000000000b" :field "credential"
            :vault-title "Agents" :item-title "Anthropic API" :field-label "credential"
            :header "x-api-key" :hosts ["api.anthropic.com"]})
     (save {:name hetzner :vault infra-vault :item "itm0000000000000000000000c" :field "password"
            :vault-title "Infra" :item-title "Hetzner DNS" :field-label "password"
            :header "Auth-API-Token" :hosts ["dns.hetzner.com"]})
     {:command :assignment/assign :credentials [github] :subjects ["group:agents"]
      :mode "inject" :approval "auto" :actor actor}
     {:command :assignment/assign :credentials [anthropic hetzner]
      :subjects ["group:agents" "tag:t3-bob"] :mode "inject" :approval "ask" :actor actor}
     (request "tag:t3-alice" ["agents"] github "api.github.com")
     (request "tag:t3-alice" ["agents"] anthropic "api.anthropic.com")
     (request "tag:t3-bob" ["agents" "github-readers"] hetzner "dns.hetzner.com")
     (request "tag:t3-bob" ["agents" "github-readers"] anthropic "api.anthropic.com")
     (request "tag:t3-carol" ["agents"] hetzner "dns.hetzner.com")
     (request "tag:t3-dave" [] hetzner "dns.hetzner.com")
     (request "tag:t3-alice" ["agents"] github "evil.example.com")
     {:command :approval/event :id 2 :event :approve :grant "24h" :actor actor}
     {:command :approval/event :id 3 :event :block :actor actor}
     (request "tag:t3-bob" ["agents" "github-readers"] anthropic "api.anthropic.com")]))

(defn start!
  "Starts a broker in directory `dir` with a fresh seeded database and a
  fake Connect server. The UI listens on `dir`/ui.sock. Returns the system
  with `:fake` added."
  [dir]
  (fs/create-dirs dir)
  (doseq [f ["broker.db" "broker.db-wal" "broker.db-shm"]]
    (fs/delete-if-exists (fs/path dir f)))
  (let [fake (fake-connect/start!)
        token-file (str (fs/path dir "token"))
        _ (spit token-file fake-connect/token)
        system (main/start! {:db                 (str (fs/path dir "broker.db"))
                             :socket             (str (fs/path dir "decide.sock"))
                             :ui-socket          (str (fs/path dir "ui.sock"))
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
