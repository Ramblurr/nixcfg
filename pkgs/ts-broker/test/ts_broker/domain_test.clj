(ns ts-broker.domain-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [ts-broker.domain :as domain])
  (:import
   [java.time Duration Instant]))

(def now (Instant/parse "2026-11-22T10:00:00Z"))

(def request
  {:identity   "tag:t3-alice"
   :node       "t3-alice"
   :groups     #{"agents"}
   :modes      #{"inject"}
   :credential "GITHUB_TOKEN"
   :host       "api.github.com"
   :header     "Authorization"
   :mode       "inject"})

(def credential
  {:name        "GITHUB_TOKEN"
   :vault       "vlt1"
   :item        "itm1"
   :field       "fld1"
   :field-label "credential"
   :header      "Authorization"
   :hosts       ["api.github.com" "*.githubusercontent.com"]
   :revision    "rev1"})

(def allow
  {:decision "allow"
   :approval nil
   :secret   {:vault "vlt1" :item "itm1" :field "fld1"}
   :inject   {:header "Authorization"}})

(defn- assignment [subject approval]
  {:subject subject :credential "GITHUB_TOKEN" :mode "inject" :approval approval})

(defn- approval [state & {:as more}]
  (merge {:id 7 :identity "tag:t3-alice" :credential "GITHUB_TOKEN" :mode "inject" :state state} more))

(defn- decide [req assignments open]
  (domain/decide req credential assignments open now))

(deftest parses-decide-requests
  (is (= request
         (domain/parse-request {:op "decide" :identity "tag:t3-alice" :node "t3-alice"
                                :groups ["agents"] :modes ["inject"] :credential "GITHUB_TOKEN"
                                :host "api.github.com" :header "Authorization" :mode "inject"})))
  (testing "malformed requests are nil"
    (is (= [nil nil nil nil nil]
           (map domain/parse-request
                [nil
                 "decide"
                 {:op "decide"}
                 (assoc request :op "decide" :groups ["agents"] :modes "inject")
                 {:op "other" :identity "x" :node "" :groups [] :modes [] :credential "A" :host "h" :mode "inject"}]))))
  (testing "a request over a length limit or with a bad credential name is nil"
    (let [m {:op "decide" :identity "tag:t3-alice" :node "t3-alice" :groups ["agents"] :modes ["inject"]
             :credential "GITHUB_TOKEN" :host "api.github.com" :header "Authorization" :mode "inject"}
          long-name (apply str (repeat 65 "A"))]
      (is (= [true false false false false false false false false false false]
             (map (comp some? domain/parse-request)
                  [(assoc m :credential (subs long-name 1))
                   (assoc m :credential long-name)
                   (assoc m :credential "github_token")
                   (assoc m :identity (apply str (repeat 1025 "x")))
                   (assoc m :node (apply str (repeat 257 "x")))
                   (assoc m :host (apply str (repeat 255 "x")))
                   (assoc m :groups (mapv str (range 65)))
                   (assoc m :groups [(apply str (repeat 129 "g"))])
                   (dissoc m :header)
                   (assoc m :header "Bad Header")
                   (assoc m :header (apply str (repeat 129 "X")))]))))))

(deftest denies-in-contract-order
  (is (= {:decision {:decision "deny" :reason "mode"} :writes []}
         (decide (assoc request :modes #{}) [(assignment "group:agents" "auto")] nil)))
  (is (= {:decision {:decision "deny" :reason "unknown"} :writes []}
         (domain/decide request nil [] nil now)))
  (is (= {:decision {:decision "deny" :reason "host"} :writes []}
         (decide (assoc request :host "evil.example.com") [(assignment "group:agents" "auto")] nil)))
  (is (= {:decision {:decision "deny" :reason "header"} :writes []}
         (decide (assoc request :header "X-Other") [(assignment "group:agents" "auto")] nil)))
  (is (= {:decision {:decision "deny" :reason "unassigned"} :writes []}
         (decide request [(assignment "group:other" "auto") (assignment "tag:t3-bob" "auto")] nil))))

(deftest a-wrong-header-spends-no-approval
  (is (= {:decision {:decision "deny" :reason "header"} :writes []}
         (decide (assoc request :header "Cookie2") [(assignment "tag:t3-alice" "ask")]
                 (approval :approved :uses-left 1))))
  (is (= {:decision {:decision "deny" :reason "header"} :writes []}
         (decide (assoc request :header "X-Other") [(assignment "tag:t3-alice" "ask")] nil))
      "and opens no pending approval")
  (is (= {:decision allow :writes []}
         (decide (assoc request :header "authorization") [(assignment "group:agents" "auto")] nil))
      "header names match case-insensitively"))

(deftest auto-assignments-allow-without-an-approval
  (is (= {:decision allow :writes []}
         (decide request [(assignment "group:agents" "auto")] nil)))
  (is (= {:decision allow :writes []}
         (decide request [(assignment "tag:t3-alice" "ask") (assignment "group:agents" "auto")]
                 (approval :blocked)))
      "an auto assignment wins over an ask one")
  (is (= {:decision allow :writes []}
         (decide (assoc request :host "RAW.githubusercontent.com.") [(assignment "tag:t3-alice" "auto")] nil))
      "host patterns match case-insensitively without a trailing dot"))

(deftest ask-assignments-follow-the-approval
  (let [ask [(assignment "tag:t3-alice" "ask")]]
    (testing "no approval opens a pending one"
      (is (= {:decision {:decision "pending"}
              :writes   [{:op       :approval/create
                          :approval {:identity "tag:t3-alice" :credential "GITHUB_TOKEN" :mode "inject"
                                     :revision "rev1" :host "api.github.com"}}]}
             (decide request ask nil))))
    (testing "a pending approval is reused"
      (is (= {:decision {:decision "pending" :approval "7"} :writes []}
             (decide request ask (approval :pending)))))
    (testing "a blocked approval denies"
      (is (= {:decision {:decision "deny" :reason "blocked"} :writes []}
             (decide request ask (approval :blocked)))))
    (testing "a live approval allows and counts a use"
      (is (= {:decision (assoc allow :approval "7") :writes []}
             (decide request ask (approval :approved :uses-left nil)))
          "an approval without a use limit does not change")
      (is (= {:decision (assoc allow :approval "7")
              :writes   [{:op :approval/save :approval (approval :expired :uses-left 0)}]}
             (decide request ask (approval :approved :uses-left 1)))
          "the last once use expires it"))
    (testing "an approval past its expiry expires and a new one is requested"
      (is (= {:decision {:decision "pending"}
              :writes   [{:op :approval/save :approval (approval :expired :expires-at now)}
                         {:op       :approval/create
                          :approval {:identity "tag:t3-alice" :credential "GITHUB_TOKEN" :mode "inject"
                                     :revision "rev1" :host "api.github.com"}}]}
             (decide request ask (approval :approved :expires-at now)))))))

(deftest transition-table
  (let [opts {:actor "casey" :now now}]
    (is (= (approval :approved :decided-at now :approver "casey"
                     :uses-left nil :expires-at (.plus now (Duration/ofHours 1)))
           (domain/transition (approval :pending) :approve (assoc opts :grant "1h"))))
    (is (= (approval :approved :decided-at now :approver "casey" :uses-left 1 :expires-at nil)
           (domain/transition (approval :pending) :approve (assoc opts :grant "once"))))
    (is (= (approval :approved :decided-at now :approver "casey" :uses-left nil :expires-at nil)
           (domain/transition (approval :pending) :approve (assoc opts :grant "forever"))))
    (is (= [:denied :blocked :revoked :expired :closed :superseded :superseded]
           (map (fn [[from event]] (:state (domain/transition (approval from) event opts)))
                [[:pending :deny] [:pending :block] [:approved :revoke] [:approved :expire] [:blocked :unblock]
                 [:pending :supersede] [:approved :supersede]])))
    (testing "every other state and event pair has no transition"
      (is (= []
             (for [from [:pending :approved :blocked :denied :revoked :expired :closed :superseded]
                   event [:approve :deny :block :revoke :expire :unblock :supersede]
                   :when (not (get-in domain/transitions [from event]))
                   :when (domain/transition (approval from) event (assoc opts :grant "once"))]
               [from event]))))
    (is (nil? (domain/transition (approval :pending) :approve (assoc opts :grant "1y")))
        "an unknown grant is refused")))

(deftest effective-assignments-cover-groups-and-the-identity
  (is (= [(assignment "group:agents" "auto") (assignment "tag:t3-alice" "ask")]
         (domain/effective-assignments "tag:t3-alice" #{"agents"}
                                       [(assignment "group:agents" "auto")
                                        (assignment "tag:t3-alice" "ask")
                                        (assignment "group:admins" "auto")
                                        (assignment "tag:t3-bob" "auto")]))))

(deftest validates-credentials-like-the-egress
  (is (= [] (domain/credential-errors credential)))
  (is (= ["Name must be at most 64 upper-case letters, digits and underscores, and not start with a digit."
          "Pick a vault, an item and a field. None of their IDs may contain /."
          "Host is a reserved header the egress does not inject into."
          "Host patterns are names or *. and a name."]
         (domain/credential-errors (assoc credential :name "1TOKEN" :item "a/b" :header "Host"
                                          :hosts ["*" "api.github.com"]))))
  (is (= ["Header must be an HTTP header name."
          "Give at least one host pattern."]
         (domain/credential-errors (assoc credential :header "Bad Header" :hosts []))))
  (is (= ["Name must be at most 64 upper-case letters, digits and underscores, and not start with a digit."]
         (domain/credential-errors (assoc credential :name (apply str (repeat 65 "A"))))))
  (is (= ["Header must be an HTTP header name."]
         (domain/credential-errors (assoc credential :header (apply str "X-" (repeat 127 "a")))))
      "a header longer than a decide request allows"))

(deftest reserves-the-egress-protected-headers
  (is (= ["connection" "content-length" "cookie" "expect" "forwarded" "host"
          "keep-alive" "proxy-" "te" "trailer" "transfer-encoding" "upgrade" "via"
          "x-forwarded-"]
         domain/reserved-headers)
      "change policy.reservedHeaders in ts-egress with it")
  (is (= [true true true true true false false]
         (map #(boolean (seq (domain/credential-errors (assoc credential :header %))))
              ["Cookie" "Expect" "Proxy-Custom" "x-forwarded-host" "TE" "X-Forwarded" "X-Api-Key"]))))

(deftest host-patterns
  (is (= ["api.github.com" "*.github.com" "_dmarc.example.com" nil nil nil nil]
         (map domain/host-pattern ["API.github.com" "*.GitHub.com" "_dmarc.example.com"
                                   "*" "a..b" "*.*.com" "exa mple.com"])))
  (is (= [true true false false]
         (map #(domain/host-matches? "*.github.com" %)
              ["github.com" "api.github.com" "notgithub.com" "github.com.evil"]))))
