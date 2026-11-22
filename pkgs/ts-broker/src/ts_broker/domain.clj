(ns ts-broker.domain
  "Pure broker rules: request parsing, host patterns, assignment matching,
  the approval state machine, and the decide function.

  Approval states and events are keywords. Times are `java.time.Instant`
  values."
  (:require
   [clojure.string :as str])
  (:import
   [java.time Duration Instant]))

(def modes
  "Delivery modes that a credential can be assigned for."
  #{"inject"})

(def approval-policies
  "Approval policies of an assignment."
  #{"auto" "ask"})

(defn credential-name?
  "Returns true when `s` is a credential name: the egress's pattern
  `[A-Z_][A-Z0-9_]*`, at most 64 characters."
  [s]
  (boolean (and (string? s) (<= (count s) 64) (re-matches #"[A-Z_][A-Z0-9_]*" s))))

(def ^:private header-name-pattern #"[!#$%&'*+\-.^_`|~0-9A-Za-z]+")

(def reserved-headers
  "Lower-case names of the headers the egress refuses a placeholder in. A
  name that ends in `-` is a prefix."
  ["connection" "content-length" "cookie" "expect" "forwarded" "host"
   "keep-alive" "proxy-" "te" "trailer" "transfer-encoding" "upgrade" "via"
   "x-forwarded-"])

(defn reserved-header?
  "Returns true when `header` is in [[reserved-headers]]."
  [header]
  (let [h (str/lower-case header)]
    (boolean (some #(if (str/ends-with? % "-") (str/starts-with? h %) (= h %))
                   reserved-headers))))

(defn- name-labels? [s label-char]
  (and (not (str/blank? s))
       (every? (fn [label]
                 (let [label (cond-> label (str/starts-with? label "_") (subs 1))]
                   (and (seq label) (every? label-char label))))
               (str/split s #"\." -1))))

(defn- ldh? [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \0) (int c) (int \9))
      (= c \-)))

(defn normalize-host
  "Returns `host` in lower case without one trailing dot."
  [host]
  (let [h (str/lower-case host)]
    (cond-> h (str/ends-with? h ".") (subs 0 (dec (count h))))))

(defn host-pattern
  "Returns `s` as a lower-case host pattern, or nil when it is not one.

  A pattern is an exact name, or `*.` and a name. Labels hold letters,
  digits and hyphens, each with an optional leading underscore."
  [s]
  (let [p (str/lower-case (str/trim s))
        n (cond-> p (str/starts-with? p "*.") (subs 2))]
    (when (name-labels? n ldh?)
      p)))

(defn host-matches?
  "Returns true when `host` matches `pattern`. `*.example.com` matches
  `example.com` and every name below it."
  [pattern host]
  (let [host (normalize-host host)]
    (if (str/starts-with? pattern "*.")
      (let [suffix (subs pattern 2)]
        (or (= host suffix) (str/ends-with? host (str "." suffix))))
      (= host pattern))))

(defn header-name?
  "Returns true when `s` is an HTTP header name of at most 128 characters,
  the longest a decide request carries."
  [s]
  (boolean (and (string? s) (<= (count s) 128) (re-matches header-name-pattern s))))

(def ^:private request-limits
  {:identity 1024 :node 256 :host 254 :header 128 :mode 16 :group 128 :groups 64 :modes 8})

(defn- bounded? [s limit]
  (and (string? s) (<= (count s) limit)))

(defn- bounded-strings? [xs n limit]
  (and (sequential? xs)
       (<= (count xs) n)
       (every? #(and (bounded? % limit) (not (str/blank? %))) xs)))

(defn parse-request
  "Returns the decide request in `m` with `:groups` and `:modes` as sets, or
  nil when `m` is malformed. `m` is the JSON object with keyword keys.
  Strings and lists have length limits, the credential name must be
  a [[credential-name?]] and the header a [[header-name?]]."
  [m]
  (when (and (map? m)
             (= "decide" (:op m))
             (every? (fn [k] (and (bounded? (get m k) (request-limits k)) (not (str/blank? (get m k)))))
                     [:identity :host :header :mode])
             (credential-name? (:credential m))
             (header-name? (:header m))
             (bounded? (:node m) (:node request-limits))
             (bounded-strings? (:groups m) (:groups request-limits) (:group request-limits))
             (bounded-strings? (:modes m) (:modes request-limits) (:mode request-limits)))
    (-> (select-keys m [:identity :node :credential :host :header :mode])
        (update :host normalize-host)
        (assoc :groups (set (:groups m))
               :modes (set (:modes m))))))

(defn group-subject
  "Returns the assignment subject of group `g`."
  [g]
  (str "group:" g))

(defn group-name
  "Returns the group name of `subject`, or nil when it names an identity."
  [subject]
  (when (str/starts-with? subject "group:")
    (subs subject 6)))

(defn subjects
  "Returns the assignment subjects that `identity` with `groups` matches."
  [identity groups]
  (into #{identity} (map group-subject) groups))

(defn effective-assignments
  "Returns the assignments in `assignments` that apply to `identity` with
  `groups`."
  [identity groups assignments]
  (let [subjects (subjects identity groups)]
    (filterv #(contains? subjects (:subject %)) assignments)))

(def transitions
  "The approval state machine as `{state {event next-state}}`."
  {:pending  {:approve   :approved
              :deny      :denied
              :block     :blocked
              :supersede :superseded}
   :approved {:revoke    :revoked
              :expire    :expired
              :supersede :superseded}
   :blocked  {:unblock :closed}})

(def open-states
  "States of an open approval. One open approval exists at most per
  identity, credential and mode."
  #{:pending :approved :blocked})

(def grants
  "Approval grants as the UI offers them."
  {"once"    {:uses-left 1}
   "1h"      {:ttl (Duration/ofHours 1)}
   "24h"     {:ttl (Duration/ofHours 24)}
   "forever" {}})

(def human-events
  "Events that record their actor as the approver."
  #{:approve :deny :block})

(defn transition
  "Returns `approval` after `event`, or nil when the state machine has no
  such transition. An `:approve` event needs a `:grant` from [[grants]].

  Options:

  | key      | description |
  |----------|-------------|
  | `:grant` | Grant name for `:approve` |
  | `:actor` | Login of the human who caused the event |
  | `:now`   | Current instant |"
  [approval event {:keys [grant actor ^Instant now]}]
  (let [to (get-in transitions [(:state approval) event])
        terms (get grants grant)]
    (when (and to (or (not= event :approve) terms))
      (cond-> (assoc approval :state to)
        (contains? human-events event)
        (assoc :decided-at now :approver actor)

        (= event :approve)
        (assoc :uses-left (:uses-left terms)
               :expires-at (some->> (:ttl terms) (.plus now)))))))

(defn live?
  "Returns true when `approval` is approved and neither expired nor used up
  at `now`."
  [approval ^Instant now]
  (and (= :approved (:state approval))
       (or (nil? (:expires-at approval)) (.isBefore now (:expires-at approval)))
       (or (nil? (:uses-left approval)) (pos? (:uses-left approval)))))

(defn- use-approval [approval now]
  (let [used (update approval :uses-left #(some-> % dec))]
    (if (live? used now)
      used
      (transition used :expire {:now now}))))

(defn- allow [credential approval-id]
  {:decision "allow"
   :approval (some-> approval-id str)
   :secret   (select-keys credential [:vault :item :field])
   :inject   (select-keys credential [:header])})

(defn- deny [reason]
  {:decision {:decision "deny" :reason reason}
   :writes   []})

(defn- open-pending [request credential]
  {:decision {:decision "pending"}
   :writes   [{:op       :approval/create
               :approval {:identity   (:identity request)
                          :credential (:credential request)
                          :mode       (:mode request)
                          :revision   (:revision credential)
                          :host       (:host request)}}]})

(defn- decide-ask [request credential approval now]
  (case (:state approval)
    nil       (open-pending request credential)
    :pending  {:decision {:decision "pending" :approval (str (:id approval))}
               :writes   []}
    :blocked  (deny "blocked")
    :approved (if (live? approval now)
                (let [used (use-approval approval now)]
                  {:decision (allow credential (:id approval))
                   :writes   (if (= used approval) [] [{:op :approval/save :approval used}])})
                (update (open-pending request credential) :writes
                        #(into [{:op       :approval/save
                                 :approval (transition approval :expire {:now now})}]
                               %)))))

(defn decide
  "Decides a parsed decide `request` and returns `{:decision d :writes ws}`.

  `credential` is the credential that the request names, or nil.
  `assignments` are the assignments of that credential. `approval` is the
  open approval of the request's identity, credential and mode, or nil.

  `d` is the response object without the id of a new approval. Each write
  is a map with an `:op` of `:approval/create` or `:approval/save`. A
  `pending` decision without `:approval` waits for the id that
  `:approval/create` makes."
  [request credential assignments approval now]
  (let [matching (->> (effective-assignments (:identity request) (:groups request) assignments)
                      (filter #(= (:mode request) (:mode %))))]
    (cond
      (not (contains? (:modes request) (:mode request))) (deny "mode")
      (nil? credential) (deny "unknown")
      (not-any? #(host-matches? % (:host request)) (:hosts credential)) (deny "host")
      (not= (str/lower-case (:header request)) (str/lower-case (:header credential))) (deny "header")
      (empty? matching) (deny "unassigned")
      (some #(= "auto" (:approval %)) matching) {:decision (allow credential nil)
                                                 :writes   []}
      :else (decide-ask request credential approval now))))

(defn credential-errors
  "Returns a vector of messages that describe what is wrong with
  `credential`, empty when it is valid."
  [{:keys [name vault item field vault-title item-title field-label header hosts]}]
  (cond-> []
    (not (credential-name? name))
    (conj "Name must be at most 64 upper-case letters, digits and underscores, and not start with a digit.")

    (or (some #(or (str/blank? %) (str/includes? % "/")) [vault item field])
        (some str/blank? [vault-title item-title field-label]))
    (conj "Pick a vault, an item and a field. None of their IDs may contain /.")

    (not (header-name? header))
    (conj "Header must be an HTTP header name.")

    (and (string? header) (reserved-header? header))
    (conj (str header " is a reserved header the egress does not inject into."))

    (empty? hosts)
    (conj "Give at least one host pattern.")

    (some nil? (map host-pattern hosts))
    (conj "Host patterns are names or *. and a name.")))

(defn split-list
  "Returns the entries of the whitespace- or comma-separated `s`."
  [s]
  (vec (remove str/blank? (str/split (str s) #"[\s,]+"))))

(defn lines
  "Returns the trimmed, non-blank lines of `s`."
  [s]
  (into [] (comp (map str/trim) (remove str/blank?)) (str/split-lines (str s))))

(defn subject-errors
  "Returns messages for each invalid assignment subject in `subjects`."
  [subjects]
  (for [s subjects
        :when (or (str/blank? s)
                  (re-find #"\s" s)
                  (= "group:" s))]
    (str "Invalid subject: " (pr-str s))))

(def placeholder-prefix
  "The prefix that ts-egress puts before a credential name to make its
  placeholder."
  "TS_EGRESS_PLACEHOLDER_")

(def ^:private base32-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defn generated-name
  "Returns a credential name made from `bytes`: `BRK_` and the first 40
  bits of `bytes` in base32. It tells nothing about the 1Password item."
  [bytes]
  (let [n (reduce (fn [n b] (+ (* n 256) (bit-and b 0xff))) 0 (take 5 bytes))]
    (apply str "BRK_" (for [shift (range 35 -1 -5)]
                        (nth base32-alphabet (bit-and (bit-shift-right n shift) 31))))))

(defn env-name
  "Returns an environment variable name for `credential` derived from its
  item title, or its name when the title has no ASCII letter or digit."
  [{:keys [name item-title]}]
  (-> (str/upper-case (str item-title))
      (str/replace #"[^A-Z0-9]+" "_")
      (str/replace #"^_+|_+$" "")
      (str/replace #"^(?=[0-9])" "_")
      not-empty
      (or name)))

(defn title
  "Returns the title of `credential`: its vault, item and field titles."
  [{:keys [vault-title item-title field-label]}]
  (str/join " / " [vault-title item-title field-label]))
