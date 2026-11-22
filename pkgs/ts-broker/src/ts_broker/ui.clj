(ns ts-broker.ui
  "Pages and actions of the broker UI. See [[ts-broker.http]] for how they
  are served.

  Pages read the store through reader connections and the catalog through
  [[ts-broker.connect]]. Actions turn the parameters of the request body
  into store commands; the store validates them. Controls send them as a
  form or a Datastar `payload`, never in the URL. A successful credential
  edit answers with a fresh edit form."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [ts-broker.connect :as connect]
   [ts-broker.domain :as domain]
   [ts-broker.store :as store])
  (:import
   [java.net URLEncoder]
   [java.time Duration Instant ZoneOffset]
   [java.time.format DateTimeFormatter]))

(defn- param [request k]
  (some-> (get-in request [:params k]) first str/trim))

(defn- params [request k]
  (vec (remove str/blank? (map str/trim (get-in request [:params k])))))

(defn- href [path query]
  (let [query (remove (comp nil? val) query)]
    (str path
         (when (seq query)
           (str "?" (str/join "&" (for [[k v] query]
                                    (str (name k) "=" (URLEncoder/encode (str v) "UTF-8")))))))))

(defn- post
  "Returns a Datastar expression that POSTs `payload` as JSON to `path`."
  [path payload]
  (str "@post('" path "', {payload: " (json/generate-string payload) "})"))

(defn- confirm-post [message path payload]
  (str "confirm(" (json/generate-string message) ") && " (post path payload)))

(def ^:private time-format
  (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss 'UTC'") ZoneOffset/UTC))

(defn- fmt-time [^Instant t]
  (when t (.format time-format t)))

(defn- remaining [^Instant now ^Instant until]
  (let [d (Duration/between now until)
        m (.toMinutes d)]
    (cond
      (neg? (.toSeconds d)) "expired"
      (< m 60) (str m " min left")
      :else (str (.toHours d) " h left"))))

(def ^:private nav-items
  [["/" "Approvals"]
   ["/credentials" "Credentials"]
   ["/catalog" "Catalog"]
   ["/assign" "Assign"]
   ["/identities" "Identities"]
   ["/audit" "Audit"]])

(defn- layout [request title & body]
  [:main#morph
   [:header
    [:nav (for [[path label] nav-items]
            [:a {:href path :aria-current (when (= path (:uri request)) "page")} label])]
    [:span.muted "Signed in as " (:actor request)]]
   [:h1 title]
   body])

(defn- identity-link [identity]
  [:a {:href (href "/identity" {:id identity})} [:code identity]])

(defn- subject-link [subject]
  (if (domain/group-name subject)
    [:code subject]
    (identity-link subject)))

(defn- credential-link [name]
  [:a {:href (href "/credential" {:name name})} [:code name]])

(defn- empty-row [n text]
  [:tr [:td.muted {:colspan n} text]])

(defn- approval-terms [now {:keys [expires-at uses-left]}]
  (cond
    uses-left (str "once (" uses-left " use left)")
    expires-at (str "until " (fmt-time expires-at) ", " (remaining now expires-at))
    :else "until revoked"))

(defn- approval-actions [{:keys [id state]}]
  (let [act (fn [label event grant & [class]]
              [:button {:class class :data-on:click (post "/action/approval" {:id id :event event :grant grant})}
               label])]
    (case state
      :pending  [:div.actions
                 (act "Once" "approve" "once")
                 (act "1 h" "approve" "1h")
                 (act "24 h" "approve" "24h")
                 (act "Until revoked" "approve" "forever")
                 (act "Deny" "deny" nil "warn")
                 (act "Deny and block" "block" nil "danger")]
      :approved [:div.actions (act "Revoke" "revoke" nil "warn")]
      :blocked  [:div.actions (act "Unblock" "unblock" nil)])))

(defn- approvals-table [now rows columns]
  [:table
   [:thead [:tr (for [[label] columns] [:th label])]]
   [:tbody
    (if (seq rows)
      (for [a rows]
        [:tr (for [[_ f] columns] [:td (f now a)])])
      (empty-row (count columns) "None."))]])

(def ^:private approval-columns
  {:identity   ["Identity" (fn [_ a] (identity-link (:identity a)))]
   :credential ["Credential" (fn [_ a] (credential-link (:credential a)))]
   :host       ["Requested host" (fn [_ a] [:code (:host a)])]
   :current    ["Header and hosts now" (fn [_ {{:keys [header hosts]} :current}]
                                         (list [:code header] " " (for [h hosts] [:code.pill h])))]
   :mode       ["Mode" (fn [_ a] (:mode a))]
   :requested  ["Requested" (fn [_ a] (fmt-time (:requested-at a)))]
   :terms      ["Terms" approval-terms]
   :approver   ["By" (fn [_ a] (:approver a))]
   :decided    ["Decided" (fn [_ a] (fmt-time (:decided-at a)))]
   :actions    ["" (fn [_ a] (approval-actions a))]})

(defn- columns [& ks]
  (mapv approval-columns ks))

(defn approvals-page
  "Renders the approval queue, standing approvals and blocked list. Queued
  and standing approvals show the host they were requested for and the
  credential's current header and hosts."
  [{:keys [store clock]} request]
  (let [now (clock)
        [open creds] (store/read store (juxt store/open-approvals store/credentials))
        by-name (into {} (map (juxt :name identity)) creds)
        by-state (group-by :state (map #(assoc % :current (by-name (:credential %))) open))
        standing (filter #(domain/live? % now) (:approved by-state))]
    (layout request "Approvals"
            [:section
             [:h2 "Waiting for a decision"]
             (approvals-table now (reverse (:pending by-state))
                              (columns :identity :credential :host :current :mode :requested :actions))]
            [:section
             [:h2 "Standing approvals"]
             (approvals-table now standing
                              (columns :identity :credential :host :current :mode :terms :approver :actions))]
            [:section
             [:h2 "Blocked"]
             (approvals-table now (:blocked by-state)
                              (columns :identity :credential :mode :approver :decided :actions))])))

(defn credentials-page
  "Renders the credential list."
  [{:keys [store]} request]
  (let [[creds assignments] (store/read store (juxt store/credentials store/assignments))
        counts (frequencies (map :credential assignments))]
    (layout request "Credentials"
            [:p [:a {:href "/catalog"} "Add a credential from the catalog"]]
            [:table
             [:thead [:tr [:th "Name"] [:th "Hosts"] [:th "Header"] [:th "1Password field"] [:th "Assignments"]]]
             [:tbody
              (if (seq creds)
                (for [{:keys [name hosts header field-label]} creds]
                  [:tr
                   [:td (credential-link name)]
                   [:td (for [h hosts] [:code.pill h])]
                   [:td [:code header]]
                   [:td [:code field-label]]
                   [:td (get counts name 0)]])
                (empty-row 5 "No credentials yet."))]])))

(defn- field-picker [{:keys [fields]}]
  [:fieldset
   [:legend "Field"]
   (for [{:keys [id label section type]} fields]
     [:label.option
      [:input {:type "radio" :name "field" :value id :required true}]
      " " [:code (if (str/blank? label) id label)]
      [:small.muted " " (str/join " · " (remove nil? [section (some-> type str/lower-case)]))]])])

(defn- credential-form-fields [{:keys [header hosts]}]
  (list
   [:label "Header"
    [:small.muted " the only header the placeholder may appear in. The tenant writes the value, such as Bearer TS_EGRESS_PLACEHOLDER_NAME."]
    [:input {:name "header" :value (or header "Authorization") :required true}]]
   [:label "Hosts" [:small.muted " names or *.domain, one per line"]
    [:textarea {:name "hosts" :rows 3 :required true} (str/join "\n" hosts)]]))

(defn- create-form [vault item]
  [:form.stack {:data-on:submit__prevent "@post('/action/credential/create', {contentType: 'form'})"}
   [:input {:type "hidden" :name "vault" :value (:id vault)}]
   [:input {:type "hidden" :name "item" :value (:id item)}]
   (field-picker item)
   [:label "Name" [:small.muted " the placeholder suffix: A-Z, 0-9 and _, not starting with a digit"]
    [:input {:name "name" :value (domain/suggested-name (:title item)) :required true
             :pattern "[A-Z_][A-Z0-9_]*"}]]
   (credential-form-fields nil)
   [:button {:type "submit"} "Create credential"]])

(defn catalog-page
  "Renders the 1Password catalog: vaults, then the items of the vault in the
  `vault` parameter, then the fields of the item in `item`."
  [{:keys [connect]} request]
  (let [vault-id (param request :vault)
        item-id (param request :item)]
    (layout request "Catalog"
            (if-not connect
              [:p.message "No 1Password Connect URL is configured, so the catalog is empty."]
              (try
              (let [vaults (connect/vaults connect)
                    vault (first (filter #(= vault-id (:id %)) vaults))]
                [:div.columns
                 [:nav.list
                  [:h2 "Vaults"]
                  (for [v vaults]
                    [:a {:href (href "/catalog" {:vault (:id v)})
                         :aria-current (when (= vault-id (:id v)) "page")}
                     (:name v)])]
                 (when vault
                   [:nav.list
                    [:h2 "Items"]
                    (for [i (connect/items connect (:id vault))]
                      [:a {:href (href "/catalog" {:vault (:id vault) :item (:id i)})
                           :aria-current (when (= item-id (:id i)) "page")}
                       (:title i) [:small.muted " " (some-> (:category i) str/lower-case)]])])
                 (when (and vault item-id)
                   (let [item (connect/item connect (:id vault) item-id)]
                     [:section
                      [:h2 (:title item)]
                      (create-form vault item)]))])
                (catch Exception e
                  [:p.message (or (connect/error-message e) (throw e))]))))))

(defn- assignment-row [{:keys [subject credential mode approval editor]} show]
  [:tr
   (when (show :subject) [:td (subject-link subject)])
   (when (show :credential) [:td (credential-link credential)])
   [:td mode]
   [:td [:span {:class (str "pill " approval)} approval]]
   [:td editor]
   [:td [:button.warn {:data-on:click (post "/action/assignment/remove"
                                            {:subject subject :credential credential :mode mode})}
         "Remove"]]])

(defn- assignments-table [rows show]
  [:table
   [:thead [:tr
            (when (show :subject) [:th "Subject"])
            (when (show :credential) [:th "Credential"])
            [:th "Mode"] [:th "Approval"] [:th "By"] [:th ""]]]
   [:tbody
    (if (seq rows)
      (for [a rows] (assignment-row a show))
      (empty-row 6 "No assignments."))]])

(defn- edit-form
  "Returns the edit form of credential `c`. Live re-renders skip the form,
  so it keeps the draft and the revision it was rendered with until a save
  replaces it or the page loads again."
  [{:keys [name revision] :as c}]
  [:form#credential-form.stack
   {:data-ignore-morph       true
    :data-on:submit__prevent "@post('/action/credential/update', {contentType: 'form'})"}
   [:input {:type "hidden" :name "name" :value name}]
   [:input {:type "hidden" :name "revision" :value revision}]
   (credential-form-fields c)
   [:div.actions
    [:button {:type "submit"} "Save"]
    [:a {:href (href "/credential" {:name name})} "Discard and reload"]
    [:button.danger {:type "button"
                     :data-on:click (confirm-post (str "Delete " name " and its assignments?")
                                                  "/action/credential/delete" {:name name :revision revision})}
     "Delete"]]])

(defn credential-page
  "Renders one credential with its edit form and who can use it."
  [{:keys [store clock]} request]
  (let [name (param request :name)
        [c assignments idents open]
        (store/read store (juxt #(store/credential % name)
                                #(store/credential-assignments % name)
                                store/identities
                                store/open-approvals))
        now (clock)]
    (if-not c
      (layout request "Credential" [:p "No such credential. " [:a {:href "/credentials"} "All credentials"]])
      (layout request [:code name]
              [:p.muted "1Password vault " [:code (:vault c)] ", item " [:code (:item c)]
               ", field " [:code (:field-label c)] " (" [:code (:field c)] "). Edited by " (:editor c) " at " (fmt-time (:updated c)) "."]
              [:section
               [:h2 "Who can use it"]
               (assignments-table assignments #{:subject})
               [:table
                [:thead [:tr [:th "Seen identity"] [:th "Through"] [:th "Approval"]]]
                [:tbody
                 (let [rows (for [{:keys [identity groups]} idents
                                  :let [via (domain/effective-assignments identity groups assignments)]
                                  :when (seq via)]
                              [identity via])]
                   (if (seq rows)
                     (for [[identity via] rows
                           :let [a (first (filter #(= [identity name] [(:identity %) (:credential %)]) open))]]
                       [:tr
                        [:td (identity-link identity)]
                        [:td (for [v via] [:code.pill (:subject v) " " (:approval v)])]
                        [:td (cond
                               (nil? a) [:span.muted "none"]
                               (domain/live? a now) (approval-terms now a)
                               :else (clojure.core/name (:state a)))]])
                     (empty-row 3 "No identity that sent a request matches.")))]]]
              [:section
               [:h2 "Edit"]
               (edit-form c)]))))

(defn- known-subjects [assignments idents]
  (let [groups (into (set (keep (comp domain/group-name :subject) assignments))
                     (mapcat :groups idents))
        identities (into (set (map :identity idents))
                         (remove domain/group-name (map :subject assignments)))]
    (concat (map domain/group-subject (sort groups)) (sort identities))))

(defn assign-page
  "Renders the bulk assignment form and all assignments."
  [{:keys [store]} request]
  (let [[creds assignments idents] (store/read store (juxt store/credentials store/assignments store/identities))]
    (layout request "Assign"
            [:form.stack {:data-on:submit__prevent "@post('/action/assign', {contentType: 'form'})"}
             [:div.columns
              [:fieldset
               [:legend "Credentials"]
               (if (seq creds)
                 (for [{:keys [name]} creds]
                   [:label.option [:input {:type "checkbox" :name "credential" :value name}] " " [:code name]])
                 [:p.muted "No credentials yet."])]
              [:fieldset
               [:legend "Subjects"]
               (for [s (known-subjects assignments idents)]
                 [:label.option [:input {:type "checkbox" :name "subject" :value s}] " " [:code s]])
               [:label "Other subjects" [:small.muted " group:name or an identity such as tag:t3-alice, one per line"]
                [:textarea {:name "subjects" :rows 2}]]]
              [:fieldset
               [:legend "Grant"]
               [:label "Mode" [:select {:name "mode"} (for [m (sort domain/modes)] [:option {:value m} m])]]
               [:label "Approval"
                [:select {:name "approval"}
                 [:option {:value "ask"} "ask each identity"]
                 [:option {:value "auto"} "auto"]]]
               [:button {:type "submit"} "Assign"]]]]
            [:section
             [:h2 "All assignments"]
             (assignments-table assignments #{:subject :credential})])))

(defn identities-page
  "Renders the identities that sent requests."
  [{:keys [store]} request]
  (let [idents (store/read store store/identities)]
    (layout request "Identities"
            [:form {:method "get" :action "/identity" :class "actions"}
             [:input {:name "id" :placeholder "tag:t3-alice" :required true}]
             [:button {:type "submit"} "Look up"]]
            [:table
             [:thead [:tr [:th "Identity"] [:th "Node"] [:th "Groups"] [:th "Modes"] [:th "Last request"]]]
             [:tbody
              (if (seq idents)
                (for [{:keys [identity node groups modes seen]} idents]
                  [:tr
                   [:td (identity-link identity)]
                   [:td node]
                   [:td (for [g groups] [:code.pill g])]
                   [:td (str/join ", " modes)]
                   [:td (fmt-time seen)]])
                (empty-row 5 "No requests yet."))]])))

(defn identity-page
  "Renders what one identity can use: its groups from its latest request,
  its own assignments, and its groups' assignments."
  [{:keys [store clock]} request]
  (let [id (param request :id)
        [idents assignments open] (store/read store (juxt store/identities store/assignments store/open-approvals))
        seen (first (filter #(= id (:identity %)) idents))
        groups (:groups seen)
        ceiling (set (:modes seen))
        effective (domain/effective-assignments id groups assignments)
        now (clock)]
    (layout request [:code id]
            (if seen
              [:p.muted "Latest request from node " [:code (:node seen)] " at " (fmt-time (:seen seen))
               ". Groups and modes come from that request's policy ceiling."]
              [:p.muted "This identity has sent no request, so its groups are unknown. Only its own assignments show."])
            [:p "Groups: " (if (seq groups) (for [g groups] [:code.pill g]) [:span.muted "none"])
             " Modes: " (if (seq ceiling) (str/join ", " (sort ceiling)) [:span.muted "none"])]
            [:section
             [:h2 "Can use"]
             [:table
              [:thead [:tr [:th "Credential"] [:th "Through"] [:th "Mode"] [:th "Approval"] [:th "Status"]]]
              [:tbody
               (if (seq effective)
                 (for [[[credential mode] via] (sort (group-by (juxt :credential :mode) effective))
                       :let [approval (if (some #(= "auto" (:approval %)) via) "auto" "ask")
                             a (first (filter #(= [id credential mode] [(:identity %) (:credential %) (:mode %)]) open))]]
                   [:tr
                    [:td (credential-link credential)]
                    [:td (for [v via] [:code.pill (:subject v)])]
                    [:td mode]
                    [:td [:span {:class (str "pill " approval)} approval]]
                    [:td (cond
                           (and seen (not (ceiling mode))) [:span.warn-text "outside the policy ceiling"]
                           (= "auto" approval) "allowed"
                           (nil? a) [:span.muted "asks on first use"]
                           (domain/live? a now) (approval-terms now a)
                           :else (name (:state a)))]])
                 (empty-row 5 "Nothing assigned."))]]]
            [:p [:a {:href (href "/audit" {:identity id})} "Audit log for this identity"]])))

(def ^:private audit-kinds ["request" "decision" "use" "approval" "assignment" "credential"])

(defn audit-page
  "Renders the newest audit rows, filtered by the `identity`, `credential`
  and `kind` parameters."
  [{:keys [store]} request]
  (let [filters (into {} (keep (fn [k] (some->> (param request k) not-empty (vector k))))
                      [:identity :credential :kind])
        entries (store/read store #(store/audit % filters 300))]
    (layout request "Audit"
            [:p.actions
             [:a {:href "/audit" :aria-current (when (empty? filters) "page")} "all"]
             (for [k audit-kinds]
               [:a {:href (href "/audit" (assoc filters :kind k))
                    :aria-current (when (= k (:kind filters)) "page")} k])
             (for [[k v] (dissoc filters :kind)]
               [:span.pill (name k) " " v " " [:a {:href (href "/audit" (dissoc filters k))} "x"]])]
            [:table.audit
             [:thead [:tr [:th "#"] [:th "At"] [:th "Kind"] [:th "Identity"] [:th "Actor"] [:th "Credential"] [:th "Detail"]]]
             [:tbody
              (if (seq entries)
                (for [{:keys [id at kind identity actor credential detail]} entries]
                  [:tr
                   [:td id]
                   [:td (fmt-time at)]
                   [:td kind]
                   [:td (when identity [:a {:href (href "/audit" (assoc filters :identity identity))} [:code identity]])]
                   [:td actor]
                   [:td (when credential [:a {:href (href "/audit" (assoc filters :credential credential))} [:code credential]])]
                   [:td [:code.detail (json/generate-string detail)]]])
                (empty-row 7 "No audit rows."))]])))

(def pages
  "UI pages by path."
  {"/"            {:title "Broker approvals" :render approvals-page}
   "/credentials" {:title "Broker credentials" :render credentials-page}
   "/credential"  {:title "Broker credential" :render credential-page}
   "/catalog"     {:title "Broker catalog" :render catalog-page}
   "/assign"      {:title "Broker assignments" :render assign-page}
   "/identities"  {:title "Broker identities" :render identities-page}
   "/identity"    {:title "Broker identity" :render identity-page}
   "/audit"       {:title "Broker audit" :render audit-page}})

(defn- credential-params [request]
  {:name   (param request :name)
   :vault  (param request :vault)
   :item   (param request :item)
   :field  (param request :field)
   :header (param request :header)
   :hosts  (domain/split-list (param request :hosts))})

(def ^:private approval-events
  {"approve" :approve "deny" :deny "block" :block "revoke" :revoke "unblock" :unblock})

(def actions
  "UI actions by path."
  {"/action/approval"
   (fn [{:keys [store]} request]
     (store/write! store {:command :approval/event
                          :id      (parse-long (str (param request :id)))
                          :event   (approval-events (param request :event))
                          :grant   (param request :grant)
                          :actor   (:actor request)}))

   "/action/credential/create"
   (fn [{:keys [connect store]} request]
     (let [{:keys [vault item field] :as c} (credential-params request)
           picked (when (and connect vault item field)
                    (->> (:fields (connect/item connect vault item))
                         (filter #(= field (:id %)))
                         first))
           label (when picked
                   (if (str/blank? (:label picked)) field (:label picked)))]
       (store/write! store {:command    :credential/create
                            :credential (assoc c :field-label label)
                            :actor      (:actor request)})))

   "/action/credential/update"
   (fn [{:keys [store]} request]
     (let [name (store/write! store {:command  :credential/update
                                     :name     (param request :name)
                                     :revision (param request :revision)
                                     :header   (param request :header)
                                     :hosts    (domain/split-list (param request :hosts))
                                     :actor    (:actor request)})]
       {:replace (edit-form (store/read store #(store/credential % name)))}))

   "/action/credential/delete"
   (fn [{:keys [store]} request]
     (store/write! store {:command  :credential/delete
                          :name     (param request :name)
                          :revision (param request :revision)
                          :actor    (:actor request)}))

   "/action/assign"
   (fn [{:keys [store]} request]
     (store/write! store {:command     :assignment/assign
                          :credentials (params request :credential)
                          :subjects    (vec (distinct (concat (params request :subject)
                                                              (domain/lines (param request :subjects)))))
                          :mode        (param request :mode)
                          :approval    (param request :approval)
                          :actor       (:actor request)}))

   "/action/assignment/remove"
   (fn [{:keys [store]} request]
     (store/write! store {:command    :assignment/remove
                          :subject    (param request :subject)
                          :credential (param request :credential)
                          :mode       (param request :mode)
                          :actor      (:actor request)}))})

(def css
  "Stylesheet of the UI."
  "body{font:14px/1.45 system-ui,sans-serif;max-width:1280px;margin:1rem auto;padding:0 .8rem;background:#f8fafc;color:#172033}
h1{font-size:1.45rem;margin:.6rem 0 .4rem}h2{font-size:1.05rem;margin:1.2rem 0 .3rem}a{color:#174ca0}
header{display:flex;justify-content:space-between;align-items:center;gap:1rem;border-bottom:1px solid #ccd4df;padding-bottom:.4rem;flex-wrap:wrap}
nav{display:flex;gap:.9rem;flex-wrap:wrap}nav a[aria-current],.list a[aria-current],.actions a[aria-current]{font-weight:600;color:#172033;text-decoration:none}
table{width:100%;border-collapse:collapse;margin:.4rem 0 .8rem}th,td{text-align:left;padding:4px 6px;border-bottom:1px solid #dde3eb;vertical-align:middle}
th{font-size:.78rem;color:#586579;font-weight:600}tbody tr:hover{background:#eef2f7}
button,select,input,textarea{font:inherit}button{cursor:pointer;padding:.2rem .55rem;border:1px solid #9fb0c6;border-radius:4px;background:#fff}
button:hover{background:#e8eef7}button.warn{border-color:#c79a3a}button.danger{border-color:#c0473a;color:#8d2418}
input,textarea,select{padding:.3rem .4rem;border:1px solid #9fb0c6;border-radius:4px;background:#fff}textarea{width:100%;box-sizing:border-box}
fieldset{border:1px solid #ccd4df;border-radius:6px;margin:.6rem 0;padding:.5rem .7rem;min-width:0}legend{font-weight:600;padding:0 .3rem}
.stack{display:flex;flex-direction:column;gap:.5rem;max-width:760px}.stack label{display:flex;flex-direction:column;gap:.15rem}
.stack label.option,label.option{display:block;flex-direction:row;margin:.15rem 0}
.columns{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:1rem;align-items:start}
.columns .stack,.columns form{max-width:none}.list{display:flex;flex-direction:column;gap:.2rem}
.actions{display:flex;flex-wrap:wrap;gap:.35rem;align-items:center}
.message{padding:.5rem .7rem;background:#fff3ce;border:1px solid #e5c76b;border-radius:4px;margin:.5rem 0}.message p{margin:.1rem 0}
.muted,small{color:#586579}.warn-text{color:#8d2418}
.pill{display:inline-block;padding:0 .4rem;margin:0 .25rem .15rem 0;border-radius:9px;background:#e4e9f1;font-size:.85em}
.pill.ask{background:#fdecc8}.pill.auto{background:#d6f0dc}
code{font-family:ui-monospace,monospace;font-size:.92em}.detail{overflow-wrap:anywhere;font-size:.8em;color:#3b4658}
table.audit td{font-size:.86rem}
@media(max-width:800px){table{display:block;overflow-x:auto}}")
