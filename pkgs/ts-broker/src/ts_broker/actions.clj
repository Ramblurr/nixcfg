(ns ts-broker.actions
  "Actions of the broker UI, and the forms whose answers they render.

  Actions turn the parameters of the request body into store commands; the
  store validates them. Controls send them as a form or a Datastar
  `payload`, never in the URL. Refusals of the create and assign forms show
  in the form's [[feedback]] element. A successful credential edit or
  assign answers with a fresh form."
  (:require
   [clojure.string :as str]
   [ts-broker.connect :as connect]
   [ts-broker.domain :as domain]
   [ts-broker.lib :as lib]
   [ts-broker.store :as store])
  (:import
   [java.security SecureRandom]))

(def ^:private approval-events
  {"approve" :approve "deny" :deny "block" :block "revoke" :revoke "unblock" :unblock})

(lib/defaction approval [{:keys [store]} request]
  (store/write! store {:command :approval/event
                       :id      (parse-long (str (lib/param request :id)))
                       :event   (approval-events (lib/param request :event))
                       :grant   (lib/param request :grant)
                       :actor   (:actor request)}))

(defn credential-form-fields
  "Returns the header and hosts inputs of a credential form, filled from
  credential `c`."
  [{:keys [header hosts]}]
  (list
   [:label "Header"
    [:small.muted " the only header the placeholder may appear in. The tenant writes the value, such as Bearer TS_EGRESS_PLACEHOLDER_NAME."]
    [:input {:name "header" :value (or header "Authorization") :required true}]]
   [:label "Hosts" [:small.muted " names or *.domain, one per line"]
    [:textarea {:name "hosts" :rows 3 :required true} (str/join "\n" hosts)]]))

(defn feedback
  "Returns the element with `id` that shows the refusal `messages` next to
  a form, or with `:ok` the outcome of a successful action. Live
  re-renders skip it, so the messages stay until an action replaces it."
  ([id messages]
   (feedback id messages :error))
  ([id messages kind]
   [:div.feedback {:id id :role "alert" :data-ignore-morph true}
    (when (seq messages)
      [:div {:class (str "message " (name kind))} (for [m messages] [:p m])])]))

(defn- feedback-on-invalid
  "Calls `f` and returns its result. When the store refuses the command,
  returns `{:replace e}` with `e` the [[feedback]] element `id` that shows
  why."
  [id f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (if-let [errors (store/invalid-errors e)]
        {:replace (feedback id errors)}
        (throw e)))))

(defn- random-bytes [n]
  (let [b (byte-array n)]
    (.nextBytes (SecureRandom.) b)
    b))

(lib/defaction create-credential
  "Creates a credential from the catalog form with a generated name and
  its 1Password titles, and moves the browser to it. Refusals show in the
  form's `create-feedback` element."
  [{:keys [connect store]} request]
  (feedback-on-invalid
   "create-feedback"
   #(let [ids {:vault (lib/param request :vault)
               :item  (lib/param request :item)
               :field (lib/param request :field)}
          titles (some-> connect (connect/titles [ids]) first)
          name (store/write! store {:command    :credential/create
                                    :credential (merge ids
                                                       titles
                                                       {:name   (domain/generated-name (random-bytes 5))
                                                        :header (lib/param request :header)
                                                        :hosts  (domain/split-list (lib/param request :hosts))})
                                    :actor      (:actor request)})]
      {:redirect (lib/href "/credential" {:name name :created true})})))

(lib/defaction delete-credential [{:keys [store]} request]
  (store/write! store {:command  :credential/delete
                       :name     (lib/param request :name)
                       :revision (lib/param request :revision)
                       :actor    (:actor request)}))

(declare update-credential)

(defn edit-form
  "Returns the edit form of credential `c`. Live re-renders skip the form,
  so it keeps the draft and the revision it was rendered with until a save
  replaces it or the page loads again. The delete confirmation adds the
  text of the live `assignment-warning` element when the page has one."
  [{:keys [name revision] :as c}]
  [:form#credential-form.stack
   {:data-ignore-morph       true
    :data-on:submit__prevent (lib/form-post update-credential)}
   [:input {:type "hidden" :name "name" :value name}]
   [:input {:type "hidden" :name "revision" :value revision}]
   (credential-form-fields c)
   [:div.actions
    [:button {:type "submit"} "Save"]
    [:a {:href (lib/href "/credential" {:name name})} "Discard and reload"]
    [:button.danger {:type "button"
                     :data-confirm  (str "Delete " (domain/title c) "?")
                     :data-on:click (str "confirm(el.dataset.confirm"
                                         " + (document.getElementById('assignment-warning')?.textContent ?? ''))"
                                         " && " (lib/post delete-credential {:name name :revision revision}))}
     "Delete"]]])

(lib/defaction update-credential [{:keys [store]} request]
  (let [name (store/write! store {:command  :credential/update
                                  :name     (lib/param request :name)
                                  :revision (lib/param request :revision)
                                  :header   (lib/param request :header)
                                  :hosts    (domain/split-list (lib/param request :hosts))
                                  :actor    (:actor request)})]
    {:replace (edit-form (store/read store #(store/credential % name)))}))

(defn- known-subjects [assignments idents]
  (let [groups (into (set (keep (comp domain/group-name :subject) assignments))
                     (mapcat :groups idents))
        identities (into (set (map :identity idents))
                         (remove domain/group-name (map :subject assignments)))]
    (concat (map domain/group-subject (sort groups)) (sort identities))))

(declare assign)

(defn- option
  "Returns the labelled checkbox of the assign form for parameter `k` and
  `value`. The box and its label have ids made from `value`, so a morph
  that reorders the boxes keeps each tick on its own box."
  [k value & label]
  (let [id (str "assign-" (name k) "-" (apply str (map #(format "%02x" %) (.getBytes (str value) "UTF-8"))))]
    (into [:label.option {:id (str id "-label")}
           [:input {:id id :type "checkbox" :name (name k) :value value}] " "]
          label)))

(defn assign-form
  "Returns the bulk assignment form with the credentials and subjects in
  reader connection `db`, and `messages` of `kind` in its [[feedback]].
  Live re-renders keep the ticked boxes, even when they move, and skip the
  typed subjects."
  [db messages kind]
  (let [creds (store/credentials db)
        subjects (known-subjects (store/assignments db) (store/identities db))]
    [:form#assign-form.panel {:data-on:submit__prevent (lib/form-post assign)}
     [:div.steps
      [:fieldset
       [:legend [:span.step "1"] "Credentials"]
       (if (seq creds)
         (for [{:keys [name] :as c} (sort-by (comp str/lower-case domain/title) creds)]
           (option :credential name (domain/title c) " " [:small [:code name]]))
         [:p.muted "No credentials yet."])]
      [:fieldset
       [:legend [:span.step "2"] "Subjects"]
       (for [s subjects]
         (option :subject s [:code s]))
       [:label "Other subjects" [:small.muted " group:name or an identity such as tag:t3-alice, one per line"]
        [:textarea#other-subjects {:name "subjects" :rows 2 :data-ignore-morph true}]]]
      [:fieldset
       [:legend [:span.step "3"] "Grant"]
       [:label "Mode" [:select {:name "mode"} (for [m (sort domain/modes)] [:option {:value m} m])]]
       [:label "Approval"
        [:select {:name "approval"}
         [:option {:value "ask"} "ask each identity"]
         [:option {:value "auto"} "auto"]]]]]
     [:div.form-footer
      [:button {:type "submit"} "Assign"]
      [:small.muted "Gives every subject ticked or typed every ticked credential, with the grant."]
      (feedback "assign-feedback" messages kind)]]))

(lib/defaction assign
  "Assigns the ticked credentials to the subjects and answers with a fresh
  form that says how many assignments it saved."
  [{:keys [store]} request]
  (feedback-on-invalid
   "assign-feedback"
   #(let [n (store/write! store {:command     :assignment/assign
                                 :credentials (lib/params request :credential)
                                 :subjects    (vec (distinct (concat (lib/params request :subject)
                                                                     (domain/lines (lib/param request :subjects)))))
                                 :mode        (lib/param request :mode)
                                 :approval    (lib/param request :approval)
                                 :actor       (:actor request)})
          saved (str "Saved " n (if (= 1 n) " assignment." " assignments."))]
      {:replace (store/read store (fn [db] (assign-form db [saved] :ok)))})))

(lib/defaction remove-assignment [{:keys [store]} request]
  (store/write! store {:command    :assignment/remove
                       :subject    (lib/param request :subject)
                       :credential (lib/param request :credential)
                       :mode       (lib/param request :mode)
                       :actor      (:actor request)}))
