(ns ts-broker.actions
  "Actions of the broker UI, and the credential form fields they read.

  Actions turn the parameters of the request body into store commands; the
  store validates them. Controls send them as a form or a Datastar
  `payload`, never in the URL. A successful credential edit answers with a
  fresh edit form."
  (:require
   [clojure.string :as str]
   [ts-broker.connect :as connect]
   [ts-broker.domain :as domain]
   [ts-broker.lib :as lib]
   [ts-broker.store :as store]))

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
  "Returns the element with `id` that shows `messages` next to a form.
  Actions replace it, so it belongs inside a form that morphing skips."
  [id messages]
  [:div.feedback {:id id :role "alert"}
   (when (seq messages)
     [:div.message (for [m messages] [:p m])])])

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

(lib/defaction create-credential
  "Creates a credential from the catalog form and moves the browser to it.
  Refusals show in the form's `create-feedback` element."
  [{:keys [connect store]} request]
  (feedback-on-invalid
   "create-feedback"
   #(let [[vault item field] (map (fn [k] (lib/param request k)) [:vault :item :field])
          picked (when (and connect vault item field)
                   (->> (:fields (connect/item connect vault item))
                        (filter (fn [f] (= field (:id f))))
                        first))
          name (store/write! store {:command    :credential/create
                                    :credential {:name        (lib/param request :name)
                                                 :vault       vault
                                                 :item        item
                                                 :field       field
                                                 :field-label (when picked
                                                                (if (str/blank? (:label picked)) field (:label picked)))
                                                 :header      (lib/param request :header)
                                                 :hosts       (domain/split-list (lib/param request :hosts))}
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
  replaces it or the page loads again."
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
                     :data-on:click (lib/confirm-post (str "Delete " name " and its assignments?")
                                                      delete-credential {:name name :revision revision})}
     "Delete"]]])

(lib/defaction update-credential [{:keys [store]} request]
  (let [name (store/write! store {:command  :credential/update
                                  :name     (lib/param request :name)
                                  :revision (lib/param request :revision)
                                  :header   (lib/param request :header)
                                  :hosts    (domain/split-list (lib/param request :hosts))
                                  :actor    (:actor request)})]
    {:replace (edit-form (store/read store #(store/credential % name)))}))

(lib/defaction assign [{:keys [store]} request]
  (store/write! store {:command     :assignment/assign
                       :credentials (lib/params request :credential)
                       :subjects    (vec (distinct (concat (lib/params request :subject)
                                                           (domain/lines (lib/param request :subjects)))))
                       :mode        (lib/param request :mode)
                       :approval    (lib/param request :approval)
                       :actor       (:actor request)}))

(lib/defaction remove-assignment [{:keys [store]} request]
  (store/write! store {:command    :assignment/remove
                       :subject    (lib/param request :subject)
                       :credential (lib/param request :credential)
                       :mode       (lib/param request :mode)
                       :actor      (:actor request)}))
