#!/usr/bin/env bb

(require '[babashka.deps :as deps])

(deps/add-deps
 '{:deps {com.outskirtslabs/sops {:mvn/version "0.1.0"}}})

(require '[babashka.fs :as fs]
         '[babashka.process :as process]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[ol.sops :as sops])

(def default-vault "home-ops-prod")
(def missing-value (Object.))

(def usage
  (str/join
   "\n"
   ["Usage: scripts/sops2op.clj [--execute] [--vault VAULT] MANIFEST"
    ""
    "Without --execute, print the import plan without creating items."
    "With --execute, create the planned items after the full plan succeeds."
    "Relative source paths resolve against the current working directory."
    ""
    "Manifest format (EDN; do not put secret values in the manifest):"
    "  {:vault \"home-ops-prod\""
    "   :items [{:title \"paperless\""
    "            :source \"configs/home-ops/shared.sops.yml\""
    "            :fields {\"admin-password\" [\"paperless\" \"adminPassword\"]"
    "                     \"oidc-provider\" [\"paperless\" \"oidcProvider\"]}}]}"
    ""
    "Each field path is a non-empty vector of string map keys and non-negative integer indexes."
    "Each :title becomes one 1Password SECURE_NOTE item."
    "Each :source selects the SOPS file to decrypt."
    "Each :fields entry maps a destination field label to a path in the decrypted SOPS data."
    "Each selected value becomes a separate CONCEALED 1Password field."
    "Field references use op://<vault>/<item-title>/<field-label>."
    "The command-line --vault overrides :vault, which defaults to home-ops-prod."
    ""
    "Human-in-the-loop: SOPS decryption may require PIN entry and a YubiKey touch."
    "Each invocation decrypts each distinct pending source once, after existing-title skips."
    "Put the largest practical batch in one manifest to minimize decryption prompts."
    "Agents must not rerun this command casually; have a human review and approve the batch."
    "Plan mode also decrypts. Running plan and execute separately repeats decryption."
    "Execute mode plans and creates in one invocation without a second decryption."
    "Plan first, review the output, then execute only with human approval."]))

(defn nonblank-string? [value]
  (and (string? value)
       (not (str/blank? value))))

(defn item-label [index]
  (format "item-%d" (inc index)))

(defn field-label [index]
  (format "field-%d" (inc index)))

(defn parse-args [args]
  (loop [args args
         options {:execute? false
                  :manifest nil
                  :vault nil}]
    (if (empty? args)
      (cond
        (:help? options) options
        (nil? (:manifest options)) (assoc options :help? true)
        :else options)
      (let [arg (first args)]
        (cond
          (or (= arg "--help") (= arg "-h"))
          (recur (rest args) (assoc options :help? true))

          (= arg "--execute")
          (recur (rest args) (assoc options :execute? true))

          (= arg "--vault")
          (if (next args)
            (recur (nnext args) (assoc options :vault (second args)))
            (throw (ex-info "--vault requires a value" {})))

          (str/starts-with? arg "--vault=")
          (recur (rest args) (assoc options :vault (subs arg 8)))

          (str/starts-with? arg "-")
          (throw (ex-info (str "unknown option: " arg) {}))

          (:manifest options)
          (throw (ex-info "expected exactly one manifest path" {}))

          :else
          (recur (rest args) (assoc options :manifest arg)))))))

(defn read-manifest [path]
  (try
    {:manifest (edn/read-string (slurp path))}
    (catch Exception _
      {:errors [(str "could not read a valid EDN manifest: " path)]})))

(defn valid-path? [path]
  (and (vector? path)
       (seq path)
       (every? #(or (string? %)
                    (and (integer? %)
                         (not (neg? %))))
               path)))

(defn absolute-source-path [source]
  (str (fs/normalize (fs/absolutize source))))

(defn source-error [source]
  (cond
    (not (string? source)) "source must be a string"
    (str/blank? source) "source must not be blank"
    :else (try
            (cond
              (not (fs/exists? source))
              (str "source file does not exist: " source)

              (not (fs/regular-file? source))
              (str "source is not a regular file: " source)

              :else nil)
            (catch Exception _
              (str "source path is invalid: " source)))))

(defn path-error [path]
  (cond
    (not (vector? path)) "field path must be a vector"
    (empty? path) "field path must not be empty"
    (not (every? #(or (string? %)
                      (and (integer? %)
                           (not (neg? %))))
                 path))
    "field path components must be strings or non-negative integers"
    :else nil))

(defn field-specs [fields]
  (if-not (map? fields)
    {:errors ["fields must be a map"]}
    (if (empty? fields)
      {:errors ["fields must not be empty"]}
      (let [entries (vec fields)
            errors (keep-indexed
                    (fn [index [label path]]
                      (cond
                        (not (nonblank-string? label))
                        (format "%s has a blank or non-string destination label"
                                (field-label index))

                        (path-error path)
                        (format "field %s (%s): %s"
                                label path (path-error path))

                        :else nil))
                    entries)]
        {:errors (vec errors)
         :fields (mapv (fn [[label path]] {:label label :path path}) entries)}))))

(defn validate-item [index item]
  (if-not (map? item)
    {:index index
     :title (item-label index)
     :errors [(format "%s must be a map" (item-label index))]}
    (let [title (:title item)
          source (:source item)
          fields-result (field-specs (:fields item))
          source-path (when (and (string? source)
                                 (not (str/blank? source)))
                        (try (absolute-source-path source)
                             (catch Exception _ nil)))
          errors (cond-> (vec (:errors fields-result))
                   (not (nonblank-string? title))
                   (conj "title must be a nonblank string")

                   (source-error source)
                   (conj (source-error source)))]
      {:index index
       :title (if (nonblank-string? title) title (item-label index))
       :source source
       :source-path source-path
       :fields (:fields fields-result)
       :errors errors})))

(defn duplicate-title-errors [items]
  (let [counts (frequencies (keep #(when (nonblank-string? (:title %))
                                    (:title %))
                                 items))]
    (into {}
          (keep (fn [[title count]]
                  (when (> count 1)
                    [title (format "duplicate item title: %s" title)])))
          counts)))

(defn validate-manifest [manifest vault-override]
  (if-not (map? manifest)
    {:vault nil
     :items []
     :errors ["manifest top level must be a map"]}
    (let [manifest-vault (:vault manifest)
          vault-errors (cond
                         (and (contains? manifest :vault)
                              (not (nonblank-string? manifest-vault)))
                         ["manifest :vault must be a nonblank string"]

                         (and vault-override
                              (not (nonblank-string? vault-override)))
                         ["--vault must be a nonblank string"]

                         :else [])
          items-value (:items manifest)
          items-errors (if (sequential? items-value)
                         []
                         ["manifest :items must be a sequential collection"])
          items (if (sequential? items-value)
                  (mapv validate-item (range (count items-value)) items-value)
                  [])
          duplicate-errors (duplicate-title-errors items)
          items (mapv (fn [item]
                        (if-let [error (duplicate-errors (:title item))]
                          (update item :errors conj error)
                          item))
                      items)
          structural-errors (vec (concat vault-errors items-errors))]
      {:vault (or vault-override manifest-vault default-vault)
       :items items
       :errors structural-errors})))

(defn query-existing-titles [vault]
  (try
    (let [{:keys [exit out]} (process/shell {:out :string
                                              :err :string
                                              :continue true}
                                             "op" "item" "list"
                                             "--vault" vault
                                             "--format" "json")]
      (if (zero? exit)
        (try
          (let [items (json/parse-string out false)]
            (if (and (sequential? items)
                     (every? #(and (map? %)
                                   (string? (get % "title")))
                             items))
              {:titles (set (map #(get % "title") items))}
              {:errors ["1Password item list returned an invalid JSON shape"]}))
          (catch Exception _
            {:errors ["1Password item list returned invalid JSON"]}))
        {:errors [(format "1Password item list failed with exit status %s" exit)]}))
    (catch Exception _
      {:errors ["could not start 1Password item list"]})))

(defn classify-existing [items titles]
  (mapv (fn [item]
          (if (contains? titles (:title item))
            (assoc item :status :skipped)
            (assoc item :status :pending)))
        items))

(defn decrypt-source [source-path]
  (try
    (let [plaintext (sops/decrypt-file-to-str source-path {:output-type "json"})]
      (try
        {:data (json/parse-string plaintext false)}
        (catch Exception _
          {:error (str "decrypted source is not valid JSON: " source-path)})))
    (catch Exception _
      {:error (str "could not decrypt source: " source-path)})))

(defn decrypt-sources [items]
  (reduce (fn [sources source-path]
            (assoc sources source-path (decrypt-source source-path)))
          {}
          (distinct (map :source-path (filter #(= :pending (:status %)) items)))))

(defn lookup-path [data path]
  (loop [value data
         components (seq path)]
    (if (empty? components)
      {:present? true :value value}
      (let [component (first components)]
        (cond
          (map? value)
          (if (contains? value component)
            (recur (get value component) (next components))
            {:present? false})

          (and (sequential? value)
               (integer? component)
               (<= 0 component)
               (< component (count value)))
          (recur (nth value component) (next components))

          :else
          {:present? false})))))

(defn value-error [source-path label path source-result]
  (if-let [error (:error source-result)]
    error
    (let [{:keys [present? value]} (lookup-path (:data source-result) path)]
      (cond
        (not present?) (format "source %s has no value at field %s" source-path label)
        (nil? value) (format "source %s has nil at field %s" source-path label)
        (not (string? value)) (format "source %s has a non-string value at field %s"
                                       source-path label)
        (empty? value) (format "source %s has an empty string at field %s"
                               source-path label)
        :else nil))))

(defn attach-values [item sources]
  (if (= :skipped (:status item))
    item
    (let [source-result (get sources (:source-path item))
          results (map (fn [{:keys [label path]}]
                         [label (value-error (:source-path item)
                                             label path source-result)
                          (when-not (value-error (:source-path item)
                                                 label path source-result)
                            (:value (lookup-path (:data source-result) path)))])
                       (:fields item))
          errors (vec (keep second results))
          values (into {} (keep (fn [[label error value]]
                                  (when-not error [label value]))
                                results))]
      (assoc item :errors (into (:errors item) errors) :values values))))

(defn all-errors [plan]
  (vec (distinct (concat (:errors plan)
                         (mapcat :errors (:items plan))))))

(defn finalize-plan [plan]
  (let [errors (all-errors plan)]
    (assoc plan
           :errors errors
           :items (mapv (fn [item]
                          (if (or (= :skipped (:status item))
                                  (empty? errors))
                            item
                            (-> item
                                (assoc :status :error)
                                (update :errors #(if (seq %)
                                                   %
                                                   ["blocked by planning errors"])))))
                        (:items plan)))))

(defn build-plan [manifest-path vault-override]
  (let [{:keys [manifest errors]} (read-manifest manifest-path)]
    (if errors
      (finalize-plan {:vault nil :items [] :errors errors})
      (let [structural (validate-manifest manifest vault-override)]
        (if (seq (:errors structural))
          (finalize-plan structural)
          (let [{:keys [titles errors]} (query-existing-titles (:vault structural))]
            (if errors
              (finalize-plan (assoc structural :errors errors))
              (let [items (classify-existing (:items structural) titles)
                    sources (decrypt-sources items)
                    source-errors (keep :error (vals sources))
                    items (mapv #(attach-values % sources) items)]
                (finalize-plan {:vault (:vault structural)
                                :items items
                                :errors (vec source-errors)})))))))))

(defn plan-summary [items errors]
  (let [planned (count (filter #(= :pending (:status %)) items))
        skipped (count (filter #(= :skipped (:status %)) items))
        errored (count (filter #(= :error (:status %)) items))]
    {:planned planned
     :skipped skipped
     :errored (if (and (zero? errored) (seq errors)) 1 errored)}))

(defn print-errors [errors]
  (doseq [error errors]
    (println "ERROR" error)))

(defn field-reference [vault item label]
  (format "op://%s/%s/%s" vault (:title item) label))

(defn print-field-references [vault item]
  (doseq [{:keys [label]} (:fields item)]
    (println (field-reference vault item label))))

(defn print-plan [plan]
  (print-errors (:errors plan))
  (doseq [item (:items plan)]
    (case (:status item)
      :pending (do
                 (println "CREATE" (:title item))
                 (print-field-references (:vault plan) item))
      :skipped (do
                 (println "SKIP" (:title item) "already exists")
                 (print-field-references (:vault plan) item))
      :error (println "ERROR" (:title item)
                      (str/join "; " (:errors item)))))
  (let [{:keys [planned skipped errored]} (plan-summary (:items plan) (:errors plan))]
    (println (format "SUMMARY planned=%d skipped=%d errored=%d"
                     planned skipped errored))))

(defn item-template [item]
  (json/generate-string
   {"title" (:title item)
    "category" "SECURE_NOTE"
    "fields" (mapv (fn [{:keys [label]}]
                      {"label" label
                       "type" "CONCEALED"
                       "value" (get (:values item) label)})
                    (:fields item))}))

(defn create-item [vault item]
  (try
    (let [{:keys [exit out]} (process/shell {:in (item-template item)
                                              :out :string
                                              :err :string
                                              :continue true}
                                             "op" "item" "create"
                                             "--vault" vault
                                             "--format" "json"
                                             "-")]
      (if (zero? exit)
        {:created? true
         :id (try
               (let [response (json/parse-string out false)]
                 (when (map? response) (get response "id")))
               (catch Exception _ nil))}
        {:created? false
         :error (format "1Password item creation failed with exit status %s" exit)}))
    (catch Exception _
      {:created? false
       :error "could not start 1Password item creation"})))

(defn print-execute-planning-failure [plan]
  (print-errors (:errors plan))
  (doseq [item (:items plan)]
    (case (:status item)
      :skipped (do
                 (println "SKIPPED" (:title item) "already exists")
                 (print-field-references (:vault plan) item))
      :error (println "FAILED" (:title item) "planning failed")
      nil))
  (let [skipped (count (filter #(= :skipped (:status %)) (:items plan)))
        failed (count (filter #(= :error (:status %)) (:items plan)))]
    (println (format "SUMMARY created=0 skipped=%d failed=%d" skipped failed))))

(defn execute-plan [plan]
  (if (seq (:errors plan))
    (do
      (print-execute-planning-failure plan)
      1)
    (let [results (mapv (fn [item]
                          (if (= :skipped (:status item))
                            (do
                              (println "SKIPPED" (:title item) "already exists")
                              (print-field-references (:vault plan) item)
                              {:status :skipped})
                            (let [{:keys [created? error]} (create-item (:vault plan) item)]
                              (if created?
                                (do
                                  (println "CREATED" (:title item))
                                  (print-field-references (:vault plan) item)
                                  {:status :created})
                                (do
                                  (println "FAILED" (:title item) error)
                                  {:status :failed})))))
                        (:items plan))
          counts (frequencies (map :status results))
          created (get counts :created 0)
          skipped (get counts :skipped 0)
          failed (get counts :failed 0)]
      (println (format "SUMMARY created=%d skipped=%d failed=%d"
                       created skipped failed))
      (if (zero? failed) 0 1))))

(defn -main [& args]
  (try
    (let [{:keys [execute? help? manifest vault]} (parse-args args)]
      (if help?
        (println usage)
        (let [plan (build-plan manifest vault)]
          (if execute?
            (System/exit (execute-plan plan))
            (do
              (print-plan plan)
              (System/exit (if (seq (:errors plan)) 1 0)))))))
    (catch Exception _
      (println "ERROR invalid command-line arguments")
      (println usage)
      (System/exit 2))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))