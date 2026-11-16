#!/usr/bin/env bb
(require '[babashka.fs :as fs] '[babashka.process :as process]
         '[cheshire.core :as json] '[clojure.string :as str])

(defn- read-secret [reference reader secret-files]
  (let [secret (str/trim
                (if-let [file (get secret-files reference)]
                  (try (slurp file)
                       (catch java.io.IOException _
                         (throw (ex-info (str "Cannot read secret file for " reference) {:safe true}))))
                  (let [{:keys [exit out]} (process/shell {:in "" :out :string :err :string :continue true}
                                                          reader reference)]
                    (when-not (zero? exit) (throw (ex-info (str "Cannot resolve " reference) {:safe true})))
                    out)))]
    (when (str/blank? secret) (throw (ex-info (str "Empty credential: " reference) {:safe true})))
    secret))

(defn render [value reader secret-files]
  (cond
    (map? value) (update-vals value #(render % reader secret-files))
    (sequential? value) (mapv #(render % reader secret-files) value)
    (and (string? value) (str/starts-with? value "op://")) (read-secret value reader secret-files)
    :else value))

(defn write-config! [settings output]
  (let [content (str (json/generate-string settings) "\n")
        temporary (fs/create-temp-file {:dir (fs/parent (fs/absolutize output))
                                        :prefix ".config-" :posix-file-permissions "rw-------"})]
    (try
      (spit (fs/file temporary) content)
      (when (fs/directory? output) (throw (ex-info "Config output is a directory" {:safe true})))
      (fs/move temporary output {:replace-existing true :atomic-move true})
      (finally (fs/delete-if-exists temporary)))))

(defn generate! [template reader output]
  (let [{:strs [settings secretFiles]} (json/parse-string (slurp template))]
    (write-config! (render settings reader secretFiles) output)))

(defn -main [& args]
  (if (not= 3 (count args))
    (do (binding [*out* *err*] (println "Usage: render-op-config.clj TEMPLATE READER OUTPUT")) (System/exit 2))
    (try (apply generate! args)
         (catch Exception e
           (binding [*out* *err*]
             (println "Config generation failed:"
                      (if (:safe (ex-data e)) (ex-message e) (.getName (class e)))))
           (System/exit 1)))))
(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
