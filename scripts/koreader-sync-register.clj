#!/usr/bin/env bb

(ns koreader-sync-register
  (:require
   [babashka.http-client :as http]
   [babashka.process :as process]
   [cheshire.core :as json]
   [clojure.string :as str]))

(def usage
  "Usage: scripts/koreader-sync-register.clj URL USERNAME PASSWORD-COMMAND [ARG ...]")

(defn register-user [url username password-command]
  (let [password (-> (apply process/shell
                            {:err :inherit
                             :out :string}
                            password-command)
                     :out
                     str/trim-newline)
        response (http/post (str (str/replace url #"/+$" "") "/users/create")
                            {:body (json/generate-string {:username username
                                                         :password password})
                             :headers {:content-type "application/json"}
                             :throw false})]
    (when-not (= 201 (:status response))
      (throw (ex-info "KOReader Sync registration failed"
                      {:body (:body response)
                       :status (:status response)})))
    response))

(defn -main [& args]
  (let [[url username & password-command] args]
    (when-not (and url username (seq password-command))
      (binding [*out* *err*]
        (println usage))
      (System/exit 2))
    (register-user url username password-command)
    (println "Registered KOReader Sync user" username)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
