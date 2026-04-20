#!/usr/bin/env bb
;; Scratch helpers for Invoice Ninja API exploration.
;; Check an existing nREPL if you know the port:
;;   BREPL_PORT=<port> brepl '(+ 1 2)'
;; Start a Babashka nREPL if needed:
;;   bb --nrepl-server 0
;; Then load this file:
;;   BREPL_PORT=<port> brepl -f ./scratch_invoiceninja.clj
(ns scratch-invoiceninja
  (:require
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [clojure.edn :as edn]))

(def config-file
  (str (System/getProperty "user.home") "/.config/invoiceninja-cli/config.edn"))

(defn load-config
  []
  (edn/read-string (slurp config-file)))

(defn api-token
  [cfg]
  (or (:api-token cfg) (:api-key cfg)))

(defn json-headers
  ([] (json-headers (load-config)))
  ([cfg]
   {"X-API-TOKEN" (api-token cfg)
    "X-Requested-With" "XMLHttpRequest"
    "Accept" "application/json"}))

(defn parse-json
  [body]
  (try
    (json/parse-string body true)
    (catch Exception _
      body)))

(defn json-get
  ([path] (json-get path {}))
  ([path opts]
   (let [cfg (load-config)
         resp (http/get (str (:base-url cfg) path)
                        (merge {:throw false
                                :headers (json-headers cfg)}
                               opts))]
     {:status (:status resp)
      :headers (:headers resp)
      :body (parse-json (:body resp))})))

(defn json-post
  ([path payload] (json-post path payload {}))
  ([path payload opts]
   (let [cfg (load-config)
         body (if (string? payload) payload (json/generate-string payload))
         resp (http/post (str (:base-url cfg) path)
                         (merge {:throw false
                                 :headers (merge {"Content-Type" "application/json"}
                                                 (json-headers cfg))
                                 :body body}
                                opts))]
     {:status (:status resp)
      :headers (:headers resp)
      :body (parse-json (:body resp))})))

(defn binary-get
  ([path] (binary-get path {}))
  ([path opts]
   (let [cfg (load-config)
         resp (http/get (str (:base-url cfg) path)
                        (merge {:throw false
                                :headers (json-headers cfg)
                                :as :bytes}
                               opts))]
     {:status (:status resp)
      :headers (:headers resp)
      :body (:body resp)})))

(defn ping
  []
  (json-get "/api/v1/ping"))

(defn login-token
  "Bootstrap a token from email/password in config. Prefer using a pre-created
  API token in config instead of calling this routinely."
  []
  (let [cfg (load-config)
        headers (cond-> {"Content-Type" "application/json"
                         "Accept" "application/json"
                         "X-Requested-With" "XMLHttpRequest"}
                  (:api-secret cfg) (assoc "X-API-SECRET" (:api-secret cfg)))
        payload (select-keys cfg [:email :password :one_time_password])
        resp (http/post (str (:base-url cfg) "/api/v1/login")
                        {:throw false
                         :headers headers
                         :body (json/generate-string payload)})]
    {:status (:status resp)
     :headers (:headers resp)
     :body (parse-json (:body resp))
     :token (get-in (parse-json (:body resp)) [:token :token])}))

(defn list-invoices
  ([] (list-invoices {}))
  ([query]
   (get-in (json-get "/api/v1/invoices" {:query-params query}) [:body :data])))

(defn latest-open-invoices
  ([] (latest-open-invoices 5))
  ([per-page]
   (list-invoices {"client_status" "unpaid,overdue"
                   "status" "active"
                   "without_deleted_clients" "true"
                   "sort" "date|desc"
                   "per_page" per-page})))

(defn latest-open-invoice
  []
  (first (latest-open-invoices 1)))

(defn show-invoice
  [invoice-id]
  (get-in (json-get (str "/api/v1/invoices/" invoice-id)) [:body :data]))

(defn invoice-invitation-key
  [invoice]
  (get-in invoice [:invitations 0 :key]))

(defn download-invoice-pdf
  [invitation-key]
  (binary-get (str "/api/v1/invoice/" invitation-key "/download")))

(defn spit-bytes!
  [path bytes]
  (with-open [out (java.io.FileOutputStream. path)]
    (.write out bytes))
  path)

(defn download-invoice-pdf!
  [invitation-key path]
  (let [resp (download-invoice-pdf invitation-key)]
    (spit-bytes! path (:body resp))
    {:path path
     :status (:status resp)
     :headers (:headers resp)}))

(defn download-latest-open-invoice-pdf!
  [path]
  (let [invoice (latest-open-invoice)
        invitation-key (invoice-invitation-key invoice)
        resp (download-invoice-pdf! invitation-key path)]
    (assoc resp
           :invoice (select-keys invoice
                                 [:id :number :status_id :amount :balance :date :due_date])
           :invitation-key invitation-key)))
