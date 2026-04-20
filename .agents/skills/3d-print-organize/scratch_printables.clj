;; Scratch helpers for Printables GraphQL exploration.
;; Check an existing nREPL if you know the port:
;;   BREPL_PORT=<port> brepl '(+ 1 2)'
;; Start a Babashka nREPL if needed:
;;   bb --nrepl-server 0
;; Then load this file:
;;   BREPL_PORT=<port> brepl -f .agents/skills/3d-print-organize/scratch_printables.clj
(ns scratch-printables
  (:require
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [babashka.process :refer [shell]]
   [cheshire.core :as json]
   [clojure.string :as str]))

(def graphql-endpoint "https://api.printables.com/graphql/")
(def default-cookie-db
  (fs/expand-home "~/.mozilla/firefox/personal/cookies.sqlite"))

(def browser-user-agent
  "Mozilla/5.0 (X11; Linux x86_64; rv:149.0) Gecko/20100101 Firefox/149.0")

(def base-headers
  {"User-Agent" browser-user-agent
   "Accept"
   "application/graphql-response+json, application/graphql+json, application/json, text/event-stream, multipart/mixed"
   "Accept-Language" "en"
   "content-type" "application/json"
   "Origin" "https://www.printables.com"})

(def print-query
  "query Print($id: ID!) {
     print(id: $id) {
       id
       name
       slug
     }
   }")

(def my-collections-query
  "query MyCollections($myId: ID!) {
     collections: userCollections(userId: $myId) {
       ...CollectionForSelection
       __typename
     }
   }

   fragment CollectionForSelection on CollectionType {
     id
     name
     private
     modelsCount: printsCount
     modified
     __typename
   }")

(def model-files-query
  "query ModelFiles($id: ID!) {
     model: print(id: $id) {
       id
       name
       slug
       filesType
       gcodes { id name folder note __typename }
       stls { id name folder note __typename }
       slas { id name folder note __typename }
       otherFiles { id name folder note __typename }
       downloadPacks { id name fileSize fileType __typename }
       __typename
     }
   }")

(def like-create-mutation
  "mutation LikeCreate($targetObjectId: ID!, $targetType: LikeTargetTypeEnum!) {
     likeCreate(targetObjectId: $targetObjectId, targetType: $targetType) {
       ok
       errors {
         ...Error
         __typename
       }
       count
       __typename
     }
   }

   fragment Error on ErrorType {
     field
     messages
     __typename
   }")

(def get-download-link-mutation
  "mutation GetDownloadLink($printId: ID!, $ids: [ID!]!, $ft: DownloadFileTypeEnum!) {
     getDownloadLink(printId: $printId, source: model_detail, files: [{fileType: $ft, ids: $ids}]) {
       ok
       errors {
         field
         messages
         code
         __typename
       }
       output {
         link
         ttl
         __typename
       }
       __typename
     }
   }")

(defn load-printables-cookies
  "Read the current Printables-related cookies from a Firefox cookies.sqlite DB."
  ([] (load-printables-cookies default-cookie-db))
  ([cookie-db]
   (fs/with-temp-dir [tmpdir]
     (let [tmp-db (fs/path tmpdir "cookies.sqlite")
           _ (fs/copy cookie-db tmp-db {:replace-existing true})
           sql (str
                "select host, name, value, expiry from moz_cookies "
                "where (host = '.printables.com' "
                "       and name in ('cookieyes-consent','cf_clearance',"
                "                    'auth.access_token','auth.access_token_exp',"
                "                    'auth.refresh_token','auth.refresh_token_exp')) "
                "   or (host = 'api.printables.com' and name = 'csrftoken') "
                "   or (host = 'www.printables.com' and name = 'client-uid') "
                "order by host, name, expiry desc;")
           lines (-> (shell {:out :string}
                            "sqlite3" "-separator" "\t" (str tmp-db) sql)
                     :out
                     str/split-lines)]
       (reduce (fn [acc line]
                 (let [[host name value _expiry] (str/split line #"\t" 4)
                       k [host name]]
                   ;; Keep the first row for each key because the query orders by
                   ;; descending expiry.
                   (if (contains? acc k) acc (assoc acc k value))))
               {}
               lines)))))

(defn browser-session
  "Build the browser-style auth material used by authenticated operations."
  ([] (browser-session default-cookie-db))
  ([cookie-db]
   (let [cookies (load-printables-cookies cookie-db)
         cookie-header (->> [(some-> (get cookies [".printables.com" "cookieyes-consent"])
                                     (str "cookieyes-consent="))
                             (some-> (get cookies ["api.printables.com" "csrftoken"])
                                     (str "csrftoken="))
                             (some-> (get cookies [".printables.com" "cf_clearance"])
                                     (str "cf_clearance="))
                             (some-> (get cookies [".printables.com" "auth.access_token"])
                                     (str "auth.access_token="))
                             (some-> (get cookies [".printables.com" "auth.access_token_exp"])
                                     (str "auth.access_token_exp="))
                             (some-> (get cookies [".printables.com" "auth.refresh_token"])
                                     (str "auth.refresh_token="))
                             (some-> (get cookies [".printables.com" "auth.refresh_token_exp"])
                                     (str "auth.refresh_token_exp="))]
                            (remove str/blank?)
                            (str/join "; "))]
     {:client-uid (get cookies ["www.printables.com" "client-uid"])
      :cookie-header cookie-header
      :cookies cookies})))

(defn public-headers []
  (assoc base-headers
         "Referer" "https://www.printables.com/"))

(defn auth-headers
  "Use the same broad request shape as a real browser session."
  [session]
  (assoc base-headers
         "client-uid" (:client-uid session)
         "graphql-client-version" "v4.5.0"
         "Cookie" (:cookie-header session)))

(defn gql
  "POST a GraphQL request and return {:status :body} with parsed JSON body."
  [headers operation-name query variables]
  (let [resp (http/post graphql-endpoint
                        {:throw false
                         :headers headers
                         :body (json/generate-string
                                {:operationName operation-name
                                 :query query
                                 :variables variables})})]
    {:status (:status resp)
     :body (json/parse-string (:body resp) true)}))

(defn curl-gql
  "POST a browser-shaped GraphQL request with curl. Use this for auth mutations."
  [session operation-name query variables]
  (let [payload (json/generate-string
                 {:operationName operation-name
                  :query query
                  :variables variables})
        resp (shell {:out :string}
                    "curl" "-sS" "--compressed" "-X" "POST"
                    graphql-endpoint
                    "-H" (str "User-Agent: " browser-user-agent)
                    "-H" (str "Accept: " (get base-headers "Accept"))
                    "-H" "Accept-Language: en"
                    "-H" (str "client-uid: " (:client-uid session))
                    "-H" "graphql-client-version: v4.5.0"
                    "-H" "content-type: application/json"
                    "-H" "Origin: https://www.printables.com"
                    "-H" "DNT: 1"
                    "-H" "Connection: keep-alive"
                    "-H" "Sec-Fetch-Dest: empty"
                    "-H" "Sec-Fetch-Mode: cors"
                    "-H" "Sec-Fetch-Site: same-site"
                    "-H" "Priority: u=4"
                    "-H" "Pragma: no-cache"
                    "-H" "Cache-Control: no-cache"
                    "-H" "TE: trailers"
                    "-H" (str "Cookie: " (:cookie-header session))
                    "--data-raw" payload)]
    {:status (:exit resp)
     :body (try
             (json/parse-string (:out resp) true)
             (catch Exception _
               (:out resp)))}))

(defn fetch-print
  "Public model lookup."
  [model-id]
  (gql (public-headers) "Print" print-query {:id (str model-id)}))

(defn fetch-my-collections
  "Authenticated collection listing. Note: the live API currently expects ID!."
  [session my-user-id]
  (curl-gql session
            "MyCollections"
            my-collections-query
            {:myId (str my-user-id)}))

(defn fetch-model-files
  "Public model-file inspection, including download pack metadata."
  [model-id]
  (gql (public-headers) "ModelFiles" model-files-query {:id (str model-id)}))

(defn like-model!
  "Authenticated like mutation. Keep this close to the working browser request."
  [session model-id]
  (curl-gql session
            "LikeCreate"
            like-create-mutation
            {:targetObjectId (str model-id)
             :targetType "print"}))

(defn pack-download-link
  "Get a signed download URL for one pack id from `downloadPacks`."
  [session model-id pack-id]
  (curl-gql session
            "GetDownloadLink"
            get-download-link-mutation
            {:printId (str model-id)
             :ids [(str pack-id)]
             :ft "pack"}))

(def add-model-to-collection-mutation
  "mutation ModelAddToCollection($collectionId: ID!, $modelId: ID) {
     addPrintToCollection(collectionId: $collectionId, printId: $modelId) {
       ok
       errors {
         ...Error
         __typename
       }
       output {
         ...CollectionForSelection
         __typename
       }
       __typename
     }
   }

   fragment CollectionForSelection on CollectionType {
     id
     name
     private
     modelsCount: printsCount
     modified
     __typename
   }

   fragment Error on ErrorType {
     field
     messages
     __typename
   }")

(defn add-model-to-collection!
  "Add a model to a collection using the verified browser-shaped curl path."
  [session collection-id model-id]
  (curl-gql session
            "ModelAddToCollection"
            add-model-to-collection-mutation
            {:collectionId (str collection-id)
             :modelId (str model-id)}))

(comment
  ;; Public read.
  (fetch-print "12345")

  ;; Authenticated read.
  (def session (browser-session))
  (fetch-my-collections session "123456")

  ;; Download pack / file-group inspection.
  (fetch-model-files "12345")

  ;; Authenticated mutation.
  (like-model! session "12345")

  ;; Signed pack download link.
  (pack-download-link session "12345" "67890")

  ;; Add to collection.
  (add-model-to-collection! session "67890" "12345"))
