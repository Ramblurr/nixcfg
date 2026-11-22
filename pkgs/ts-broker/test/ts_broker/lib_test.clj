(ns ts-broker.lib-test
  (:require
   [clojure.test :refer [deftest is]]
   [ts-broker.actions]
   [ts-broker.lib :as lib]
   [ts-broker.pages]))

(lib/defaction approval [_ _])

(lib/defview home {:path "/" :title "Home"} [_ _])

(deftest refuses-two-definitions-of-one-route
  (is (= {:pages ["/"] :actions ["/action/approval"]}
         (update-vals (lib/routes 'ts-broker.lib-test) (comp vec keys))))
  (let [e (try (lib/routes 'ts-broker.pages 'ts-broker.actions 'ts-broker.lib-test)
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= (str "Two definitions share a route: / (ts-broker.pages/approvals-page, ts-broker.lib-test/home),"
                " /action/approval (ts-broker.actions/approval, ts-broker.lib-test/approval).")
           (ex-message e)))))
