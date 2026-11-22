(ns ts-broker.test-runner
  "Runs every test namespace of ts-broker."
  (:require
   [clojure.test :as t]
   [ts-broker.domain-test]
   [ts-broker.store-test]
   [ts-broker.ui-test]))

(defn run
  "Runs the tests and returns the number of failures and errors."
  []
  (let [{:keys [fail error]} (t/run-tests 'ts-broker.domain-test 'ts-broker.store-test 'ts-broker.ui-test)]
    (+ fail error)))
