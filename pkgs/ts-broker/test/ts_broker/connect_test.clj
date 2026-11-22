(ns ts-broker.connect-test
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is]]
   [org.httpkit.server :as server]
   [ts-broker.connect :as connect]
   [ts-broker.fake-connect :as fake-connect])
  (:import
   [java.net StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio.channels ServerSocketChannel]))

(deftest reads-connect-through-a-proxy-socket
  (let [dir (fs/create-temp-dir {:prefix "ts-broker-connect-test"})
        socket (str (fs/path dir "proxy.sock"))
        targets (atom [])
        proxy (server/run-server
               (fn [{:keys [uri] :as request}]
                 (swap! targets conj [(get-in request [:headers "host"]) uri])
                 (fake-connect/handler (assoc request :uri (str (.getPath (java.net.URI. uri))))))
               {:address-finder       (constantly (UnixDomainSocketAddress/of ^String socket))
                :channel-factory      (fn [_] (ServerSocketChannel/open StandardProtocolFamily/UNIX))
                :legacy-return-value? false})]
    (try
      (let [client (connect/client "http://op-connect.tail-scale.ts.net:8080" fake-connect/token
                                   {:proxy-socket socket})]
        (is (= ["Agents" "Infra"] (mapv :name (connect/vaults client))))
        (is (= [["op-connect.tail-scale.ts.net:8080" "http://op-connect.tail-scale.ts.net:8080/v1/vaults"]]
               @targets)
            "the proxy gets the request in absolute form for the Connect host"))
      (finally
        @(server/server-stop! proxy {:timeout 100})
        (fs/delete-tree dir)))))
