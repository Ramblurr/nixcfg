(ns ts-broker.socket
  "The decide server: newline-delimited JSON over a unix socket.

  Each line holds one request object and gets one response line. A
  connection may carry several requests in sequence. A line must arrive
  whole within the read timeout, or the connection closes. A line longer
  than the limit, a line that holds more than one JSON value, a last line
  without its newline, or a request that
  [[ts-broker.domain/parse-request]] refuses is answered `malformed`
  without an audit row. An over-long or unterminated line also closes the
  connection. Connections beyond the limit close at once."
  (:require
   [cheshire.core :as json]
   [ts-broker.domain :as domain]
   [ts-broker.store :as store])
  (:import
   [java.io BufferedInputStream ByteArrayOutputStream InputStream OutputStream StringReader]
   [java.net StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio.channels Channels ServerSocketChannel SocketChannel]
   [java.nio.charset StandardCharsets]
   [java.nio.file Files Path]
   [java.nio.file.attribute PosixFilePermissions]
   [java.util.concurrent Executors ScheduledExecutorService Semaphore TimeUnit]))

(def ^:private malformed {:decision "deny" :reason "malformed"})

(defn- read-line!
  "Returns the next line of `in` as a string without its newline, nil at
  the end of the stream, `::unterminated` when the stream ends inside a
  line, or `::too-long` once the line passes `limit` bytes."
  [^InputStream in limit]
  (let [buf (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (cond
          (= -1 b) (when (pos? (.size buf)) ::unterminated)
          (= 10 b) (.toString buf StandardCharsets/UTF_8)
          (>= (.size buf) limit) ::too-long
          :else (do (.write buf b) (recur)))))))

(defn- parse [line]
  (domain/parse-request
   (try
     (let [[m & more] (json/parsed-seq (StringReader. line) true)]
       (when (empty? more) m))
     (catch Exception _ nil))))

(defn- log! [& message]
  (binding [*out* *err*]
    (apply println "decide:" message)))

(defn- answer! [^OutputStream out response]
  (.write out (.getBytes (str (json/generate-string response) "\n") StandardCharsets/UTF_8))
  (.flush out))

(defn- serve-connection
  "Answers each request line on `ch` until the peer closes it. Closes `ch`
  without an answer when the store fails, so the egress fails closed."
  [{:keys [store ^ScheduledExecutorService timer read-timeout-ms max-line]} ^SocketChannel ch]
  (with-open [ch ch
              in (BufferedInputStream. (Channels/newInputStream ch))
              out (Channels/newOutputStream ch)]
    (try
      (loop []
        (let [deadline (.schedule timer ^Runnable #(.close ch) (long read-timeout-ms) TimeUnit/MILLISECONDS)
              line (read-line! in max-line)]
          (.cancel deadline false)
          (cond
            (nil? line) nil
            (= ::too-long line) (do (log! "malformed request: line over" max-line "bytes")
                                    (answer! out malformed))
            (= ::unterminated line) (do (log! "malformed request: no newline at the end of the stream")
                                        (answer! out malformed))
            :else (do (if-let [request (parse line)]
                        (answer! out (store/write! store {:command :decide :request request}))
                        (do (log! "malformed request of" (count line) "characters")
                            (answer! out malformed)))
                      (recur)))))
      (catch Exception e
        (log! "connection closed:" (ex-message e))))))

(defn- accept-loop [{:keys [^Semaphore slots] :as server} ^ServerSocketChannel channel]
  (loop []
    (when-let [ch (try (.accept channel) (catch Exception _ nil))]
      (if (.tryAcquire slots)
        (future
          (try
            (serve-connection server ch)
            (finally
              (.release slots))))
        (do (log! "refused a connection: too many open")
            (.close ^SocketChannel ch)))
      (recur))))

(defn start!
  "Listens on the unix socket at `path` and answers decide requests from
  `store`. Replaces a stale socket file. The socket file gets mode 0660 and
  the process's group. Returns a server for [[stop!]].

  Options:

  | key                | description |
  |--------------------|-------------|
  | `:read-timeout-ms` | Time for one request line to arrive (default 5000) |
  | `:max-line`        | Longest request line in bytes (default 65536) |
  | `:max-connections` | Connections served at once (default 32) |"
  [store path {:keys [read-timeout-ms max-line max-connections]
               :or   {read-timeout-ms 5000 max-line 65536 max-connections 32}}]
  (let [p (Path/of path (into-array String []))
        _ (Files/deleteIfExists p)
        channel (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                  (.bind (UnixDomainSocketAddress/of p)))
        server {:store           store
                :timer           (Executors/newSingleThreadScheduledExecutor)
                :slots           (Semaphore. max-connections)
                :read-timeout-ms read-timeout-ms
                :max-line        max-line}]
    (Files/setPosixFilePermissions p (PosixFilePermissions/fromString "rw-rw----"))
    (assoc server
           :channel channel
           :path    p
           :thread  (doto (Thread. #(accept-loop server channel) "ts-broker-decide")
                      (.setDaemon true)
                      (.start)))))

(defn stop!
  "Closes the decide server and removes its socket file."
  [{:keys [^ServerSocketChannel channel ^Path path ^ScheduledExecutorService timer]}]
  (.close channel)
  (.shutdownNow timer)
  (Files/deleteIfExists path)
  nil)
