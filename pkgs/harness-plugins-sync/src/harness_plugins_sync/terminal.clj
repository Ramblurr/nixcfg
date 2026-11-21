(ns harness-plugins-sync.terminal
  "Colors, status messages and a progress spinner for CLI applications.

  Messages and progress go to stderr. [[style]] decorates text for stdout.
  `harness-plugins-sync.cli/run` binds [[*terminal*]] for each invocation, so
  application code needs no checks for pipes, `NO_COLOR`, `TERM=dumb` or
  `--json`/`--edn` modes. sugar picks colors for the terminal's color level and
  background and turns them off for pipes, `NO_COLOR` and `TERM=dumb`."
  (:require
   [sugar.core :as sugar]
   [supports-color.core :as supports-color]))

(def ^:private tty-err?
  ;; Only the spinner needs this: NO_COLOR removes color, not animation.
  ;; Neither the JVM nor Babashka can test one descriptor, so ask a POSIX shell.
  (delay
    (try
      (-> (ProcessBuilder. ^java.util.List ["sh" "-c" "[ -t 2 ]"])
          (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)
          .start .waitFor zero?)
      (catch Exception _ false))))

(defn detect
  "Returns terminal capabilities: `:color-out?`, `:color-err?` and `:animate?`.

  `structured?` is true in `--json`/`--edn` modes, which turn all three off.
  Otherwise sugar decides stdout color, supports-color decides stderr color, and
  the spinner runs when stderr is a terminal and `TERM` is not `dumb`."
  [structured?]
  (if structured?
    {:color-out? false :color-err? false :animate? false}
    {:color-out? true
     :color-err? (pos? (:level (supports-color/profile {:stream :stderr})))
     :animate?   (and (not= "dumb" (System/getenv "TERM")) @tty-err?)}))

(def ^:dynamic *terminal*
  "Terminal capabilities from [[detect]]. Nil outside `harness-plugins-sync.cli/run`,
  where functions detect them from the process environment."
  nil)

(defn- terminal []
  (or *terminal* (detect false)))

(def ^:private roles
  {:ok :green :warn :yellow :error :red :info :blue :dim :gray :bold :bold :heading :bold.blue})

(defn- paint
  "Returns `text` styled for a role or any sugar style unless `color?` is false.
  sugar checks only stdout, so stderr text is plain when stdout is redirected."
  [color? role-or-style text]
  (if color?
    (sugar/styled (str text) (roles role-or-style role-or-style))
    (str text)))

(defn style
  "Returns `text` decorated when stdout supports color.

  `role` is `:ok`, `:warn`, `:error`, `:info`, `:dim`, `:bold` or `:heading`, or
  any sugar style such as `:bold.magenta` or `{:color \"#ff5f00\"}`. Keep
  meaning in the words; color only aids scanning."
  [role text]
  (paint (:color-out? (terminal)) role text))

(def ^:private lock (Object.))

(defonce ^:private !active (atom nil))

(def ^:private erase-line "\r\u001b[2K")
(def ^:private hide-cursor "\u001b[?25l")
(def ^:private show-cursor "\u001b[?25h")

(defn- write-err! [^String s]
  (locking lock
    (.write *err* s)
    (.flush *err*)))

(defn- message! [role label text]
  (let [color? (:color-err? (terminal))
        line   (str (when label (str (paint color? role label) " ")) text "\n")]
    (locking lock
      (when @!active (.write *err* ^String erase-line))
      (.write *err* ^String line)
      (.flush *err*))))

(defn info "Prints an informational message to stderr." [text] (message! :info nil text))
(defn success "Prints `ok: text` to stderr." [text] (message! :ok "ok:" text))
(defn warn "Prints `warning: text` to stderr." [text] (message! :warn "warning:" text))
(defn error "Prints `error: text` to stderr." [text] (message! :error "error:" text))

;; Dots frames from cli-spinners, MIT License,
;; Copyright (c) Sindre Sorhus <sindresorhus@gmail.com>. See NOTICE.
(def ^:private frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(def ^:private restore-on-exit
  (delay
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread. ^Runnable
              (fn []
                (when-let [state @!active]
                  (locking lock
                    (.print System/err (str erase-line (:title @state) ": interrupted\n" show-cursor))
                    (.flush System/err))))))))

(defn- spinner-thread [state ^java.io.Writer err]
  (doto (Thread.
         ^Runnable
         (fn []
           (try
             (loop [i 0]
               (when (:running? @state)
                 (locking lock
                   (.write err (str erase-line (paint (:color? @state) :info (frames i))
                                    " " (:label @state)))
                   (.flush err))
                 (Thread/sleep 80)
                 (recur (mod (inc i) (count frames)))))
             (catch InterruptedException _ nil))))
    (.setDaemon true)
    (.start)))

(def ^:dynamic ^:private *progress*
  "State atom for the innermost [[with-progress]] in this thread."
  nil)

(defn step
  "Reports a new phase of the current [[with-progress]] operation.
  Updates the spinner label, or prints a plain line when not animated."
  [text]
  (if (and *progress* (= *progress* @!active))
    (swap! *progress* assoc :label text)
    (info text)))

(defn- elapsed [start]
  (format "%.1fs" (/ (- (System/nanoTime) start) 1e9)))

(defn call-with-progress
  "Calls `f` while showing `label` as progress on stderr. Returns or throws as `f` does.

  Animates a spinner only for one operation at a time on an interactive stderr.
  Nested or concurrent calls, pipes, `TERM=dumb` and structured modes print plain
  lines instead. Stops the spinner and restores the cursor before returning."
  [label f]
  (let [{:keys [animate? color-err?]} (terminal)
        state (atom {:title label :label label :running? true :color? color-err?})
        start (System/nanoTime)
        own?  (and animate? (compare-and-set! !active nil state))]
    (if-not own?
      (do (info (str label "..."))
          (binding [*progress* state]
            (let [result (try (f)
                              (catch Throwable t
                                (message! :error nil (str label ": failed"))
                                (throw t)))]
              (message! :ok nil (str label ": done (" (elapsed start) ")"))
              result)))
      (let [_      @restore-on-exit
            _      (write-err! hide-cursor)
            thread (spinner-thread state *err*)
            stop!  (fn [role text]
                     (swap! state assoc :running? false)
                     (.interrupt ^Thread thread)
                     (.join ^Thread thread 1000)
                     (locking lock
                       (reset! !active nil)
                       (.write *err* (str erase-line (paint color-err? role text) "\n" show-cursor))
                       (.flush *err*)))]
        (try
          (let [result (binding [*progress* state] (f))]
            (stop! :ok (str "✓ " label ": done (" (elapsed start) ")"))
            result)
          (catch Throwable t
            (stop! :error (str "✗ " label ": failed"))
            (throw t))
          (finally
            (when (= state @!active)
              (reset! !active nil)
              (write-err! show-cursor))))))))

(defmacro with-progress
  "Evaluates `body` while showing `label` as progress. See [[call-with-progress]]."
  [label & body]
  `(call-with-progress ~label (fn [] ~@body)))
