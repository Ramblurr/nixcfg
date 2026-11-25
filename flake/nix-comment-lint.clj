(ns nix-comment-lint
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as p]
   [cheshire.core :as json]
   [clojure.string :as str]))

(def ^:private vendor-exclude ":!pkgs/ts-egress/vendor/*")

(def ^:private ast-grep-rule
  (str/join "\n"
            ["id: no-nix-comments"
             "language: nix"
             "rule:"
             "  kind: comment"]))

(def ^:private cli-spec
  {:base {:ref "<ref>"
          :desc "Check lines added since <ref> plus untracked .nix files (default: HEAD)"}
   :staged {:coerce :boolean
            :desc "Check only staged added lines"}
   :all {:coerce :boolean
         :desc "Check all tracked and untracked .nix files in the repo"}
   :help {:alias :h
          :coerce :boolean
          :desc "Show this help"}})

(defn- usage []
  (str "Usage: nix-comment-lint [OPTIONS] [PATH...]\n\n"
       "Report Nix comments (# and /* ... */) using ast-grep.\n\n"
       "Options:\n"
       (cli/format-opts {:spec cli-spec :order [:base :staged :all :help]}) "\n\n"
       "Examples:\n"
       "  nix-comment-lint\n"
       "  nix-comment-lint --staged\n"
       "  nix-comment-lint --base origin/main\n"
       "  nix-comment-lint --all\n"
       "  nix-comment-lint flake.nix hosts/quine\n"))

(defn- sh-out [dir args]
  (let [{:keys [exit out err]} (apply p/shell {:dir dir :out :string :err :string :continue true} args)]
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println (str "error: command failed (" exit "): " (str/join " " args)))
        (when-not (str/blank? err)
          (print err))
        (flush))
      (System/exit 2))
    out))

(defn parse-diff-added-lines
  "Parse unified=0 git diff output into a set of [file line] pairs (1-indexed)."
  [diff-text]
  (loop [lines (str/split-lines diff-text)
         current-file nil
         acc (transient #{})]
    (if-let [line (first lines)]
      (cond
        (str/starts-with? line "+++ b/")
        (recur (rest lines) (subs line 6) acc)

        (and current-file (str/starts-with? line "@@ "))
        (if-let [[_ start-s count-s] (re-find #" \+(\d+)(?:,(\d+))? @@" line)]
          (let [start (parse-long start-s)
                cnt (if count-s (parse-long count-s) 1)
                acc' (reduce (fn [s ln] (conj! s [current-file ln]))
                             acc
                             (range start (+ start cnt)))]
            (recur (rest lines) current-file acc'))
          (recur (rest lines) current-file acc))

        :else
        (recur (rest lines) current-file acc))
      (persistent! acc))))

(defn- git-root []
  (str/trim (sh-out "." ["git" "rev-parse" "--show-toplevel"])))

(defn- git-nix-files [root mode]
  (let [flags (case mode
                :all ["--cached" "--others" "--exclude-standard"]
                :untracked ["--others" "--exclude-standard"])
        raw (sh-out root (into ["git" "ls-files" "-z"]
                               (concat flags ["--"] ["*.nix" vendor-exclude])))]
    (->> (str/split raw #"\u0000")
         (remove str/blank?)
         (filter #(fs/regular-file? (fs/path root %)))
         vec)))

(defn- untracked-added-lines [root files]
  (into #{}
        (mapcat (fn [file]
                  (let [n (count (str/split-lines (slurp (str (fs/path root file)))))]
                    (map (fn [ln] [file ln]) (range 1 (inc n))))))
        files))

(defn- scan-files [dir files]
  (if (empty? files)
    []
    (let [args (into ["ast-grep" "scan" "--inline-rules" ast-grep-rule "--json=stream" "--"] files)
          {:keys [exit out err]} (apply p/shell {:dir dir :out :string :err :string :continue true} args)]
      (when-not (contains? #{0 1} exit)
        (binding [*out* *err*]
          (println (str "error: ast-grep failed (" exit ")"))
          (when-not (str/blank? err)
            (print err))
          (flush))
        (System/exit 2))
      (->> (str/split-lines out)
           (remove str/blank?)
           (map #(json/parse-string % true))
           vec))))

(defn- match->findings [{:keys [file text] match-range :range}]
  (let [start-ln (inc (get-in match-range [:start :line]))
        end-ln (inc (get-in match-range [:end :line]))
        first-line (first (str/split-lines text))]
    (map (fn [ln] [file ln first-line]) (range start-ln (inc end-ln)))))

(defn- report-findings! [findings]
  (let [sorted (->> findings distinct (sort-by (juxt first second)))]
    (if (seq sorted)
      (do
        (doseq [[file ln text] sorted]
          (println (str file ":" ln ": " text)))
        1)
      0)))

(defn- run-diff-mode [root {:keys [staged base]}]
  (let [diff-args (cond-> ["git" "diff" "--no-ext-diff" "--no-color" "--unified=0" "--diff-filter=AM"]
                    staged (conj "--cached")
                    (not staged) (conj (or base "HEAD"))
                    true (into ["--" "*.nix" vendor-exclude]))
        diff-added (parse-diff-added-lines (sh-out root diff-args))
        untracked-files (if staged [] (git-nix-files root :untracked))
        added-set (into diff-added (untracked-added-lines root untracked-files))
        files (->> added-set (map first) distinct sort vec)
        findings (->> (scan-files root files)
                      (mapcat match->findings)
                      (filter (fn [[file ln _]] (contains? added-set [file ln]))))]
    (report-findings! findings)))

(defn -main [& raw-args]
  (let [{:keys [opts args]}
        (try
          (cli/parse-args raw-args {:spec cli-spec :restrict true})
          (catch clojure.lang.ExceptionInfo e
            (binding [*out* *err*]
              (println (str "error: " (ex-message e)))
              (println)
              (print (usage))
              (flush))
            (System/exit 2)))]
    (when (:help opts)
      (print (usage))
      (flush)
      (System/exit 0))
    (let [mode-count (count (filter identity [(:staged opts) (:all opts) (:base opts) (seq args)]))]
      (when (> mode-count 1)
        (binding [*out* *err*]
          (println "error: --base, --staged, --all, and PATH arguments are mutually exclusive")
          (println)
          (print (usage))
          (flush))
        (System/exit 2)))
    (let [exit-code (cond
                      (seq args)
                      (->> (scan-files "." args)
                           (mapcat match->findings)
                           report-findings!)

                      (:all opts)
                      (let [root (git-root)
                            files (git-nix-files root :all)]
                        (->> (scan-files root files)
                             (mapcat match->findings)
                             report-findings!))

                      :else
                      (run-diff-mode (git-root) opts))]
      (System/exit exit-code))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
