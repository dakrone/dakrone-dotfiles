(ns kibit-mode.core
  (:require [kibit.check :as c]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn detailed-read
  "Read a form from DetailedFormReader reader,
  attaching the following metadata to the form:

  :line - the line of the stream the form was found on
  :end-line - the last line of the stream containing the form
  :start-character - the first byte which describes the form
  :end-character - the last byte which describes the form

  Accepts all of the same arguments and arities as clojure.core/read."
  ([reader] (detailed-read reader true nil))
  ([reader eof-error?] (detailed-read reader eof-error? nil))
  ([reader eof-error? eof-value] (detailed-read reader eof-error? eof-value false))
  ([reader eof-error? eof-value recursive?]
    (let [form (clojure.core/read reader eof-error? eof-value recursive?)
          {start-line :line} (meta form)
          end-line (.getLineNumber reader)
          details (.resetStats reader)
          start-character (:start-character details)
          end-character (:current-character details)
          source (:source details)]
      (if (and (not eof-error?)
               (= form eof-value))
        eof-value
        (with-meta form {:line start-line
                         :end-line end-line
                         :start-character start-character
                         :end-character end-character
                         :source source})))))
(gen-interface
 :name kibit-mode.core.DetailedFormReader
 :methods [
           (resetStats [] clojure.lang.APersistentMap)
           (peekStats [] clojure.lang.APersistentMap)])

(defn whitespace?
  "Returns true if character is Clojure whitespace, false otherwise"
  [character]
  (try (let [character (char character)]
         (or (Character/isWhitespace character)
             (= character \,)))
       (catch Exception e
         false)))

(defn- read-update-fn
  "Update stats after reading the character result"
  [stats c]
  (let [new-stats (-> stats
                  (update-in [:total-read] inc)
                  (update-in [:start-character] (if (and (whitespace? c)
                                                         (:only-whitespace stats))
                                                  inc
                                                  identity))
                  (assoc-in [:only-whitespace] (and (:only-whitespace stats)
                                                    (whitespace? c))))
        character (try (char c) (catch Exception e nil))]
    (if (and character (not (:only-whitespace new-stats)))
      (update-in new-stats [:source-chars] conj character)
      new-stats)))

(defn- unread-update-fn
  "Update stats after unreading the character c"
  [stats c]
  (-> stats
      (update-in [:total-read] dec)
      (update-in [:start-character] (if (and (whitespace? c)
                                             (:only-whitespace stats))
                                      inc
                                      identity))
      (update-in [:source-chars] (if (:only-whitespace stats) identity rest))))

(defn- read-line-update-fn
  "Update stats after reading the line line"
  [stats line]
  (if line
    (-> stats
       (update-in [:total-read]
                  +
                  (inc (count line)))
       (update-in [:source-chars] concat (conj (reverse (seq line)) \newline))
       stats)))

(defn- reset-stats-update-fn
  "Reset stats based on the current state of reader"
  [stats reader]
  (-> stats
      (assoc-in [:start-character]
                (:total-read stats))
      (assoc-in [:only-whitespace]
                true)
      (assoc-in [:source-chars]
                '())))

(defn make-detailed-form-reader
  "Wraps LineNumberingPushbackReader with tracing data in order to
  enable detailed-read"
  [reader]
  (let [stats (atom {:only-whitespace true
                     :start-character 0
                     :total-read 0
                     :source-chars '()})]
    (proxy
        [clojure.lang.LineNumberingPushbackReader kibit-mode.core.DetailedFormReader]
        [reader]
      (getLineNumber [] (proxy-super getLineNumber))
      (read []
        (let [read-result (proxy-super read)]
          (swap! stats read-update-fn read-result)
          read-result))
      (unread [c]
        (proxy-super unread (int c))
        (swap! stats unread-update-fn (int c)))
      (readLine []
        (let [read-result (proxy-super readLine)]
          (when read-result
            (swap! stats read-line-update-fn read-result))
          read-result))
      (atLineStart []
        (proxy-super atLineStart))
      (resetStats []
        (let [peek (.peekStats this)]
          (swap! stats reset-stats-update-fn this)
          peek))
      (peekStats []
        {:current-line (.getLineNumber this)
         :start-character (:start-character @stats)
         :current-character (:total-read @stats)
         :source (apply str (reverse (:source-chars @stats)))}))))

(defn detailed-forms
  "Return a seq of all the forms from detailed-form-reader, including
  detailed metadata for :line, :end-line, :start-character,
  and :end-character"
  [detailed-form-reader]
  (let [read-result (detailed-read detailed-form-reader false :eof)]
    (lazy-seq
      (when-not (= read-result :eof)
        (cons read-result
              (detailed-forms detailed-form-reader))))))

(defn assoc-reporter
  "Given a kibit simplification map, print an emacs-lispy."
  [file {:keys [expr alt line] :as simplify-map}]
  (println (str "'((file . \""
                file
                "\") (line . "
                line
                ") (expr . \""
                (pr-str expr)
                "\") (replacement-exp . \""
                (pr-str alt)
                "\"))")))

(defn report-error
  "Given a kibit simplification, print the line number and normalized
  form of the expr and the replacement"
  [file {:keys [expr alt line] :as simplify-map}]
  (println (str file
                ":"
                line
                ":\n  Replace\n    "
                (pr-str expr)
                "\n  with\n    "
                (pr-str alt)
                )))

(defn check-file
  [file reporter]
  (with-open [reader (io/reader file)]
    (let [errors (c/check-reader reader)]
      (doseq [simplify-map errors]
        (reporter file simplify-map))
      errors)))

(defn -main
  [file]
  (when-not (empty? (check-file file report-error))
    (System/exit 1)))
