(ns kibit-mode.core
  (:require [kibit.check :as c]
            [clojure.java.io :as io]
            [clojure.string :as s]))

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
