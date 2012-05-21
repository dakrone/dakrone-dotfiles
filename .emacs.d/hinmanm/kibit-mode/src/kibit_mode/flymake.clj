(ns kibit-mode.flymake
  (:require [kibit-mode.core :as c]))

(defn report-flymake-error
  "Given a kibit simplification, print the line number and normalized
  form of the expr and the replacement"
  [file {:keys [expr alt line] :as simplify-map}]
  (println (str file
                ":"
                line
                ": ERROR: "
                (pr-str expr)
                " CORRECTION: "
                (pr-str alt))))

(defn -main
  [file]
  (c/check-file file report-flymake-error)
  (System/exit 0))
