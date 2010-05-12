;(use 'clojure.contrib.repl-utils)

(defn class-methods [x]
  (let [c (if (class? x) x (class x))]
    (distinct (sort (seq 
                      (map #(.getName %1)
                           (.getMethods c)))))))

;; http://gist.github.com/252421
;; Inspired by George Jahad's version: http://georgejahad.com/clojure/debug-repl.html
(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (map key @clojure.lang.Compiler/LOCAL_ENV)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))
 
(declare *locals*)
(defn eval-with-locals
  "Evals a form with given locals. The locals should be a map of symbols to
values."
  [locals form]
  (binding [*locals* locals]
    (eval
     `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
        ~form))))
 
(defmacro debug-repl
  "Starts a REPL with the local bindings available."
  []
  `(clojure.main/repl
    :prompt #(print "dr => ")
    :eval (partial eval-with-locals (local-bindings))))
