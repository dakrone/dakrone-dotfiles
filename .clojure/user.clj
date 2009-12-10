(defn class-methods [x]
  (let [c (if (class? x) x (class x))]
    (distinct (sort (seq 
                      (map #(.getName %1)
                           (.getMethods c)))))))

