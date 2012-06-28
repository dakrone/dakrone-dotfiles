(defproject kibit-mode "0.0.1"
  :description "A kibit compiler for Clojure files"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [jonase/kibit "0.0.3"]]
  :profiles {:dev {:resource-paths ["bogus_src"]
                   :dependencies [[midje "1.4.0"]
                                  [lein-midje "2.0.0-SNAPSHOT"]]}}
  :eval-in :leiningen
  )
