{:user
 {:plugins [[codox "0.6.7"]
            [cider/cider-nrepl "0.8.2"]
            [jonase/eastwood "0.2.0"]
            [lein-bikeshed "0.2.0"]
            [lein-clojuredocs "1.0.2"]
            [lein-difftest "2.0.0"]
            [lein-marginalia "0.7.1"]
            [lein-pprint "1.1.1"]
            [lein-ritz "0.7.0"]
            [slamhound "1.3.1"]
            [lein-immutant "2.0.0-alpha2"]
            [lein-ancient "0.6.0"]
            [lein-vanity "0.2.0"]]
  :dependencies [[pjstadig/humane-test-output "0.6.0"]]
  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]}
 :with-gc {:jvm-opts ["-Xloggc:garbage.log"
                      "-XX:+PrintGCDetails"
                      "-XX:+PrintGCDateStamps"]}}
