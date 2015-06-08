{:user {:plugins      [[cider/cider-nrepl "0.9.0-SNAPSHOT" :exclusions [org.clojure/clojure
                                                                        org.clojure/tools.nrepl]]
                       [lein-hiera "0.9.0"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [criterium "0.4.3"]]
        :injections   [(require 'pjstadig.humane-test-output)
                       (pjstadig.humane-test-output/activate!)]}}
