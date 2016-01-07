{:user {:plugins      [[cider/cider-nrepl "0.10.1"
                        :exclusions [org.clojure/clojure
                                     org.clojure/tools.nrepl]]
                       [lein-hiera "0.9.0"]
                       [refactor-nrepl "1.1.0"]
                       [lein-pprint "1.1.2"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [criterium "0.4.3"]
                       [org.clojure/tools.nrepl
                        "0.2.12" :exclusions [org.clojure/clojure]]]
        :injections   [(require 'pjstadig.humane-test-output)
                       (pjstadig.humane-test-output/activate!)]}}
