{:user {:plugins      [[cider/cider-nrepl "0.12.0"
                        :exclusions [org.clojure/clojure
                                     org.clojure/tools.nrepl]]
                       [refactor-nrepl "2.2.0"]
                       [lein-pprint "1.1.2"]
                       [lein-try "0.4.3"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [criterium "0.4.3"]
                       [org.clojure/tools.nrepl
                        "0.2.12" :exclusions [org.clojure/clojure]]]
        :injections   [(require 'pjstadig.humane-test-output)
                       (pjstadig.humane-test-output/activate!)]}}
