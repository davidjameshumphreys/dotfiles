{:user {:plugins [[cider/cider-nrepl "0.8.0-SNAPSHOT"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [clojure-complete "0.2.3"]
                       [criterium "0.4.3"]]
        :injections   [(require 'pjstadig.humane-test-output)
                       (pjstadig.humane-test-output/activate!)]}}
