{:user {:plugins      [[cider/cider-nrepl "0.7.0-SNAPSHOT"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]]
        :injections   [(require 'pjstadig.humane-test-output)
                       (pjstadig.humane-test-output/activate!)]}}
