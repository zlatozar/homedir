{:user
 {:plugins [[lein-ritz "0.7.0"]]
  :dependencies [[ritz/ritz-nrepl-middleware "0.7.0"]
                 [clojure-complete "0.2.3"]]
  :repl-options {:nrepl-middleware
                 [ritz.nrepl.middleware.javadoc/wrap-javadoc
                  ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}}}
