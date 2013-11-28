{:user
 ;; https://github.com/technomancy/leiningen/wiki/Plugins
 {:plugins [[lein-ancient "0.5.4"]
            [lein-ritz "0.7.0"]]
  :dependencies [[nrepl-inspect "0.3.0"]
                 [clojure-complete "0.2.3"]
                 [ritz/ritz-nrepl-middleware "0.7.0"]]
  :repl-options {:nrepl-middleware
                 [inspector.middleware/wrap-inspect
                  ritz.nrepl.middleware.javadoc/wrap-javadoc
                  ritz.nrepl.middleware.apropos/wrap-apropos]}}}
