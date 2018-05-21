(ns user)

(require '[clojure.tools.nrepl.server :refer [default-handler start-server]])
(require '[cider.nrepl :refer [cider-middleware]])
(require '[refactor-nrepl.middleware :refer [wrap-refactor]])

(def server
  (start-server :port 5000
                :handler (apply
                          default-handler
                          (conj
                           (map resolve cider-middleware)
                           #'wrap-refactor))))
(println "Started server on port 5000")
