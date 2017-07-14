;;; Compojure and Sente routing.

(ns improviso.core
  (:gen-class)
  (:require [compojure.core :refer (defroutes GET POST)]
            [compojure.route :refer (files not-found resources)]
            [improviso.sente :as sente]
            [improviso.terrain :as terrain]
            [improviso.util :as util]
            [ring.middleware.defaults :refer (wrap-defaults site-defaults)]
            [rum.core :as rum]))

(defn root [req]
  {:status 200
   :headers {"content-type" "text/html"}
   :body (rum/render-html
          [:html
           [:head [:title "Olá mundo!"]]
           [:body {:style {:border 0 :padding 0 :margin 0}}
            [:div#app_container {:style {:border 0 :padding 0 :margin 0}}
             "Wonderful things would happen here if you had Javascript enabled..."]]
           [:script {:type "text/javascript" :src "js/main.js"}]
           [:script {:type "text/javascript"} "improviso.core.main();"]])})

(defmethod sente/client-msg-handler :terrain/make-map
  [{:keys [?data ?reply-fn]}]
  (when ?reply-fn (?reply-fn (terrain/make-map))))

(defroutes handler
  (GET "/" req (root req))
  ;; sente
  (GET  "/chsk" req sente/ring-ajax-get-or-ws-handshake)
  (POST "/chsk" req sente/ring-ajax-post)
  (resources (if util/in-development? "/public" "/"))
  (files "/")
  (not-found "Page not found."))

(def app
  (wrap-defaults handler site-defaults))

(if util/in-development?
  (sente/start-router!))

(defn -main []
  (sente/start-router!))
