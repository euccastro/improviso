(ns improviso.core
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! put! chan)]
            [improviso.gl :as gl]
            [improviso.sente :as sente]
            [rum.core :as rum]
            [taoensso.sente :refer (cb-success?)]))

(enable-console-print!)

(rum/defc hello [x]
  [:div (str "Hello, " x "!")])

(defn ^:export main []
  (enable-console-print!)
  (println "Hello!")
  (sente/start-once!)
  (rum/mount (gl/canvas)
             (js/document.getElementById "app_container")))
