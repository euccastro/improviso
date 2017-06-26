(ns improviso.gl
  (:require [rum.core :as rum]))

(enable-console-print!)

(defn window-size []
  [(.-innerWidth js/window)
   (.-innerHeight js/window)])

(rum/defc canvas <
  (rum/local (window-size) ::window-size)
  rum/static
  {
   ;; :init (fn [state props]
   ;;         (println "init" state props)
   ;;         state)
   ;; :will-mount (fn [state]
   ;;               (println "will mount" state)
   ;;               state)
   :before-render (fn [state]
                    (println "before render")
                    state)
   ;; :wrap-render (fn [render]
   ;;                (println "wrapping render")
   ;;                (fn [& args]
   ;;                  (println "rendering" args)
   ;;                  (apply render args)))
   :did-mount (fn [{[{:keys [on-resize]}] :rum/args :as state}]
                (let [on-resize (fn []
                                  (let [[w h] (window-size)]
                                    (println "resizing to" w h)
                                    (reset! (::window-size state)
                                            [w h])
                                    (when on-resize
                                      (on-resize w h (rum/dom-node state)))))]
                  (.addEventListener js/window
                                     "resize"
                                     on-resize)
                  (assoc state ::on-resize on-resize)))
   :after-render (fn [{[{:keys [after-render]}] :rum/args :as state}]
                   (when after-render (after-render (rum/dom-node state)))
                   state)
   ;; :did-remount (fn [old-state state]
   ;;                (println "did remount" old-state state)
   ;;                state)
   :will-update (fn [state]
                  (println "will update")
                  state)
   :did-update (fn [state]
                 (println "did update")
                 state)
   :will-unmount (fn [state]
                   (println "will unmount")
                   (when-let [on-resize (::on-resize state)]
                     (.removeEventListener js/window "resize" on-resize))
                   (dissoc state ::on-resize))
   }
  [handlers]
  (println "rendering")
  (let [[w h] (window-size)]
    [:canvas
     {:width w
      :height h
      :style {:width w
              :height h
              :padding "0"
              :margin "0"
              :border "0"
              :background-color "#ff00ff"}}]))

