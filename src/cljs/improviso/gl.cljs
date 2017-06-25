(ns improviso.gl
  (:require [rum.core :as rum]))

(enable-console-print!)

(defn window-size []
  [(.-innerWidth js/window)
   (.-innerHeight js/window)])

(rum/defc canvas < (rum/local (window-size) ::window-size)
                   {:init (fn [state props]
                            (println "init" state props)
                            state)
                    :will-mount (fn [state]
                                  (println "will mount" state)
                                  state)
                    :before-render (fn [state]
                                     (println "before render" state)
                                     state)
                    :wrap-render (fn [render]
                                   (println "wrapping render")
                                   (fn [& args]
                                     (println "rendering" args)
                                     (apply render args)))
                    :did-mount (fn [state]
                                 (println "did mount" state)
                                 (comment (reset! (::window-size state)
                                                  (window-size)))
                                 (.addEventListener js/window
                                                    "resize"
                                                    (fn []
                                                      (println "resizing to" (window-size))
                                                      (reset! (::window-size state)
                                                              (window-size))))
                                 state)
                    :after-render (fn [state]
                                    (println "after render" state)
                                    state)
                    :did-remount (fn [old-state state]
                                   (println "did remount" old-state state)
                                   state)
                    :will-update (fn [state]
                                   (println "will update" state)
                                   state)
                    :did-update (fn [state]
                                  (println "did update" state)
                                  state)
                    :will-unmount (fn [state]
                                    (println "will unmount" state)
                                    state)}
  []
  [:canvas {:style {:width (.-innerWidth js/window)
                    :height (.-innerHeight js/window)
                    :padding "0"
                    :margin "0"
                    :border "0"
                    :background-color "#ff00ff"}}])

