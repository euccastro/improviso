(ns improviso.core
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! put! chan)]
            [improviso.gl :refer (canvas)]
            [improviso.sente :as sente]
            [rum.core :as rum]
            [taoensso.sente :refer (cb-success?)]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.shaders :as shader]
            [thi.ng.geom.gl.shaders.basic :refer (make-shader-spec-2d)]
            [thi.ng.geom.gl.webgl.constants :as glc]
            [thi.ng.geom.line :as line]
            [thi.ng.geom.matrix :as mat :refer (M44)]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as vec :refer (vec3)]
            [thi.ng.math.core :as math]))

(enable-console-print!)

(def line-shader
  {:vs "
void main() {
  gl_Position=txn*vec4(position, 0.0, 1.0);
}"
   :fs "
void main() {
  gl_FragColor=color;
}
"
   :uniforms {:txn [:mat4 M44]
              :color :vec4}
   :attribs {:position :vec2}})

(defn draw
  [dom-node]
  (println "drawing on dom node" dom-node)
  (let [w (.-clientWidth dom-node)
        h (.-clientHeight dom-node)
        gl (gl/gl-context dom-node)
        shader (shader/make-shader-from-spec gl line-shader)
        proj (gl/ortho 0 0 w h -1 1)
        cx (/ w 2)
        cy (/ h 2)
        model (-> (line/linestrip2 [(- cx 10) (- cy 10)]
                                   [(+ cx 20) (+ cy 50)])
                  (gl/as-gl-buffer-spec {})
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (assoc :shader shader)
                  (update-in [:uniforms] merge {:color [0.9 0.4 0.1 1]}))]
    (println "drawing with size" w h)
    (gl/clear-color-and-depth-buffer gl 0.3 0.8 0.3 1 1)
    (gl/set-viewport gl 0 0 w h)
    (gl/draw-with-shader gl
                         (assoc-in model [:uniforms :txn] proj))))

(defn ^:export main []
  (enable-console-print!)
  (println "Hello!")
  (sente/start-once!)
  (rum/mount (canvas {:after-render (fn [& args] (apply draw args))})
             (js/document.getElementById "app_container")))
