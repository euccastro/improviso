(ns improviso.core
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! put! chan)]
            [datascript.core :as d]
            [improviso.gl :refer (canvas)]
            [improviso.hex :as hex]
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

(defn init-map [conn]
  (d/transact! conn
               [{:db/id -1
                 :map/cols 2
                 :map/rows 2
                 :map/hexes [{:hex/x 0 :hex/y 0 :hex/z 0 :hex/color [1.0 0.0 0.0 1.0]}
                             {:hex/x 1 :hex/y -1 :hex/z 0 :hex/color [0.0 1.0 0.0 1.0]}
                             {:hex/x 1 :hex/y 0 :hex/z -1 :hex/color [0.0 0.0 1.0 1.0]}
                             {:hex/x 2 :hex/y -1 :hex/z -1 :hex/color [1.0 1.0 0.0 1.0]}]}]))

(defonce conn
  (let [conn- (d/create-conn {:map/hexes {:db/cardinality :db.cardinality/many
                                          :db/valueType :db.type/ref
                                          :db/isComponent true}})]
    (init-map conn-)
    conn-))

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
        map-id (d/q '[:find ?m . :where [?m :map/cols]] @conn)
        map-ent (d/entity @conn map-id)
        hradii (hex/map-width (:map/cols map-ent))
        vradii (hex/map-height (:map/rows map-ent))
        radius (min (/ w hradii) (/ h vradii))
        map-w (* radius hradii)
        map-h (* radius vradii)
        gl (gl/gl-context dom-node)
        shader (shader/make-shader-from-spec gl line-shader)
        proj (gl/ortho 0 0 w h -1 1)
        model-txn (-> M44
                      (geom/translate (vec3 (/ w 2) (/ h 2) 0))
                      (math/* (geom/scale M44 (vec3 (/  radius 2) (/ radius 2) 1))))
        wf hex/width-factor
        model (-> (line/linestrip2 [0 1]
                                   [wf 0.5]
                                   [wf -0.5]
                                   [0 -1]
                                   [(- wf) -0.5]
                                   [(- wf) 0.5]
                                   [0 1])
                  (gl/as-gl-buffer-spec {})
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (assoc :shader shader))]
    (println "drawing with size" w h map-id map-ent (:map/rows map-ent) radius)
    (gl/clear-color-and-depth-buffer gl 0.3 0.3 0.3 1 1)
    (gl/set-viewport gl 0 0 w h)
    (doseq [{:keys [hex/x hex/y hex/z hex/color]}
            (:map/hexes (d/pull @conn '[{:map/hexes [*]}] map-id))]
      (do
        (println "entry" x y z color hex/x-unit hex/y-unit hex/z-unit)
        (gl/draw-with-shader gl
                             (-> model
                                 (assoc-in [:uniforms :color] color)
                                 (assoc-in [:uniforms :txn]
                                           (math/*
                                            proj
                                            (geom/translate
                                             model-txn
                                             (math/+
                                              (math/* hex/x-unit x)
                                              (math/+
                                               (math/* hex/y-unit y)
                                               (math/* hex/z-unit z))))))))))))

(defn ^:export main []
  (enable-console-print!)
  (println "Hello!")
  (sente/start-once!)
  (rum/mount (canvas {:after-render (fn [& args] (apply draw args))})
             (js/document.getElementById "app_container")))
