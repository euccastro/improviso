(ns improviso.core
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! put! chan)]
            [cljs.pprint :refer (pprint)]
            [datascript.core :as d]
            [improviso.gl :refer (canvas)]
            [improviso.hex :as hex]
            [improviso.sente :as sente]
            [rum.core :as rum]
            [taoensso.sente :refer (cb-success?)]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.gl.buffers :as buf]
            [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.shaders :as shader]
            [thi.ng.geom.gl.shaders.basic :refer (make-shader-spec-2d)]
            [thi.ng.geom.gl.webgl.constants :as glc]
            [thi.ng.geom.line :as line]
            [thi.ng.geom.matrix :as mat :refer (M44)]
            [thi.ng.geom.polygon :as poly]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as vec :refer (vec2 vec3)]
            [thi.ng.math.core :as math]))

(def wheel-scale-factor 0.001)

(enable-console-print!)

(defn init-map [conn map]
  (d/transact! conn [(merge {:db/id -1} map)]))

(defonce conn
  (d/create-conn {:map/hexes {:db/cardinality :db.cardinality/many
                              :db/valueType :db.type/ref
                              :db/isComponent true}}))

(def line-shader-spec
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

(def rect-shader-spec
  {:vs "
void main() {
  vUV = position;
  gl_Position = txn*vec4(position, 0.0, 1.0);
}"
   :fs "
void main() {
  //gl_FragColor = vec4(vUV, 1.0 - (vUV.x + vUV.y) / 2.0, 1.0);
  vec4 texcol = texture2D(tex, vUV);
  gl_FragColor = texcol; // vec4(texcol.r, texcol.g, texcol.b, vUV.x);
}
"
   :uniforms {:txn [:mat4 M44]
              :tex :sampler2D}
   :varying  {:vUV      :vec2}
   :attribs {:position :vec2}})

(defn on-resize [state]
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)
        proj (gl/ortho 0 0 w h -1 1)
        map-id (d/q '[:find ?m . :where [?m :map/cols]] @conn)
        map-ent (d/entity @conn map-id)
        hradii (hex/map-width (:map/cols map-ent))
        vradii (hex/map-height (:map/rows map-ent))
        radius (/ (min (/ w hradii) (/ h vradii)) 2)] ; XXX / 2
    (swap! (:user-data state)
           #(merge {:eye-pos (vec2 0 0)}
                   %
                   {:window-size (vec2 w h)
                    :projection proj
                    :radius-px radius
                    :map-id map-id}))))

(defn draw [state]
  (let [{:keys [window-size
                projection
                radius-px
                eye-pos
                map-id
                selected-hex]}
        @(:user-data state)
        dom-node (rum/dom-node state)
        gl (gl/gl-context dom-node {:alpha false})
        shader (shader/make-shader-from-spec gl line-shader-spec)
        rect-shader (shader/make-shader-from-spec gl rect-shader-spec)
        model-txn (-> M44
                      (geom/translate (math/* window-size 0.5))
                      (math/* (geom/scale M44 (vec3 radius-px radius-px 1)))
                      (geom/translate eye-pos))
        wf hex/width-factor
        rect-model (-> (rect/rect 0 0 1 1)
                       (gl/as-gl-buffer-spec {})
                       (gl/make-buffers-in-spec gl glc/static-draw)
                       (assoc :shader rect-shader))
        tex (or (.-tex gl)
                (let [tex-ready (atom false)
                      tex (buf/load-texture
                           gl {:callback (fn [tex img]
                                           (println "LOADED!!!")
                                           (reset! tex-ready true))
                               :src "img/wink.png"
                               :type (.-UNSIGNED_BYTE gl)
                               :format glc/rgba
                               :flip false})]
                  (set! (.-tex gl) tex)
                  (set! (.-texReady gl) tex-ready)
                  tex))
        model (-> (poly/polygon2 [0 1]
                                 [wf 0.5]
                                 [wf -0.5]
                                 [0 -1]
                                 [(- wf) -0.5]
                                 [(- wf) 0.5])
                  (gl/as-gl-buffer-spec {:normals false})
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (assoc :shader shader))]
    (.enable gl (.-BLEND gl))
    (.blendFunc gl (.-SRC_ALPHA gl) (.-ONE_MINUS_SRC_ALPHA gl))
    (gl/clear-color-and-depth-buffer gl 0.3 0.3 0.3 1.0 1)
    (gl/set-viewport gl 0 0 (:x window-size) (:y window-size))
    (doseq [{:keys [db/id hex/x hex/y hex/z hex/color]}
            (:map/hexes (d/pull @conn '[{:map/hexes [*]}] map-id))]
      (gl/draw-with-shader gl
                           (-> model
                               (assoc-in [:uniforms :color]
                                         (if (= id selected-hex)
                                           [1 1 1 1]
                                           color))
                               (assoc-in [:uniforms :txn]
                                         (math/*
                                          projection
                                          (geom/translate
                                           model-txn
                                           (math/+
                                            (math/* hex/x-unit x)
                                            (math/+
                                             (math/* hex/y-unit y)
                                             (math/* hex/z-unit z))))))))
      (when @(.-texReady gl)
        (gl/draw-with-shader gl
                             (-> rect-model
                                 (assoc-in [:uniforms :tex] tex)
                                 (assoc-in [:uniforms :txn] (math/* projection model-txn))))))))

(defn on-mouse-move [state e]
  ; XXX: flatten this structure
  (let [{:keys [window-size
                window-height
                eye-pos
                radius-px
                eye0-pos
                anchor
                map-id]}
        @(:user-data state)
        mouse-px (math/- (vec2 (.-clientX e) (.-clientY e)) (math/div window-size 2))
        mouse-pos (math/- (math/div mouse-px radius-px) eye-pos)
        [x y z] (hex/px->cube (:x mouse-pos) (:y mouse-pos))
        ;; XXX: remember last hex coords and only look for new one if not the same?
        hex (d/q '[:find ?c .
                   :in $ ?x ?y ?z
                   :where
                   [?c :hex/x ?x]
                   [?c :hex/y ?y]
                   [?c :hex/z ?z]]
                 @conn x y z)]
    (swap! (:user-data state)
           (fn [old]
             (cond-> old
               true (assoc :selected-hex hex)
               anchor (merge
                       {:eye-pos (math/+ eye0-pos
                                         (math/div (math/- (vec2 (.-clientX e) (.-clientY e))
                                                           anchor)
                                                   radius-px))}))))))

(defn end-drag [state]
  (swap! (:user-data state) dissoc :anchor :eye0-pos))

(defn on-mouse-down [state e]
  (swap! (:user-data state)
         (fn [old]
           (merge
            old
            {:eye0-pos (:eye-pos old)
             :anchor (vec2 (.-clientX e) (.-clientY e))}))))

(defn on-mouse-up [state e]
  (end-drag state))

(defn on-mouse-leave [state e]
  (end-drag state))

(defn on-wheel [state e]
  (swap! (:user-data state)
         (fn [{:keys [window-size radius-px eye-pos] :as old}]
           (let [scale (- 1 (* (.-deltaY e) wheel-scale-factor))
                 mouse-pos (math/+ eye-pos
                                   (math/div (math/- (vec2 (.-clientX e) (.-clientY e))
                                                     (math/div window-size 2))
                                             radius-px))]
             (merge old
                    {:radius-px (* radius-px scale)
                     :eye-pos (math/+ mouse-pos
                                      (math/* (math/- eye-pos mouse-pos)
                                              scale))})))))

(defn when-ready []
  (sente/send! [:terrain/make-map]
               10000
               (fn [result]
                 (println "got result!" result)
                 (if-not (cb-success? result)
                   (js/alert (str "Problem fetching initial map: " (pr-str result)))
                   (do
                     (init-map conn result)
                     (rum/mount (canvas {:after-render (fn [& args] (apply draw args))
                                         :on-mouse-down (fn [& args] (apply on-mouse-down args))
                                         :on-mouse-up (fn [& args] (apply on-mouse-up args))
                                         :on-mouse-leave (fn [& args] (apply on-mouse-leave args))
                                         :on-mouse-move (fn [& args] (apply on-mouse-move args))
                                         :on-resize (fn [& args] (apply on-resize args))
                                         :on-wheel (fn [& args] (apply on-wheel args))})
                                (js/document.getElementById "app_container")))))))

(defmethod sente/server-msg-handler :chsk/state
  [{[_ [_ {:keys [first-open?]}]] :event}]
  (when first-open? (when-ready)))

(defn ^:export main []
  (enable-console-print!)
  (sente/start-once!))
