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
  gl_Position=vec4(position, 0.0, 1.0);
  pos = position.xy;
}"
   :fs "
float round(in float x) {
  return floor(x + 0.5);
}

vec3 vround(in vec3 v) {
  return floor(v + 0.5);
}

vec3 cube_round(in vec3 v) {
  vec3 r = vround(v);
  vec3 d = abs(v - r);
  if (d.y < d.x && d.z < d.x) {
    return vec3(-r.y-r.z, r.y, r.z);
  }
  if (d.x < d.y && d.z < d.y) {
    return vec3(r.x, -r.x-r.z, r.z);
  }
  return vec3(r.x, r.y, -r.x-r.y);
}

float cube_length(in vec3 v) {
  return (abs(v.x) + abs(v.y) + abs(v.z)) / 2.0;
}

vec3 cube_wrap(in vec3 v) {
  if (cube_length(v) <= mapradius) {
    return v;
  }
  vec3 other = vec3(mapradius * 2.0 + 1.0,
                    -mapradius,
                    -mapradius - 1.0);
  for (int i=0; i<6; i++) {
     vec3 dv = v - other;
     if (cube_length(dv) <= mapradius) {
       return dv;
     }
     other = vec3(-other.y, -other.z, -other.x);
  }
  return v;
}

vec3 px2cube(in vec2 v) {
  float x = (v.x * 1.7320508075688772 - v.y) / 3.0;
  float z = v.y * 0.6666666666666666;
  return cube_round(vec3(x, -x-z, z));
}

void main() {
  vec4 pos4 = invtxn * vec4(pos, 0.0, 1.0);
  vec3 raw_cube = px2cube(pos4.xy);
  vec3 cube = cube_wrap(raw_cube);
  if (cube == selectedhex) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  } else {
//    gl_FragColor = vec4(cube, 1.0);
    float diameter = float(mapradius) * 2.0 + 3.0;
    float u = (1.5 + cube.x + mapradius) / diameter;
    float v = (1.5 + cube.z + mapradius) / diameter;
    gl_FragColor = texture2D(maptex, vec2(u, v));
    if (cube_length(raw_cube) > mapradius) {
      gl_FragColor.a = min(0.6, gl_FragColor.a);
    }
  }
}
"
   :uniforms {:color :vec4
              :invtxn [:mat4 M44]
              ;; defaults to impossible coords; don't add to 0
              :mapradius :float
              :maptex :sampler2D
              :selectedhex [:vec3 (vec3 -1 -1 -1)]}
   :varying {:pos :vec2}
   :attribs {:position :vec2}})

(defn on-resize [state]
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)
        proj (gl/ortho 0 0 w h -1 1)
        [[map-id map-radius]] (into [] (d/q '[:find ?m ?r :where [?m :map/radius ?r]] @conn))
        map-ent (d/entity @conn map-id)
        map-diameter (+ 1 (* 2 (:map/radius map-ent)))
        hradii (hex/map-width map-diameter)
        vradii (hex/map-height map-diameter)
        radius (min (/ w hradii) (/ h vradii))]
    (swap! (:user-data state)
           #(merge {:eye-pos (vec2 0 0)}
                   %
                   {:window-size (vec2 w h)
                    :projection proj
                    :radius-px radius
                    :map-id map-id
                    :map-radius map-radius}))))

(defn make-tex-array [radius {:keys [map/hexes]}]
  (let [diameter (+ 1  ; center hex
                    2  ; borders
                    (* 2 radius))
        length (* diameter diameter 4)  ; RGBA
        array (js/Uint8Array. length)]
    (.fill array 0)
    (doseq [{:keys [hex/x hex/z hex/color]} hexes
            :let [index (* 4
                           (+ (* diameter (+ 1 z radius)) ; 1 for v margin
                              1  ; for h margin
                              x
                              radius))]
            [i v] (map-indexed vector color)]
      (aset array (+ index i) (Math/floor (* v 255))))
    array))

(defn draw [state]
  (let [{:keys [window-size
                projection
                radius-px
                eye-pos
                map-id
                map-radius
                selected-hex]}
        @(:user-data state)
        dom-node (rum/dom-node state)
        gl (gl/gl-context dom-node {:alpha false})
        txn (-> M44
                (geom/translate (math/* window-size 0.5))
                (math/* (geom/scale M44 (vec3 radius-px radius-px 1)))
                (geom/translate eye-pos))
        inv-radius-px (/ 1 radius-px)
        inv-txn (-> M44
                    (geom/translate (math/- eye-pos))
                    (math/* (geom/scale M44 (vec3 inv-radius-px inv-radius-px 1)))
                    (geom/translate (math/- (math/* window-size 0.5))))
        tex (or (.-tex gl)
                (let [mapm
                      (d/pull @conn '[{(limit :map/hexes nil) [*]}] map-id)
                      diameter (+ 1 2 (* 2 map-radius)) ; 2 for margins
                      opts {:width diameter
                            :height diameter
                            :format glc/rgba
                            :filter [glc/nearest glc/nearest]
                            :wrap [glc/clamp-to-edge glc/clamp-to-edge]
                                        ;          :type glc/float
                            :pixels (make-tex-array map-radius mapm)}
                      t (buf/make-texture gl opts)]
                  (set! (.-tex gl) t)
                  t))
        shader (shader/make-shader-from-spec gl line-shader-spec)
        model (-> (rect/rect (math/- (math/div window-size 2)) window-size)
                  (gl/as-gl-buffer-spec {})
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (assoc :shader shader))]
    (.enable gl (.-BLEND gl))
    (.blendFunc gl (.-SRC_ALPHA gl) (.-ONE_MINUS_SRC_ALPHA gl))
    (gl/clear-color-and-depth-buffer gl 0.3 0.3 0.3 1.0 1)
    (gl/set-viewport gl 0 0 (:x window-size) (:y window-size))
    (gl/draw-with-shader gl
                         (cond-> model
                             true (assoc-in [:uniforms :invtxn] (math/* inv-txn (math/invert projection)))
                             true (assoc-in [:uniforms :color] [1 0 0 1])
                             true (assoc-in [:uniforms :mapradius] map-radius)
                             true (assoc-in [:uniforms :maptex] tex)
                             selected-hex (assoc-in [:uniforms :selectedhex] selected-hex)))))

(defn on-mouse-move [state e]
  ; XXX: flatten this structure
  (let [{:keys [window-size
                window-height
                eye-pos
                radius-px
                eye0-pos
                anchor
                map-id
                map-radius]}
        @(:user-data state)
        mouse-px (math/- (vec2 (.-clientX e) (.-clientY e)) (math/div window-size 2))
        mouse-pos (math/- (math/div mouse-px radius-px) eye-pos)
        mouse-cube (hex/map-wrap map-radius (hex/px->cube mouse-pos))]
    (apply println "selected hex" mouse-cube)
    (swap! (:user-data state)
           (fn [old]
             (cond-> old
               true (assoc :selected-hex mouse-cube)
               anchor (merge
                       {:eye-pos
                        (hex/map-wrap-px
                         map-radius
                         (math/+ eye0-pos
                                 (math/div (math/- (vec2 (.-clientX e) (.-clientY e))
                                                   anchor)
                                           radius-px)))}))))))

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
         (fn [{:keys [window-size radius-px eye-pos map-radius] :as old}]
           (let [scale (- 1 (* (.-deltaY e) wheel-scale-factor))
                 mouse-pos (math/+ eye-pos
                                   (math/div (math/- (vec2 (.-clientX e) (.-clientY e))
                                                     (math/div window-size 2))
                                             radius-px))]
             (merge old
                    {:radius-px (* radius-px scale)
                     :eye-pos (hex/map-wrap-px
                               map-radius
                               (math/+ mouse-pos
                                       (math/* (math/- eye-pos mouse-pos)
                                               scale)))})))))

(defn when-ready []
  (sente/send! [:terrain/make-map]
               10000
               (fn [result]
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
