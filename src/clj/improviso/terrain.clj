(ns improviso.terrain
  (:require [clojure.pprint :refer (pprint)]
            [improviso.hex :as hex]
            [thi.ng.geom.vector :refer (vec2 vec3)]
            [thi.ng.math.core :as math])
  (:import OpenSimplexNoise))

(def rescale-factor 1.1548607)

(def map-radius 96)

(defn sample [osn scale v]
  (let [[x y] (math/* v scale)]
    (* rescale-factor (.eval osn x y))))

(defn average [& nums]
  (/ (apply + nums) (count nums)))

(defn samples [osn v scales]
  (apply average (map #(sample osn % v) scales)))

(defn coords->color [osn v]
  (let [v (math/div (hex/cube->xy v) 4.0)
        warp-x (samples osn (math/+ v (vec2 120.0 240.0)) [0.0078125 0.015625 0.03125 0.125])
        warp-y (samples osn (math/+ v (vec2 240.0 120.0)) [0.0078125 0.015625 0.03125 0.125])
        n (samples osn (vec2 (* (:x v) warp-x) (* (:y v) warp-y)) [0.03125 0.0625 0.125 0.25 0.5])
        c (/ (+ n 1.0) 2.0)]
    (if (< c 0.6)
      [0.0 c 1.0 1.0]
      [1.0 c 0.0 1.0])))

(defn make-map []
  (let [osn (OpenSimplexNoise.)]
    {:map/radius map-radius
     :map/hexes (into []
                      (for [x (range (- map-radius) (+ 1 map-radius))
                            y (range (max (- map-radius) (- (+ x map-radius)))
                                     (+ 1 (min map-radius (- map-radius x))))
                            :let [z (- (+ x y))]]
                        {:hex/x x :hex/y y :hex/z z :hex/color (coords->color osn (vec3 x y z))}))}))
