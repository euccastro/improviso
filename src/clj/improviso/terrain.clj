(ns improviso.terrain
  (:require [clojure.pprint :refer (pprint)]
            [improviso.hex :as hex]
            [thi.ng.geom.vector :refer (vec2 vec3)]
            [thi.ng.math.core :as math]
            [thi.ng.math.noise :refer (noise2 noise3)])
  (:import OpenSimplexNoise))

(def rescale-factor 1.1548607)

(def map-radius 64)

(def sea-level 0.7)

(def border-saturation-factor 1.0)

(def flat-land false)

(def flat-sea false)

(def rng-seed 141)

(def warp-offset 3)

(defn sample [osn scale v]
  (let [[x y] (math/* v scale)]
    (comment (* rescale-factor (noise2 x y)))
    (comment (* rescale-factor (noise3 x y 3.5)))
    (* rescale-factor (.eval osn x y))
    ))

(defn average [& nums]
  (/ (apply + nums) (count nums)))

(defn samples
  "Freqs are assumed to be octaves (ie., each one half the next one)"
  [osn v freqs]
  (reduce (fn [accum [i val]]
            (+ accum (/ val (+ i 2))))
          0
          (map-indexed (fn [i freq]
                         [i (sample osn freq v)])
                       freqs)))

(defn coords->height:1-center [osn v]
  (let [v (math/div (hex/cube->xy v) 4.0)
        warp-x (samples osn (math/+ v (vec2 120.0 240.0)) [0.125 0.25])
        warp-y (samples osn (math/+ v (vec2 240.0 120.0)) [0.125 0.25])
        ;[warp-x warp-y] [0.0 0.0]
        ]
    (samples osn
             (vec2 (+ (:x v) (* warp-x warp-offset))
                   (+ (:y v) (* warp-y warp-offset)))
             [0.2 0.4 0.8])))

(defn square [x]
  (* x x))

(defn clamp [x lowest highest]
  (min highest (max lowest x)))

(defn coords->color [osn map-radius v]
  (let [centers (hex/mirror-centers map-radius)
        diffs (map #(math/- v %) centers)
        heights (map #(coords->height:1-center osn %) diffs)
        inv-distance-sqs (map #(/ 1 (max 0.5 (square (hex/cube-length %)))) diffs)
        inv-distance-sq-sum (apply + inv-distance-sqs)
        weights (map #(/ % inv-distance-sq-sum) inv-distance-sqs)
        rel-height (apply + (map * heights weights))
        rel-height (clamp (* rel-height (+ 1 (* border-saturation-factor
                                                (/ (hex/cube-length v) map-radius))))
                          -1.0
                          1.0)
        abs-height (/ (+ rel-height 1.0) 2.0)]
    (if (< abs-height sea-level)
      [0.0 (if flat-sea 0.3 abs-height) 1.0 1.0]
      [0.9 (if flat-land 0.8 abs-height) 0.2 1.0])))

(defn make-map []
  (let [osn (OpenSimplexNoise. rng-seed)]
    {:map/radius map-radius
     :map/hexes (into []
                      (for [x (range (- map-radius) (+ 1 map-radius))
                            y (range (max (- map-radius) (- (+ x map-radius)))
                                     (+ 1 (min map-radius (- map-radius x))))
                            :let [z (- (+ x y))]]
                        {:hex/x x :hex/y y :hex/z z :hex/color (coords->color osn map-radius (vec3 x y z))}))}))
