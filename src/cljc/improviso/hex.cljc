(ns improviso.hex
  (:require [thi.ng.geom.vector :as vec :refer (vec2 vec3)]
            [thi.ng.math.core :as math]))

;; in radii, assuming rows > 0
(defn map-height [rows]
  (+ 2 (* rows (/ 2 3))))

(def width-factor (/ (Math/sqrt 3) 2))

;; in radii, assuming rows > 1 and columns > 0
(defn map-width [columns]
  (* width-factor (+ 1 (* 2 columns))))

(def x-unit (vec2 width-factor -0.5))

(def y-unit (vec2 (- width-factor) -0.5))

(def z-unit (vec2 0.0 1.0))

(defn cube-round [x y z]
  ;; http://www.redblobgames.com/grids/hexagons/#rounding
  (let [rx (Math/round x)
        ry (Math/round y)
        rz (Math/round z)
        dx (Math/abs (- rx x))
        dy (Math/abs (- ry y))
        dz (Math/abs (- rz z))]
    (cond (and (< dy dx) (< dz dx)) [(- (+ ry rz)) ry rz]
          (and (< dx dy) (< dz dy)) [rx (- (+ rx rz)) rz]
          :else [rx ry (- (+ rx ry))])))

(def sqrt-3-over-3 (/ (Math/sqrt 3) 3))
(def two-thirds (/ 2 3))

(defn px->cube
  "px and py are assumed to be normalized to radius and offset"
  [px py]
  (let [x (- (* px sqrt-3-over-3)
             (/ py 3))
        z (* py two-thirds)
        y (- (+ x z))]
    (cube-round x y z)))

(defn cube->xy
  [[x y z]]
  (math/+ (math/* x-unit x) (math/* y-unit y) (math/* z-unit z)))
