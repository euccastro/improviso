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

(defn cube-round [[x y z]]
  ;; http://www.redblobgames.com/grids/hexagons/#rounding
  (let [rx (Math/round x)
        ry (Math/round y)
        rz (Math/round z)
        dx (Math/abs (- rx x))
        dy (Math/abs (- ry y))
        dz (Math/abs (- rz z))]
    (apply vec3
           (cond (and (< dy dx) (< dz dx)) [(- (+ ry rz)) ry rz]
                 (and (< dx dy) (< dz dy)) [rx (- (+ rx rz)) rz]
                 :else [rx ry (- (+ rx ry))]))))

(def sqrt-3-over-3 (/ (Math/sqrt 3) 3))
(def two-thirds (/ 2 3))

(defn px->cube
  "px and py are assumed to be normalized to radius and offset"
  [[px py]]
  (let [x (- (* px sqrt-3-over-3)
             (/ py 3))
        z (* py two-thirds)
        y (- (+ x z))]
    (cube-round [x y z])))

(defn cube->xy
  [[x y z]]
  (math/+ (math/* x-unit x) (math/* y-unit y) (math/* z-unit z)))

(defn northeast-mirror-center [radius]
  (vec3 (+ (* 2 radius) 1)
        (- radius)
        (- (+ radius 1))))

(def mirror-centers
  (memoize
   (fn [radius]
     (let [v (northeast-mirror-center radius)]
       [v
        (vec3 (- (:y v)) (- (:z v)) (- (:x v)))
        (vec3 (:z v) (:x v) (:y v))
        (vec3 (- (:x v)) (- (:y v)) (- (:z v)))
        (vec3 (:y v) (:z v) (:x v))
        (vec3 (- (:z v)) (- (:x v)) (- (:y v)))]))))

(defn cube-length [v]
  (/ (+ (Math/abs (:x v)) (Math/abs (:y v)) (Math/abs (:z v)))
     2.0))

(defn map-wrap [radius v]
  (loop [centers (cons (vec3 0 0 0)
                       (mirror-centers radius))]
    (if (nil? centers)
      v
      (let [c (first centers)
            dv (math/- v c)]
        (if (<= (cube-length dv) radius)
          dv
          (recur (rest centers)))))))

(defn map-wrap-px [radius v-px]
  (let [v (px->cube v-px)
        wrap-v (map-wrap radius v)]
    (math/+ v-px (cube->xy (math/- wrap-v v)))))
