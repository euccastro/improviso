(ns improviso.hex
  (:require [thi.ng.geom.vector :as vec :refer (vec3)]
            [thi.ng.math.core :as math]))

;; XXX: reader conditional in cljc.
(defn sqrt [x]
  (Math/sqrt x))

;; in radii, assuming rows > 0
(defn map-height [rows]
  (+ 2 (* rows (/ 2 3))))

(def width-factor (/ (sqrt 3) 2))

;; in radii, assuming rows > 1 and columns > 0
(defn map-width [columns]
  (* width-factor (+ 1 (* 2 columns))))

(def x-unit (vec3 width-factor -0.5 0))

(def y-unit (vec3 (- width-factor) -0.5 0))

(def z-unit (vec3 0 1 0))
