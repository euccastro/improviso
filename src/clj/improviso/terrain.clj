(ns improviso.terrain
  (:require [clojure.pprint :refer (pprint)])
  (:import OpenSimplexNoise))

(def rescale-factor 1.1548607)

(defn make-map- [rows cols]
  (let [osn (OpenSimplexNoise.)]
    (println "OSN" (* rescale-factor (.eval osn 1.0 1.0)))))

(def map-radius 3)

(defn coord->color [coord]
  (/ (+ coord map-radius)
     (* 2 map-radius)))

(defn make-map []
  (let [ret 
        {:map/radius map-radius
         :map/hexes (into []
                          (for [x (range (- map-radius) (+ 1 map-radius))
                                y (range (max (- map-radius) (- (+ x map-radius)))
                                         (+ 1 (min map-radius (- map-radius x))))
                                :let [z (- (+ x y))]]
                            {:hex/x x :hex/y y :hex/z z :hex/color [(coord->color x)
                                                                    (coord->color y)
                                                                    (coord->color z)
                                                                    1.0]}))}]
    (pprint ret)
    ret))
