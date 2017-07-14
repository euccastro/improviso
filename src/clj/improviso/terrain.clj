(ns improviso.terrain
  (:import OpenSimplexNoise))

(def rescale-factor 1.1548607)

(defn make-map- [rows cols]
  (let [osn (OpenSimplexNoise.)]
    (println "OSN" (* rescale-factor (.eval osn 1.0 1.0)))))

(defn make-map []
  {:map/cols 2
   :map/rows 2
   :map/hexes [{:hex/x 0 :hex/y 0 :hex/z 0 :hex/color [1.0 0.0 0.0 1.0]}
               {:hex/x 1 :hex/y -1 :hex/z 0 :hex/color [0.0 1.0 0.0 1.0]}
               {:hex/x 1 :hex/y 0 :hex/z -1 :hex/color [0.0 0.0 1.0 1.0]}
               {:hex/x 2 :hex/y -1 :hex/z -1 :hex/color [1.0 0.0 1.0 1.0]}]})
