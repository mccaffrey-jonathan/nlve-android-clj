(ns com.mccaffrey.utils.time
  (:use [com.mccaffrey.utils general]))

(defn s2ms
  [s]
  (* 1000 s))

(defn min2ms
  [mn]
  (-> mn
    (* 60)
    s2ms))

(defn hr2ms
  [hr]
  (-> hr
    (* 60)
    min2ms))
