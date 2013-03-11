(ns com.mccaffrey.utils.media-effects
  (:import 
    [android.media.effect Effect])
  (:use [com.mccaffrey.utils general]
        [neko log]))

(set! *warn-on-reflection* true)

(defn apply-effect
  [^Effect effect src-tex dst-tex]
  ; Extract apply to a fn
  (.apply effect
          ; repeated drilling into state is ugly, should clean up
          ; Also type hints suck, I'd like to push em into function decls
          ^Integer (src-tex :name)
          ; TODO use real size
          ^Integer (src-tex :width)
          ^Integer (src-tex :height)
          ^Integer (dst-tex :name)))
