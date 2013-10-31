(ns com.mccaffrey.utils.media-effects
  (:import 
    [android.media.effect Effect EffectFactory])
  (:use [com.mccaffrey.utils general]
        [neko log]))

(set! *warn-on-reflection* true)
; (deflog "media-effects")

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

(defn make-effect
  "Wrapper for MediaEffect creation"
  [^EffectFactory factory str-name]
  {:pre [(EffectFactory/isEffectSupported str-name)]}
  (log-i (str "Making effect " str-name))
  (? (.createEffect factory str-name)))

