(ns com.mccaffrey.nlve.vp
  (:use [com.mccaffrey.utils general time]))

; TODO it'd be nice to use Schema to lay down some loose data-types

(def ^{:dynamic true} *vp*)

(defn ms-window-end
  [vp]
  (+ (vp :ms-window-width)
     (vp :ms-window-start)))

(defn pixel-window-end
  [vp]
  (+ (vp :pixel-window-width)
     (vp :pixel-window-start)))

(defn int-lg
  [^Integer x]
  (loop 
    [xx x
     n 0]
    (if (zero? xx) 
      n
      (recur (bit-shift-right xx 1) (inc n)))))

(defn pot-<=
  [^Integer x]
  (bit-shift-left 1 (int-lg x)))

(defn pot->=
  [^Integer x]
  (let [pot (pot-<= x)]
    (if (= pot x)
      x
      (bit-shift-left pot 1))))

(defn multiple-<=
  [factor x]
  (- x (mod x factor)))

(defn multiple->=
  [factor x]
  (let [under (multiple-<= factor x)]
    (if (= x under)
      under
      (+ under factor))))

; TODO, is a POT up big enough to avoid frequent resize?
(defn expand-vp-to-chunky-size
  [vp]
  (let [pot->=-width (pot->= (int (vp :ms-window-width)))
        chunky-start (multiple-<= pot->=-width (int (vp :ms-window-start)))
        chunky-end (multiple->= pot->=-width (ms-window-end vp))]
    (assoc vp
           :ms-window-width (- chunky-end chunky-start)
           :ms-window-start chunky-start)))

; Just scale by width.  Useful for widths/measures/etc
(defn scale-ms-to-pix
  [ms & {:keys [vp] :or {vp *vp*}}]
  (-> ms
    (* (vp :pixel-window-width))
    (/ (vp :ms-window-width))
    (int)))

; Just scale by width.  Useful for widths/measures/etc
(defn scale-pix-to-ms
  [pix & {:keys [vp] :or {vp *vp*}}]
  (-> pix
    (* (vp :ms-window-width))
    (/ (vp :pixel-window-width))
    (int)))

(defn transform-ms-to-pix
  [ms & {:keys [vp] :or {vp *vp*}}]
  (scale-ms-to-pix
    (- ms (vp :ms-window-start))))

(defmacro with-vp
  [vp & body]
  `(binding [*vp* ~vp]
     ~@body))

; TODO should this be X-only?  It's doable.

(defn transform-pix-to-ms
  [px & {:keys [vp] :or {vp *vp*}}]
  (+ (/ (* px (vp :ms-window-width))
        (vp :pixel-window-width))
     (vp :ms-window-start)))

; scale to keep the
; gestures focal point
; at the same spot
; unless we hit 0 ms
; that works out to 
; new-ms-offset = scale*ms-offset + ms-pos*(1.0f - scale)
; mx = px*(mw/pw) + mo
(defn vp-scaler
  [sf fx]
  (fn [vp]
    (assoc vp
           :ms-window-width
           (/ (vp :ms-window-width) sf)
           :ms-window-start
           (+ (* sf (vp :ms-window-start))
              (* (- 1.0 sf)
                 (? (transform-pix-to-ms (? fx) :vp (? vp))))))))

(defn vp-translater-pix
  [dx]
  (fn [vp] (update-in vp [:ms-window-start] + (scale-pix-to-ms dx :vp vp))))

(def min-vp-width-ms 50)
(def max-vp-width-ms (hr2ms 24))
(defn vp-validate
  [vp]
  ; We should only go so skinny
  (-> (? vp)
    (update-in [:ms-window-width] max min-vp-width-ms)
    (update-in [:ms-window-width] min max-vp-width-ms)
    (update-in [:ms-window-start] max 0)))
