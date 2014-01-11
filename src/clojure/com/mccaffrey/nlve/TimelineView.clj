(ns com.mccaffrey.nlve.TimelineView 
  (:import [android.animation AnimatorListenerAdapter ObjectAnimator]
           [android.graphics Color]
           [android.view 
            View
            ViewGroup
            ViewGroup$LayoutParams]
           [android.widget
            HorizontalScrollView
            LinearLayout
            RelativeLayout
            RelativeLayout$LayoutParams
            TextView ])
  (:use [com.mccaffrey.utils general time]
        [neko activity]))

; Idea: periodically zero out scale and sizes
; Testing code for doing timeline this way
(defn bounce-scale
  [view x]
  (doto (ObjectAnimator/ofFloat view View/SCALE_X (into-array Float/TYPE [x]))
    (.setDuration 300)
    (.addListener
      (proxy [AnimatorListenerAdapter] []
        (onAnimationCancel [anim] nil)
        (onAnimationEnd [anim]
          (bounce-scale view (if (= x 1) 5 1)))))
    (.start)))

(defn add-bouncey-view [^ViewGroup vg]
    (.addView 
              vg
              (doto (HorizontalScrollView. *activity*)
                (.addView
                  (do-let [rel (RelativeLayout. *activity*)]
                          (doseq [x (range 10)
                                  :let [params (RelativeLayout$LayoutParams.
                                                 ViewGroup$LayoutParams/WRAP_CONTENT
                                                 ViewGroup$LayoutParams/WRAP_CONTENT)]]
                            (.addView rel (doto (TextView. *activity*)
                                            (.setSingleLine true)
                                            (.setBackgroundColor Color/DKGRAY)
                                            (.setText (apply str (repeat 3 (str x)))))
                                      params)
                            (set! (. params leftMargin) (* 100 x)))
                          (bounce-scale rel 5))))
              (ViewGroup$LayoutParams.
                ViewGroup$LayoutParams/MATCH_PARENT
                ViewGroup$LayoutParams/WRAP_CONTENT)))

(defn adjust-view-group-to-model!
  [^ViewGroup vg model add-view-fn]
  (let [model-cnt (count model)
        view-cnt (.getChildCount vg)
        extra-view-cnt (- view-track-cnt model-track-cnt)]
    (if (> extra-view-cnt 0)
      (.removeViews vg model-cnt extra-view-cnt))
    ; May be an empty range
    (doseq [i (range model-cnt view-cnt)]
      (add-view-fn vg i))))

(defn add-clip-view
  [vg i]
  (.addView ^RelativeLayout vg
            ^TextView (doto (TextView. *activity*)
                        (.setClickable true)
                        (.setLongClickable true)
                        (.setBackgroundColor Color/BLUE)
                        (.setSingleLine true))
            ^RelativeLayout$LayoutParams
            (RelativeLayout$LayoutParams.
              0 ; We have to provide some width... real one set below
              ^Integer ViewGroup$LayoutParams/FILL_PARENT)))

; This is specific to zooming our horizontal scrollview
(defn zoom-listener
  [^ViewGroup scroller]
  (let [zoomer (.getChildAt scroller 0)
        rescale-from-detector
        (fn [^ScaleGestureDetector sc-detector]
          (.setScaleX zoomer
                      (+ (.getScaleX zoomer)
                         (.getScaleFactor sc-detector))))
        sc-detector
          (proxy [ScaleGestureDetector$OnScaleGestureListener] []
            (onScaleBegin [sc-detector] (rescalesc-detector))
            (onScale [sc-detector] (rescale-from-detector sc-detector))
            (onScaleEnd [sc-detector] (rescale-from-detector sc-detector)))]
    (proxy [View$OnTouchListener] []
      (onTouch [_ ev]
          (.onTouchEvent sc-detector ev)
          (.isInProgress sc-detector)))))

; For the initial scale, start at 5-30 seconds onscreen at once,
; in the size the elements are created at, try all in the scaleX?
; TODO add model schema?
(defn update-ui 
  [ui model]
  ; ignore ui for now?
  (let [pix-w (-> *activity*
                  (.getWindowManager)
                  (.getDefaultDisplay)
                  (.getWidth))
        scroller (or ui
                     (do-let [hsv (HorizontalScrollView. *activity*)]
                       (.addView hsv
                                 (doto (RelativeLayout. *activity*)
                                   (.setScaleX (/ pix-w (s2ms 30)))
                                   (.addView
                                     (doto (LinearLayout. *activity*)
                                       (.setOrientation LinearLayout/VERTICAL)))))
                       (.setOnTouchListener hsv (zoom-listener hsv))))
        track-views (-> scroller (.getChildAt 0) (getChildAt 0))]
    (adjust-view-group-to-model! track-views model
                                 (fn [vg i]
                                   (.addView vg (RelativeLayout. *activity*))))
    (doseq [[i track] (indexed model)
            ; TODO way to share the collection length matching?
            :let [rel ^RelativeLayout (.getChildAt track-views i)]]
      (adjust-view-group-to-model! rel track add-clip-view)
      (doseq [[j clip] (indexed track)
              :let [cv (.getChildAt rel j)
                    params ^RelativeLayout$LayoutParams (.getLayoutParams cv)]]
        (.setContentDescription cv (clip :txt))
        (set! (. params width (clip :length-ms)))
        (set! (. params leftMargin (clip :start-ms)))))))
