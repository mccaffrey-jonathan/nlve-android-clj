(ns com.mccaffrey.nlve.timeline-ui
  ; TODO trime neko list
  (:import 
    [android.content Context]
    [android.graphics Color]
    [android.view View]
    [android.view
     GestureDetector
     GestureDetector$SimpleOnGestureListener
     MotionEvent
     ScaleGestureDetector
     ScaleGestureDetector$OnScaleGestureListener
     ViewGroup$LayoutParams
     ViewGroup$MarginLayoutParams ]
    [android.widget
     ArrayAdapter
     HorizontalScrollView
     LinearLayout
     ListView
     RelativeLayout
     RelativeLayout$LayoutParams
     TextView]
    [com.mccaffrey.nlve
     ObservableScrollView
     ObservableScrollView$ObservableScrollViewListener]
    [java.util ArrayList])
  (:use [com.mccaffrey.nlve vp]
        [com.mccaffrey.utils general time]
        [neko activity]))


(def debug-filter true)

; TODO, soon separate file for timeline data
; TODO either this filter or the viewport is wrong.
; Also, the zooming is centered weirdly
(defn filter-intersects-vp
  [vp track]
  (if debug-filter track
    (let [chunky-vp (expand-vp-to-chunky-size vp)]
      (subseq track
              >= {:start-ms (vp :ms-window-start)}
              < {:start-ms (ms-window-end vp)}))))

(defn make-track-layout [convertView]
  (or convertView
      (doto (proxy [RelativeLayout] [*activity*]
              (onTouchEvent [_] false))
      ; (doto (RelativeLayout. *activity*)
        ; TODO Like styles, can we refer to an XML template for behavior?
        (.setFocusable false)
        (.setClickable false)
        (.setContentDescription "RelativeLayout containing clips")
        (.setMinimumHeight 50))))

; TODO lookup Android equivalent to CSS.  Use it.
(defn set!-clipview-for-clip
  [^TextView tv clip]
  (doto tv
    (.setText ^String (clip :txt)))
  (set! (. ^RelativeLayout$LayoutParams
           (.getLayoutParams tv) width)
        (scale-ms-to-pix (clip :length-ms)))
  (set! (. ^RelativeLayout$LayoutParams
           (.getLayoutParams tv) leftMargin)
        ; No transformation, handle that with scrollview?
        (scale-ms-to-pix (clip :start-ms))))

(defn add-or-get-clipview-from-layout
  [layout idx]
  (if (< idx (.getChildCount ^RelativeLayout layout))
    (.getChildAt ^RelativeLayout layout ^Integer idx)
    (do-let [tv ^TextView (TextView. *activity*)]
            (doto tv
              (.setContentDescription "TextView for clip")
              (.setClickable true)
              (.setLongClickable true)
              (.setBackgroundColor Color/BLUE)
              (.setSingleLine true))
            (.addView ^RelativeLayout layout
                      ^TextView tv
                      ^RelativeLayout$LayoutParams
                      (RelativeLayout$LayoutParams.
                        0 ; We have to provide some width... real one set below
                        ^Integer ViewGroup$LayoutParams/FILL_PARENT)))))

; TODO something more data-bind-ey
; If that thing doesn't exit, be a good project
; Backbone or whatnot for android + clojure
(defn track-view
  "This needs to take this back-ackwards convertView argument
  so that it's a nicely behaved Adapter."
  [convertView track]
  (do-let [^RelativeLayout layout (make-track-layout convertView)]
          (let [; ^RelativeLayout layout (.getChildAt ^ViewGroup scroller 0)
                vp-track (filter-intersects-vp *vp* track)
                ; Not sure if this'd be memo-ized nicely otherwise
                cnt-vp-track (count vp-track)
                cnt-layout (.getChildCount ^RelativeLayout layout)]
            ; First, knock-off any views we don't need from the end
            (doseq [^Integer idx (reverse (range cnt-vp-track cnt-layout))]
              (.removeViewAt layout idx))
            ; Add views we need onto the end
            (doseq [[idx clip] (indexed vp-track)]
              (set!-clipview-for-clip
                (add-or-get-clipview-from-layout layout idx) clip)))))
; we use the zoom levels for ticks/labeling that are the appropriate size
; for our current zoom.
(def ruler-ticks-in-ms
  (apply sorted-set 
         100 ; ms
         (concat
           (map s2ms [1 2 3 5 10 30 60])
           (map min2ms [5 10 30])
           (map hr2ms [1 2 4 6 8 12 24]))))

; func in case it needs to think about it
(defn max-num-divisions-per-screen [] 10)

; Make this handle times like 1h2m better
(defn short-pp-time
  [time-ms]
  (cond
    (mod time-ms 1000) (str (/ time-ms 1000) "s")
    (mod time-ms (* 1000 60)) (str (/ time-ms 1000) "m")
    :else "|"))

(defn tick-ms-for-viewport
  []
  (or (first (filter
               #(< (/ (*vp* :ms-window-width) %)
                   (max-num-divisions-per-screen))
               ruler-ticks-in-ms))
      (last ruler-ticks-in-ms)))

(defn ticks-ms-for-viewport []
  (let [tick-ms (tick-ms-for-viewport)
        ms-window-end (+ (*vp* :ms-window-width)
                         (*vp* :ms-window-start))
        start-tick (-> *vp*
                     :ms-window-start
                     (/ tick-ms)
                     (float)
                     (Math/ceil)
                     (* tick-ms))]
    (for [i (range)
          :let [ms-offset (+ start-tick (* i tick-ms))]
          :while (< ms-offset ms-window-end)]
      ms-offset)))

; TODO this doesn't translate happily
; TODO make some ViewportParams struct, i've been writing
; pixel-window ms-window alot.
(defn time-ruler []
  (do-let [rel (RelativeLayout. *activity*)]
    (doseq [ms (ticks-ms-for-viewport)]
      (.addView rel
        (doto (LinearLayout. *activity*)
          (.setOrientation LinearLayout/VERTICAL)
          ; TODO add a line here
          (.addView (doto ^TextView (TextView. *activity*)
                      (.setText ^String (short-pp-time ms))
                      (.setSingleLine true))))
          (do-let [params (RelativeLayout$LayoutParams. 
                                    ViewGroup$LayoutParams/WRAP_CONTENT
                                    ViewGroup$LayoutParams/WRAP_CONTENT)]
             ; TODO just setting left margin is going to push these to the side..
             (set! (. params leftMargin)
                   (transform-ms-to-pix ms)))))))

(defn update-timeline-adapter
  [^ArrayAdapter adapter ^ArrayList l timeline]
  (.clear l)
  (.addAll l ^Collection timeline)
  (.notifyDataSetChanged adapter))

(defn make-timeline-adapter
  []
  (let [l ^List (ArrayList.)
        act *activity*] 
    [(proxy [ArrayAdapter] [^Context *activity* 0 l]
       (getView [position convertView parent]
         ; TODO change where seconds is calced
         (with-activity
           act
           (with-vp
             (@*activity* :vp)
             (let [this ^ArrayAdapter this]
               (track-view convertView
                           (proxy-super getItem ^Integer position))))))) l]))
(defn update-zoom-timeline-layout
  [^RelativeLayout zoom-layout timeline]
  ; TODO don't just add a new ruler
  ; (.removeViewAt zoom-layout 0)
  ; (.addView zoom-layout ^View (time-ruler) 0)
  ; Can't use thread-first, hard to type-hint
  (let [^HorizontalScrollView scrollview (.getChildAt zoom-layout 1)
        ^ListView lv (.getChildAt scrollview 0)]
    ; TODO find a more subtle way to invalidate these
    (update-timeline-adapter (.getAdapter lv) (.getTag lv) timeline))
  (.invalidate zoom-layout))

; TODO figure out how to trigger viewport modification from here
; Really not sure what right way to structure is...
(defn zoom-timeline-layout
  [modify-viewport-fn]
  (let [rescale-from-detector
        (fn [^ScaleGestureDetector detector]
          (log-i (str "scale by factor "
                      (.getScaleFactor detector)))
          (modify-viewport-fn
            (vp-scaler (.getScaleFactor detector)
                       (.getFocusX detector))))
        ^ScaleGestureDetector sc-detector
        (ScaleGestureDetector.
          ^Context *activity*
          ^ScaleGestureDetector$OnScaleGestureListener
          (proxy [ScaleGestureDetector$OnScaleGestureListener] []
            (onScaleBegin [sc-detector] (rescale-from-detector sc-detector))
            (onScale [sc-detector] (rescale-from-detector sc-detector))
            (onScaleEnd [sc-detector] (rescale-from-detector sc-detector))))
        ^GestureDetector detector
        (GestureDetector.
          ^Context *activity*
          ^GestureDetector$SimpleOnGestureListener
          (proxy [GestureDetector$SimpleOnGestureListener] []
            (onScroll [e1 e2d dx dy]
              (modify-viewport-fn (vp-translater-pix dx)))))
        ^Activity local-act *activity*]
    (proxy [LinearLayout] [local-act]
      ; TODO do something more subtle than steal all events
      (onInterceptTouchEvent [^MotionEvent ev]
        (with-activity local-act
          (log-i "scaling-detecting proxy [LinearLayout] onTouchEvent")
          (.onTouchEvent sc-detector ev)
          (.isInProgress sc-detector)))
      (onTouchEvent [^MotionEvent ev]
        (with-activity local-act
          (log-i "scaling-detecting proxy [LinearLayout] onTouchEvent")
          ; TODO do we need to check the return values of these?
          (.onTouchEvent sc-detector ev)
          (.isInProgress sc-detector))
        ; (.onTouchEvent detector ev)
        ; Do we need to give proxy-super a shot?
        ; (let [^LinearLayout this this]
        ;  (proxy-super onTouchEvent ev))
        ))))


; TODO TODO TODO I'm [pretty sure the zooming does not stay centered on the
; focal point because the scrollview scrolls without updating the viewport ms
; offset.  So either update that as scrolling happens or figure out a way not
; to need it.

; For new, traverse whole structure and update all ui elements to match
; This may/may not scale.
; Hint, it doesn't scale.
(defn update-ui-to-timeline
  [modify-viewport-fn ui timeline vp]
  (let [act *activity*]
    (with-vp vp
      (doto
        (if ui
          ui ; TODO update path
          (doto ^LinearLayout (zoom-timeline-layout modify-viewport-fn)
            (.setOrientation LinearLayout/VERTICAL)
            (.addView (time-ruler))
            (.addView 
              (doto (ObservableScrollView. *activity*)
                (.setFocusable false)
                (.setClickable false)
                (.setHorizontalScrollBarEnabled false)
                (.setObservableScrollViewListener 
                  (proxy [ObservableScrollView$ObservableScrollViewListener] []
                    (onScrollChanged [l t ol ot]
                      (with-activity (? act)
                        (modify-viewport-fn
                          (fn [ovp]
                            (? l )
                            (assoc ovp :ms-window-start
                                   (scale-pix-to-ms l :vp ovp))))))))
                (.addView
                  (let [[adapter l] (make-timeline-adapter)]
                    (doto (ListView. *activity*)
                      (.setFocusable false)
                      (.setClickable false)
                      (.setAdapter adapter)
                      (.setTag l))))))))
        (update-zoom-timeline-layout timeline)))))
