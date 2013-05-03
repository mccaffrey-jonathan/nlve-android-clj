(ns com.mccaffrey.nlve.ImageEffectActivity
  (:gen-class :main false
              :init init
              :implements   [clojure.lang.IDeref]
              :state state
              :extends android.app.Activity
              :constructors {[] []}
              :exposes-methods {onCreate superOnCreate})
  (:import
    [android R]
    [android.app Activity FragmentManager FragmentTransaction ListFragment]
    [android.content Context]
    [android.graphics Color SurfaceTexture]
    [android.hardware Camera]
    [android.media MediaPlayer]
    [android.media.effect Effect EffectContext EffectFactory]
    [android.net Uri]
    [android.opengl GLSurfaceView GLES20 GLES10 GLES11Ext]
    [android.os Bundle]
    [android.view 
     MotionEvent
     View
     ViewGroup
     ViewGroup$LayoutParams
     ViewGroup$MarginLayoutParams
     ScaleGestureDetector
     ScaleGestureDetector$OnScaleGestureListener
     Surface]
    [android.widget
     ArrayAdapter
     ListView
     LinearLayout
     FrameLayout$LayoutParams
     RelativeLayout
     RelativeLayout$LayoutParams
     TextView
     VideoView]
    [com.mccaffrey.nlve R$id R$layout]
    [java.io File]
    [java.nio ByteBuffer IntBuffer ByteOrder]
    [javax.microedition.khronos.egl EGLContext EGL10 EGL]
    [java.util ArrayList Collection])
  (:use [neko activity context find-view log resource ui]
        [neko.listeners view]
        [neko.ui mapping]
        [com.mccaffrey.utils general gl media media-effects youtube]))

(set! *warn-on-reflection* true)

(set-classname! :gl-surface-view android.opengl.GLSurfaceView :inherits :view)
(set-classname! :seek-bar android.widget.SeekBar :inherits :view)

(deflog "ImageEffectActivity")

;(def debug-device :tablet)
 (def debug-device :phone)

; phone
(def local-movie-path "/storage/emulated/0/DCIM/Camera/VID_20130301_175709.mp4")
(def local-phone-movie-two "content://media/external/video/media/8113")
(def phone-movies
  [local-movie-path
   local-phone-movie-two
   "content://media/external/video/media/6487"
   "content://media/external/video/media/463"
   "content://media/external/video/media/3990"
   "content://media/external/video/media/3476"
   "content://media/external/video/media/3375"])

; tablet
(def local-content-path "content://media/external/video/media/11205")
(def assassins-creed-path "content://media/external/video/media/11204")
(def tablet-movies
  [local-content-path
   assassins-creed-path])

(def ^{:dynamic true} *vp*)


;(defn string-seq-fragment
;  [seq-of-strings] 
;  (proxy [ListFragment] []
;    (onActivityCreated [^Bundle savedInstanceState] 
;      (proxy-super onActivityCreated savedInstanceState)
;      (proxy-super setListAdapter 
;                   (ArrayAdapter.
;                     *activity*
;                     android.R$layout/simple_list_item_1
;                     (to-array seq-of-strings)))
;      (.setLayoutParams
;        (proxy-super getListView)
;        (FrameLayout$LayoutParams.
;                   ViewGroup$LayoutParams/FILL_PARENT
;                   ViewGroup$LayoutParams/WRAP_CONTENT)))
;    (onListItemClick [l v pos id] 
;      ; Do something with it
;      )))

(defn remap-range
  [[lo-in hi-in] [lo-out hi-out] in]
  (+ (* (/ (- in lo-in)
           (- hi-in lo-in))
        (- hi-out lo-out))
     lo-out))

; TODO include some linking meta-data?
(def simple-pos-tex-vs
  "attribute vec2 aPosition;
  attribute vec2 aTexcoord0;

  varying vec2 vTexcoord0;

  void main() {
  gl_Position = vec4(aPosition, 0, 1);
  // Check to pass texcoord from v -> f
  //vTexcoord0 = vec2(1.0, 0.0);
  vTexcoord0 = aTexcoord0;
  }")

; Seems like it's going to be hard to keep up...
(def layout [:linear-layout {:id-holder true
                             :orientation :vertical
                             :layout-width :fill-parent
                             :layout-height :fill-parent}
             [:text-view {:text "Saturation"
                          :layout-width :wrap-content
                          :layout-height :wrap-content}]
             [:seek-bar {:id ::saturation-bar
                         :layout-width :fill-parent
                         :layout-height :wrap-content}]
             [:text-view {:text "Fade!"
                          :layout-width :wrap-content
                          :layout-height :wrap-content}]
             [:seek-bar {:id ::fade-bar
                         :layout-width :fill-parent
                         :layout-height :wrap-content}]
             [:text-view {:text "Seek first!"
                          :layout-width :wrap-content
                          :layout-height :wrap-content}]
             [:seek-bar {:id ::seek-0
                         :layout-width :fill-parent
                         :layout-height :wrap-content}]
             [:text-view {:text "Seek second!"
                          :layout-width :wrap-content
                          :layout-height :wrap-content}]
             [:seek-bar {:id ::seek-1
                         :layout-width :fill-parent
                         :layout-height :wrap-content}]
             [:gl-surface-view {:id ::preview-surface
                                :layout-width :wrap-content
                                :layout-height :wrap-content
                                :visibility :gone}]])

(make-unary-ons frame-available
                android.graphics.SurfaceTexture$OnFrameAvailableListener
                onFrameAvailable
                [this surface-texture])

(defn get-preview-size
  [^Camera cam] 
  (let [sz (.. cam
             (getParameters)
             (getPreviewSize))]
    [(.width sz) (.height sz)]))

(defn get-display-rotation
  [^Activity activity]
  (let [rotation (.. activity
                   getWindowManager
                   getDefaultDisplay
                   getRotation)]
    (cond 
      (= rotation Surface/ROTATION_0) 0
      (= rotation Surface/ROTATION_90) 90
      (= rotation Surface/ROTATION_180) 180
      (= rotation Surface/ROTATION_270) 270
      :default 0)))

(defn get-camera-info
  [cam-id]
  (let [info (android.hardware.Camera$CameraInfo.)]
    (Camera/getCameraInfo cam-id info) info))

(defn front-facing-camera-correction
  [cam-id degrees]
  (let [info ^android.hardware.Camera$CameraInfo
        (get-camera-info cam-id)]
    (cond
      (= (.facing info) android.hardware.Camera$CameraInfo/CAMERA_FACING_FRONT) 
      (mod (- 360 (mod (+ (.orientation info) degrees) 360)))
      :default
      (mod (+ (- (.orientation info) degrees) 360) 360))))

(defprotocol state-passing-renderer
  "Wrapper for GLSurfaceView$Renderer that passes a state map in and returns an
  updates version."
  (onDrawFrame [self state gl10])
  (onSurfaceChanged [self state gl10 w h])
  (onSurfaceCreated [self state gl10 egl-cfg]))

(defn wrap-state-passing-renderer
  [inner]
  (let [state (ref {})]
    (reify android.opengl.GLSurfaceView$Renderer
      (onDrawFrame 
        [_ gl10]  
        (dosync (ref-set state (onDrawFrame inner @state gl10))))
      (onSurfaceChanged
        [_ gl10 w h] 
        (dosync (ref-set state (onSurfaceChanged inner @state gl10 w h))))
      (onSurfaceCreated
        [_ gl10 egl-cfg] 
        (dosync (ref-set state (onSurfaceCreated inner @state gl10 egl-cfg)))))))

(def scale (ref 50))
(def fade (ref 1))
(def seeks (vec (for [_ (range 2)]
                 (ref nil))))

(defn make-preview-renderer
  [^GLSurfaceView gl-surface-view
   video-streams
   rotation]
  (wrap-state-passing-renderer
    (reify state-passing-renderer
      (onDrawFrame 
        [_ state gl10] 
        (log-i "onDrawFrame")
        (.setParameter ^Effect (state :saturate)
                       "scale"
                       (float (- (* (/ (float @scale) 100.) 2.) 1.)))
                       ; ^Float ((float 0))
                       ;(float (Math/sin (state :time))))
        ; TODO remove this!!!!
        ; SCARY! TRANSACTION + MAJOR SIDE EFFECT!
        ; BAAAAD CLOJURE!!!!
        (dosync 
          (letfn [(seek-to [perc ^MediaPlayer mp]
                    (if perc (.seekTo mp (-> (.getDuration mp)
                                           (* perc)
                                           (/ 100)
                                           int))
                      nil))]
            (doseq [[seek-ref strm] (map vector seeks (state :tex-streams))]
              (alter seek-ref seek-to (strm :mp)))))
        (doseq [strm (state :tex-streams)]
          (.updateTexImage ^SurfaceTexture (strm :st)))
        ; TODO make this at least kinda functionaly
 
        ; Probably some kind of declarative renderer? !
        (copy-sampler-external-to-tex
          (get-in state [:tex-streams 0 :tex])
          (state :progs)
          (state :fbo))
        (copy-sampler-external-to-tex
          (get-in state [:tex-streams 1 :tex])
          (state :progs)
          (state :fbo)
          :opacity (float (/ (float @fade) 100)))
        ; Extract apply to a fn
        (apply-effect
          (state :saturate)
          ; repeated drilling into state is ugly, should clean up
          ; Also type hints suck, I'd like to push em into function decls
          (state :buffer-tex)
          (state :display-tex))
        ; (print-gl-errors-checked)
        ; Restore state the the media effect may have mucked up, and clear
        (gl-calls (GLES20/glBindFramebuffer GLES20/GL_FRAMEBUFFER 0)
                  (GLES20/glViewport 0 0 (state :w) (state :h))
                  (GLES20/glScissor 0 0 (state :w) (state :h))
                  (GLES20/glClearColor 0.8 0.3 0.3 1.0)
                  (GLES20/glClear GLES20/GL_COLOR_BUFFER_BIT))
        (with-program (get-in state [:progs :simple])
                      (draw-fs-tex-rect GLES20/GL_TEXTURE_2D 
                                        (state :display-tex)))

        ; This isn't perfect, it'll update the state after rendering... 
        ; move it ahead somehow?
        (update-in state [:time] (partial + 0.1))) 

      (onSurfaceChanged
        [_ state gl10 w h] 
        (log-i "onSurfaceChanged")
        (assoc state
               :w w
               :h h))
      (onSurfaceCreated
        [renderer state gl10 egl-cfg] 
        (log-i "onSurfaceCreated")
        (let [; cam-id 0
              ; cam (Camera/open cam-id)
              ; [w h] (get-preview-size cam)
              preview-tex (make-2x2-texture)
              display-tex (gen-texture)
              saturate ^Effect (-> (EffectContext/createWithCurrentGlContext)
                                 .getFactory
                                 (make-effect EffectFactory/EFFECT_SATURATE))

              external-blt-with-opacity-prg
              (load-program
                simple-pos-tex-vs
                ; frag shader
                "
                #extension GL_OES_EGL_image_external : require

                precision mediump float;
                varying vec2 vTexcoord0;
                uniform samplerExternalOES uSampler;
                uniform float uOpacity;

                void main() {
                gl_FragColor = texture2D(uSampler, vTexcoord0);
                gl_FragColor.a *= uOpacity;

                // gl_FragColor = vec4(0, 1, 0.2, 1);
                // gl_FragColor = syntax err
                // Check to make sure texcoord is valid
                // gl_FragColor = vec4(vTexcoord0, 1, 1);
                }")
              external-blt-prg
              (load-program
                simple-pos-tex-vs
                ; frag shader
                "
                #extension GL_OES_EGL_image_external : require

                precision mediump float;
                varying vec2 vTexcoord0;
                uniform samplerExternalOES uSampler;

                void main() {
                gl_FragColor = texture2D(uSampler, vTexcoord0);
                // gl_FragColor = vec4(0, 1, 0.2, 1);
                // gl_FragColor = syntax err
                // Check to make sure texcoord is valid
                // gl_FragColor = vec4(vTexcoord0, 1, 1);
                }")
              simple-prg
              (load-program
                simple-pos-tex-vs
                ; frag shader
                "precision mediump float;
                varying vec2 vTexcoord0;
                uniform sampler2D uSampler;

                void main() {
                gl_FragColor = texture2D(uSampler, vTexcoord0);
                // gl_FragColor = vec4(0, 1, 0.2, 1);
                // gl_FragColor = syntax err
                // Check to make sure texcoord is valid
                // gl_FragColor = vec4(vTexcoord0, 1, 1);
                }")
              [fbo buffer-tex] (fbo-tex
                                 GLES20/GL_TEXTURE_2D
                                 GLES20/GL_RGBA
                                 GLES20/GL_UNSIGNED_BYTE
                                 512
                                 512)]
          ; Enabling GL_TEXTURE_2D is not needed in ES 2.0.
          ; (gl-calls (GLES20/glEnable GLES20/GL_TEXTURE_2D))

          (.setParameter ^Effect saturate
                         "scale"
                         ^Float (float 0))
          ;           (doto cam
          ;             (.setPreviewTexture preview-st)
          ;             ; (.setDisplayOrientation
          ;             ;   (front-facing-camera-correction cam-id rotation))
          ;             (.startPreview))w
          ;

          (assoc state
                 :saturate saturate
                 :preview-tex preview-tex
                 :display-tex display-tex
                 ; TODO Make this associative by index without conv to vec?
                 :tex-streams (vec (for [stream video-streams]
                                (let [tex (assoc (gen-texture)
                                                :width 512
                                                :height 512)]
                                  (doto ^SurfaceTexture (stream :st)
                                    (.detachFromGLContext)
                                    (.attachToGLContext (tex :name))
                                    (.setOnFrameAvailableListener
                                      (on-frame-available 
                                        (log-i "video frame available")
                                        ; TODO this apparently not safe,
                                        ; as on-frame-available can be called on any thread.
                                        ; (.updateTexImage video-st)
                                        (.requestRender gl-surface-view))))
                                  (assoc stream :tex tex))))
                 ; Replace keys with attribute maps?
                 :progs {:simple simple-prg
                        :external-blit external-blt-prg
                        :external-blit-with-opacity external-blt-with-opacity-prg}
                 :fbo fbo
                 :buffer-tex buffer-tex
                 :time 0
                 ))))))

(defn setup-video-view [^VideoView video-view]
  (fn [uri] 
    (doto video-view
      (.setOnErrorListener
        (on-media-player-error-call log-mp-error))
      (.setOnInfoListener
        (on-media-player-info-call log-mp-info))
      (.setOnPreparedListener
        (on-media-player-prepared
          (log-i "MediaPlayer prepared")))
      (.setOnCompletionListener
        (on-media-player-completion-call log-mp-completion))
      (.setVideoURI uri)
      (.start))))

; PICKUP setup mediaplayer for URI and plug it up to a surface-texture
; and then to a texture for most excellent GL filtering!
(defn media-player-for-uri [ctx surface-texture uri]
  (? ctx)
  (? surface-texture)
  (? uri)
  (doto ^MediaPlayer (MediaPlayer.)
    (.setOnErrorListener
      (on-media-player-error-call log-mp-error))
    (.setOnInfoListener
      (on-media-player-info-call log-mp-info))
    (.setOnCompletionListener
      (on-media-player-completion-call log-mp-completion))
    (.setOnPreparedListener
      (on-media-player-prepared-call
        (generic-log-fn "OnMediaPlayerPrepared")))
    (.setOnBufferingUpdateListener
      (on-media-player-buffering-update-call
        (generic-log-fn "OnMediaPlayerBufferingUpdate")))
    (.setOnSeekCompleteListener
      (on-media-player-seek-complete-call
        (generic-log-fn "OnMediaPlayerSeekComplete")))
    (.setOnTimedTextListener
      (on-media-player-timed-text-call
        (generic-log-fn "OnMediaPlayerTimedText")))
    (.setOnVideoSizeChangedListener
      (on-media-player-video-size-changed-call
        (generic-log-fn "OnMediaPlayerVideoSizeChanged")))
    (.setDataSource ctx uri)
    (.setSurface (Surface. surface-texture))
    ; TODO use async version
    (.prepare)
    (.start)))
; TODO make these w/o texture name?

(defn setup-filter-view [^Activity ctx ^GLSurfaceView preview-surface]
  (fn [uris]
    (log-i "Setup Filter View!")
    (with-activity
      ; TODO make these w/o texture name?
      ctx
        (doto preview-surface
          (.setEGLContextClientVersion 2)
          (.setPreserveEGLContextOnPause false) ; start with the more bug-prone mode
          (.setEGLConfigChooser false) ; no depth
          ; Annoyingly, this is broken for ES2 funcs
          ; (.setDebugFlags GLSurfaceView/DEBUG_CHECK_GL_ERROR)
          (.setRenderer (make-preview-renderer
                          preview-surface
                          (for [uri uris]
                            (let [st (SurfaceTexture. 1)]
                              {:st st
                               :mp (media-player-for-uri ctx st uri)}))
                          (get-display-rotation *activity*)))
          (.setRenderMode GLSurfaceView/RENDERMODE_WHEN_DIRTY)
          ; (.setRenderMode GLSurfaceView/RENDERMODE_CONTINUOUSLY)
          ; Go!
          (.setVisibility View/VISIBLE)))))

(defn ref-updating-seek-bar-listener
  [ref-to-update]
  (reify android.widget.SeekBar$OnSeekBarChangeListener
    (onProgressChanged [this seek-bar progress from-user]
      (dosync (ref-set ref-to-update progress)))
    (onStartTrackingTouch [this seek-bar])
    (onStopTrackingTouch [this seek-bar])))

;(defn add-fragment
;  [ctx id frag]
;  (doto (-> (.getFragmentManager ctx) .beginTransaction)
;    ; TODO use neko ids
;    (.add id frag)
;    (.commit)))

; TODO is the order right for standard comparator semantics?
(defn cmp-start-ms
  [fst scd]
  (-
    (fst :start-ms)
    (scd :start-ms)))

(defn s2ms
  [s]
  (* 1000 s))

(defn min2ms
  [mn]
  (-> mn
    (* 60)
    s2ms))

; TODO
; A decent starting data-structure for tracks would be
; a vector of tracks,
; a track is mostly a sorted-map of start times to clips?
; Clip is 'resource' + resource offset + length for now
;
; TODO is txt field a keeper?  Maybe make fn...
; Model!
(def demo-timeline-structure
  [(into (sorted-set-by cmp-start-ms)
                  (for [s (range 10 400 10)]
                    {:start-ms (s2ms s) :length-ms (s2ms 5) :txt (str "T0C" s)}))
   (into (sorted-set-by cmp-start-ms)
                  (for [s (range 25 400 40)]
                    {:start-ms (s2ms s) :length-ms (s2ms 20) :txt (str "T1C" s)}))
   (sorted-set-by cmp-start-ms
                  {:start-ms (s2ms 18) :length-ms (s2ms 10) :txt "Clip 20"})])

; Just scale by width.  Useful for widths/measures/etc
(defn scale-ms-to-pix
  [ms]
  (-> ms
    (* (*vp* :pixel-window-width))
    (/ (*vp* :ms-window-width))
    (int)))

(defn transform-ms-to-pix
  [ms]
  (scale-ms-to-pix
    (- ms (*vp* :ms-window-start))))

; TODO something more data-bind-ey
; If that thing doesn't exit, be a good project
; Backbone or whatnot for android + clojure
(defn track-view
  [track]
  (do-let [layout (doto (RelativeLayout. *activity*)
                    (.setMinimumHeight 50))]
          (doseq [clip track]
            (.addView layout
                      (doto ^TextView (TextView. *activity*)
                        (.setText ^String (clip :txt))
                        (.setBackgroundColor Color/YELLOW)
                        (.setSingleLine true))
                      (do-let
                        [params (RelativeLayout$LayoutParams.
                                                ^Integer (scale-ms-to-pix
                                                           (clip :length-ms))
                                                ^Integer ViewGroup$LayoutParams/FILL_PARENT)]
                        (set! (. params leftMargin) 
                              (transform-ms-to-pix (clip :start-ms))))))))

; we use the zoom levels for ticks/labeling that are the appropriate size
; for our current zoom.
(def ruler-ticks-in-ms
  (sorted-set 
    100
   (s2ms 1)
   (s2ms 5)
   (s2ms 10)
   (s2ms 60)
   (min2ms 5)))

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
  (? *vp*)
  (let [tick-ms (tick-ms-for-viewport)
        start-tick (-> *vp*
                     :ms-window-start
                     (/ tick-ms)
                     (float)
                     (Math/ceil)
                     (* tick-ms))]
    (for [i (range)
          :let [ms-offset (+ start-tick (* i tick-ms))]
          :while (< ms-offset (*vp* :ms-window-width))]
      (+ ms-offset (*vp* :ms-window-start)))))

; TODO make some ViewportParams struct, i've been writing
; pixel-window ms-window
; alot.
(defn time-ruler []
  (do-let [rel (RelativeLayout. *activity*)]
    (doseq [ms (ticks-ms-for-viewport)]
      (.addView rel
        (doto (LinearLayout. *activity*)
          (.setOrientation LinearLayout/VERTICAL)
          ; TODO add a line here
          (.addView (doto ^TextView (TextView. *activity*)
                      (.setText ^String (short-pp-time ms)))))
          (do-let
            [params (RelativeLayout$LayoutParams. 
                                    ViewGroup$LayoutParams/WRAP_CONTENT
                                    ViewGroup$LayoutParams/WRAP_CONTENT)]
             ; TODO just setting left margin is going to push these to the side..
             (set! (. params leftMargin)
                   (transform-ms-to-pix ms)))))))

(defn update-timeline-adapter
  [^ArrayAdapter adapter timeline]
  (.clear adapter)
  (.addAll adapter ^Collection timeline))


  (defn update-zoom-timeline-layout
    [^ViewGroup zoom-layout timeline]
    ; Just add a new ruler?
    (.removeViewAt zoom-layout 0)
    (.addView zoom-layout ^View (time-ruler) 0)
    ; Can't use thread-first, hard to type-hint
    (update-timeline-adapter
      (.getAdapter
        ^ListView (.getChildAt zoom-layout 1))
      timeline)
    ; TODO find a more subtle way to invalidate these
    (.invalidate zoom-layout))

(defmacro with-vp
  [vp & body]
  `(binding [*vp* ~vp]
     ~@body))

(defn make-timeline-adapter
  []
  (let [act *activity*] 
    (proxy [ArrayAdapter] [^Context *activity*
                           0 ; textviewresid
                           ^List (ArrayList.)]
      (getView [position convertView parent]
        ; TODO change where seconds is calced
        (with-activity
          act
          (with-vp
            (@*activity* :vp)
            (let [this ^ArrayAdapter this]
              (track-view (proxy-super getItem ^Integer position)))))))))

; TODO should this be X-only?  It's doable.
; scale to keep the
; gestures focal point
; at the same spot
; unless we hit 0 ms
; that works out to 
; new-ms-offset = scale*ms-offset + ms-pos*(1.0f - scale)
; mx = px*(mw/pw) + mo

(defn transform-pix-to-ms
  [px & {:keys [vp] :or {vp *vp*}}]
  (+ (* px
        (/ (vp :ms-window-start)
           (vp :pixel-window-width)))
     (vp :ms-window-start)))

(defn vp-scaler
  [sf fx]
  (fn [vp]
    (assoc vp
           :pixel-window-width
           (* sf (vp :pixel-window-width))
           :ms-window-start
           (+ (* sf (vp :ms-window-start))
              (* (- 1.0 sf)
                 (transform-pix-to-ms fx :vp vp))))))

(declare modify-viewport)

; TODO figure out how to trigger viewport modification from here
; Really not sure what right way to structure is...
(defn zoom-timeline-layout []
  (let [rescale-from-detector
        (fn [^ScaleGestureDetector detector]
          (log-i (str "scale by factor "
                      (.getScaleFactor detector)))
          (modify-viewport
            (vp-scaler (.getScaleFactor detector)
                       (.getFocusX detector))))
        ^ScaleGestureDetector detector 
        (ScaleGestureDetector.
          *activity*
          (proxy [ScaleGestureDetector$OnScaleGestureListener] []
            (onScaleBegin [detector] (rescale-from-detector detector))
            (onScale [detector] (rescale-from-detector detector))
            (onScaleEnd [detector] (rescale-from-detector detector))))]
    (let [local-act *activity*]
      (proxy [LinearLayout] [*activity*]
        ; TODO do something more subtle than steal all events
        (onInterceptTouchEvent [^MotionEvent ev]
          (log-i "LinearLayout onInterceptTouchEvent")
          true)
        (onTouchEvent [^MotionEvent ev]
          (with-activity local-act
            (log-i "LinearLayout onTouchEvent")
            (.onTouchEvent detector ev)
            (let [^LinearLayout this this]
              (proxy-super onTouchEvent ev))
            true
            ))))))

; For new, traverse whole structure and update all ui elements to match
; This may/may not scale.
(defn update-ui-to-timeline
  [ui timeline vp]
  (with-vp
    vp
    (doto
      (if ui
        ui ; TODO update path
        (doto ^LinearLayout (zoom-timeline-layout)
          (.setOrientation LinearLayout/VERTICAL)
          (.addView (time-ruler))
          (.addView (doto (ListView. *activity*)
                      (.setAdapter (make-timeline-adapter))))))

      (update-zoom-timeline-layout timeline))))

(defmacro wrap-cb-with-bindings
  [cb & dyns]
  (let [syms (repeatedly (count dyns) gensym)]
    ; Use a let to shuttle some bindings to the inner closure
  `(let [~@(interleave syms dyns)]
     (fn [& args#]
       (binding
         [~@(interleave dyns syms)]
         (apply ~cb args#))))))

(defn update-ui-for-state []
    (swap! (.state *activity*) assoc :vc  
      (apply update-ui-to-timeline (map @(.state *activity*) [:vc :model :vp]))))

(defn modify-viewport
  [vp-fn]
  (swap! (.state *activity*)
         (fn [old-state]
           (do-let [new-state (update-in old-state [:vp] vp-fn)]
                   (log-i (str {:old (old-state :vp)
                                :new (new-state :vp)})))))
  ; TODO only do this if it really changed
  ; TODO better way to get handler...
  (-> ^View (:vc @*activity*)
    .getHandler
    (.post (wrap-cb-with-bindings update-ui-for-state *activity*))))

(defn -init
  []
  (log-i "Init called!")
  [[] (atom nil)])

; Cute trick to shorten addressing
(defn -deref
  [this]
  @(.state this))

(defn -onCreate
  [^Activity this ^Bundle bundle] 
  (with-activity this
    (log-i (str "this " this "bundle " bundle))
    (.superOnCreate this bundle)
    (log-i "post onCreate")
    (.setContentView this R$layout/timeline)
    ; Test video playback
    (log-i "onCreate")
    ; (add-fragment this R$id/fragment_spot )
    ;

    ; Start a base spot
    (reset! (.state this) {:vc nil
                           :model demo-timeline-structure
                           :vp {:pixel-window-width 700
                                :ms-window-start 0
                                :ms-window-width (s2ms 60)}})
    ; Save the initial UI
    (update-ui-for-state)
    ; Add the initial UI
    (.addView ^ViewGroup (find-view R$id/timeline_spot) ^View (@*activity* :vc))

; TODO re-enable media playback
;    ((setup-filter-view this (find-view R$id/preview_surface))
;      ;(Uri/parse local-content-path))
;      ; TODO make choice more dynamic
;       ;(Uri/fromFile (File. ^String local-movie-path))
;       (if (= debug-device :phone) [(Uri/fromFile (File. ^String local-movie-path))
;          (Uri/parse local-phone-movie-two)]
;         (map #(Uri/parse %) tablet-movies)))
      ;    (youtube-media-uri
      ;      example-id
      ;      ;(setup-video-view *activity*)
      ;      (setup-filter-view *activity*)
      ;      (fn [e res] (log-i (str e))))
    (log-i "AfterMediaURI")))

