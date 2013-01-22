(ns com.mccaffrey.nlve.ImageEffectActivity
  (:gen-class :main false
              :extends android.app.Activity
              :exposes-methods {onCreate superOnCreate})
  (:import [android R]
           [android.app Activity]
           [android.graphics SurfaceTexture]
           [android.hardware Camera]
           [android.media.effect EffectContext EffectFactory]
           [android.opengl GLSurfaceView GLES20 GLES10 GLES11Ext]
           [android.os Bundle]
           [android.view Surface]
           [com.mccaffrey.nlve R$layout]
           [javax.microedition.khronos.egl EGLContext EGL10 EGL])
  (:use [neko context find-view log activity]
        [neko.listeners view]))

(deflog "ImageEffectActivity")

(defn get-display-rotation
  [activity]
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
    (Camera/getCameraInfo cam-id info)
    info))

(defn front-facing-camera-correction
  [cam-id degrees]
  (let [info (get-camera-info cam-id)]
    (cond
      (= (.facing info) android.hardware.Camera$CameraInfo/CAMERA_FACING_FRONT) 
      (mod (- 360 (mod (+ (.orientation info) degrees) 360)))
      :default
      (mod (+ (- (.orientation info) degrees) 360) 360))))

(defn gen-texture
  []
  (let [textures (make-array Integer/TYPE 1)]
    (GLES20/glGenTextures 1 textures 0)
    (nth textures 0)))

(defn make-texture 
  []
  (let [name (gen-texture)]
    (doto (GLES11Ext/GL_TEXTURE_EXTERNAL_OES)
      (GLES20/glBindTexture name)
      (GLES20/glTexParameterf GLES20/GL_TEXTURE_MIN_FILTER GLES20/GL_LINEAR)
      (GLES20/glTexParameterf GLES20/GL_TEXTURE_MAG_FILTER GLES20/GL_LINEAR)
      (GLES20/glTexParameteri GLES20/GL_TEXTURE_WRAP_S GLES20/GL_CLAMP_TO_EDGE)
      (GLES20/glTexParameteri GLES20/GL_TEXTURE_WRAP_T GLES20/GL_CLAMP_TO_EDGE))
    name))

(defn get-preview-size
  [cam] 
  (let [sz (.. cam
             (getParameters)
             (getPreviewSize))]
    [(.width sz) (.height sz)]))

; (defn request-window-features!
;   "Requests the given features for the activity.  The features should be
;   keywords such as :no-title or :indeterminate-progress corresponding
;   FEATURE_NO_TITLE and FEATURE_INDETERMINATE_PROGRESS, respectively.  Returns a
;   sequence of boolean values corresponding to each feature, where a true value
;   indicates the requested feature is supported and now enabled.
; 
;   If within a with-activity form, supplying an activity as the first argument
;   is not necessary.
; 
;   This function should be called before set-content-view!."
;   {:arglists '([& features] [activity & features])}
;   [activity & features]
;   {:pre  [(or (activity? activity)
;               (and (keyword? activity)
;                    (has-*activity*?)))
;           (every? keyword? features)]
;    :post [%
;           (every? (fn [x] (instance? Boolean x)) %)]}
;  (let [[^Activity activity features]
;          (if (instance? Activity activity)
;            [activity features]
;            [*activity* (cons activity features)])

(def 
  ^{:doc "The current EGL to operate on"
    :dynamic true}
  *egl*)

(def 
  ^{:doc "The current EGL display to operate on"
    :dynamic true}
  *egl-display*)

(defmacro with-egl
  "Evaluates body such that egl is bound to *egl*"
  [egl & body]
  `(let [egl# ~egl]
     (binding [*egl* egl#]
       ~@body)))

(defmacro with-egl-display
  "Evaluates body such that egl is bound to *egl*"
  [egl-display & body]
  `(let [egl-display# ~egl-display]
     (binding [*egl-display* egl-display#]
       ~@body)))

(defn egl-attrib-list
  [attrib-map]
  (int-array (conj (vec (flatten attrib-map)) EGL10/EGL_NONE)))

; if egl isn't specified, *egl* is checked
(defn egl-choose-config
  [attrib-map & {:keys [egl egl-display]
                 :or {egl *egl* egl-display *egl-display*}}]
  (let [max-configs 10
        config-arr (int-array max-configs)
        num-config (int-array 1)]
    (.eglChooseConfig
      egl
      egl-display
      (egl-attrib-list attrib-map)
      config-arr
      (count config-arr)
      num-config)
    (vec (take (nth num-config 0) config-arr))))

(def EGL_CONTEXT_CLIENT_VERSION 0x3098)
(def EGL_OPENGL_ES2_BIT 0x4)

(defn init-egl
  [attrib-map]
  (with-egl
    (EGLContext/getEGL)
    (with-egl-display 
      (.eglGetDisplay *egl* EGL10/EGL_DEFAULT_DISPLAY)
      (.eglInitialize
        *egl*
        *egl-display*
        (int-array 2))
      (.eglCreateContext
        *egl*
        *egl-display*
        ((egl-choose-config attrib-map) 0)
        EGL10/EGL_NO_CONTEXT
        (egl-attrib-list {EGL_CONTEXT_CLIENT_VERSION 2})))))

(defn make-preview-renderer
  []
  (reify android.opengl.GLSurfaceView$Renderer
    (onDrawFrame 
      [_ gl10] 
      (GLES20/glClearColor 0.8 0.3 0.3 1.0)
      (GLES20/glClear GLES20/GL_COLOR_BUFFER_BIT))
    (onSurfaceChanged
     [_ gl10 w h] )
    (onSurfaceCreated
      [_ gl10 egl-cfg] )))

(defn -onCreate
  [^Activity this ^Bundle bundle] 
  (with-activity
    this
    (doto this
      (.superOnCreate bundle)
      (.setContentView (get-layout :preview)))
    (doto (find-view (get-id :preview_surface))
      (.setEGLContextClientVersion 2)
      (.setPreserveEGLContextOnPause false) ; start with the more bug-prone mode
      (.setEGLConfigChooser false) ; no depth
      (.setRenderer (make-preview-renderer ))
      (.setRenderMode GLSurfaceView/RENDERMODE_WHEN_DIRTY))))

;  (defn -onCreate
;    [this bundle]
;    (log-i "onCreate")
;    (doto this
;      (.superOnCreate bundle)
;      (.setContentView R$layout/preview))
;    (doto (find-view this :preview-view)
;      (.setSurfaceTextureListener
;        (log-i "set st listener")
;        (let [egl-ctx
;              (init-egl
;                {EGL10/EGL_SURFACE_TYPE EGL10/EGL_WINDOW_BIT,
;                 EGL10/EGL_RENDERABLE_TYPE EGL_OPENGL_ES2_BIT})
;              cam-id 0
;              cam (Camera/open cam-id)
;              [w h] (get-preview-size cam)
;              preview-tex (make-texture)
;              display-tex (make-texture)
;              preview-st (SurfaceTexture. preview-tex)
;              effect (.. 
;                       (EffectContext/createWithCurrentGlContext)
;                       (getFactory)
;                       (createEffect EffectFactory/EFFECT_TEMPERATURE))]
;          (.setOnFrameAvailable
;            (log-i "set preview onFrameAvailable")
;            preview-st
;            (reify android.graphics.SurfaceTexture$OnFrameAvailableListener
;              (onFrameAvailable [_ st]
;                (log-i "preview frame available")
;                (.apply effect preview-tex w h display-tex))))
;          (reify android.view.TextureView$SurfaceTextureListener
;            (onSurfaceTextureAvailable [_ st _ _]
;              (log-i "Setting preview texture for camera")
;              (.applyToGLContext st display-tex)
;              (doto cam
;                (.setPreviewTexture preview-st)
;                (.setDisplayOrientation
;                  (front-facing-camera-correction 
;                    cam-id (get-display-rotation this)))
;                (.startPreview)))
;            (onSurfaceTextureDestroyed [_ st]
;              (true?
;                (doto cam
;                  (.stopPreview)
;                  (.release))))
;            (onSurfaceTextureUpdated [_ _] 
;              nil)))))))

  ;    (.setContentView R$layout/main))
;  (.setOnClickListener
;    (find-view this :button-0)
;    (on-click-call
;      #(.setText %1 "Now it's on"))))
;
