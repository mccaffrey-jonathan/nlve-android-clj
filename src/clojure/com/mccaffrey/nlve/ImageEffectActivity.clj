(ns com.mccaffrey.nlve.ImageEffectActivity
  (:use neko.find-view)
  (:use neko.listeners.view)
  (:use neko.log)
  (:gen-class :main false
              :extends android.app.Activity
              :exposes-methods {onCreate superOnCreate})
  (:import [com.mccaffrey.nlve R$layout])
  (:import [android.graphics SurfaceTexture])
  (:import [android.hardware Camera])
  (:import [android.media.effect EffectContext EffectFactory])
  (:import [android.opengl GLES20 GLES10 GLES11Ext])
  (:import [android.view Surface])
  (:import [javax.microedition.khronos.egl EGLContext EGL10 EGL]))

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

(defn is-egl?
  [maybe-egl]
  (instance? EGL maybe-egl))

(defmacro with-egl
  "Evaluates body such that egl is bound to *egl*"
  [egl & body]
  `(let [egl# ~egl]
     (binding [*egl* egl#]
       ~@body)))

(defn egl-attrib-list
  [attrib-map]
  (int-array (conj (vec (flatten attrib-map)) EGL10/EGL_NONE)))

; if egl isn't specified, *egl* is checked
(defn egl-choose-config
  [egl-display attrib-map & {egl :egl :or *egl*}]
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
    (let [display (.eglGetDisplay *egl* EGL10/EGL_DEFAULT_DISPLAY)]
      (.eglInitialize
        *egl*
        display
        (int-array 2))
      (.eglCreateContext
        *egl*
        display
        ((egl-choose-config *egl* display attrib-map) 0)
        EGL10/EGL_NO_CONTEXT
        (egl-attrib-list {EGL_CONTEXT_CLIENT_VERSION 2})))))

(defn -onCreate
  [this bundle]
  (doto this
    (.superOnCreate bundle)
    (.setContentView R$layout/preview))
  (doto (find-view this :preview-view)
        (.setSurfaceTextureListener
          (let [egl-ctx
                (init-egl
                  {EGL10/EGL_SURFACE_TYPE EGL10/EGL_WINDOW_BIT,
                   EGL10/EGL_RENDERABLE_TYPE EGL_OPENGL_ES2_BIT})
              cam-id 0
              cam (Camera/open cam-id)
              [w h] (get-preview-size cam)
              preview-tex (make-texture)
              display-tex (make-texture)
              preview-st (SurfaceTexture. preview-tex)
              effect (.. 
                       (EffectContext/createWithCurrentGlContext)
                       (getFactory)
                       (createEffect EffectFactory/EFFECT_TEMPERATURE))]
          (.setOnFrameAvailable
            preview-st
            (reify android.graphics.SurfaceTexture$OnFrameAvailableListener
              (onFrameAvailable [_ st]
                (.apply effect preview-tex w h display-tex))))
          (reify android.view.TextureView$SurfaceTextureListener
            (onSurfaceTextureAvailable [_ st _ _]
              (log-i "Setting preview texture for camera")
              (.applyToGLContext st display-tex)
              (doto cam
                (.setPreviewTexture preview-st)
                (.setDisplayOrientation
                  (front-facing-camera-correction 
                    cam-id (get-display-rotation this)))
                (.startPreview)))
            (onSurfaceTextureDestroyed [_ st]
              (true?
                (doto cam
                  (.stopPreview)
                  (.release))))
            (onSurfaceTextureUpdated [_ _] 
              nil))))))

;    (.setContentView R$layout/main))
;  (.setOnClickListener
;    (find-view this :button-0)
;    (on-click-call
;      #(.setText %1 "Now it's on"))))
;
