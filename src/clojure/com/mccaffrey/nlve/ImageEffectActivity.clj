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
           [java.nio ByteBuffer IntBuffer ByteOrder]
           [javax.microedition.khronos.egl EGLContext EGL10 EGL])
  (:use [neko context find-view log activity]
        [neko.listeners view]
        [com.mccaffrey.utils general gl media youtube]))

(deflog "ImageEffectActivity")

(defn on-frame-available-call
  "Create an SurfaceTexture$OnFrameAvailableListener from a handler-fn."
  [handler-fn]
  {:pre  [(fn? handler-fn)]
   :post [(instance? android.graphics.SurfaceTexture$OnFrameAvailableListener %)]}
  (reify android.graphics.SurfaceTexture$OnFrameAvailableListener
    (onFrameAvailable [this surface-texture]
      (boolean (handler-fn surface-texture)))))

; Functions to deal with the camera parameters
(defmacro on-frame-available
  [& body]
  `(on-frame-available-call
     (fn [~'surface-texture] ~@body)))

(defn get-preview-size
  [cam] 
  (let [sz (.. cam
             (getParameters)
             (getPreviewSize))]
    [(.width sz) (.height sz)]))

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

(defn make-effect
  "Wrapper for MediaEffect creation"
  [factory str-name]
  {:pre [(EffectFactory/isEffectSupported str-name)]}
  (log-i (str "Making effect " str-name))
  (? (.createEffect factory str-name)))

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

(defn make-preview-renderer
  [gl-surface-view rotation]
  (wrap-state-passing-renderer
    (reify state-passing-renderer
      (onDrawFrame 
        [_ state gl10] 
        (.apply (state :saturate)
                (get-in state [:preview-tex :name])
                (get-in state [:preview-tex :width])
                (get-in state [:preview-tex :height])
                (get-in state [:display-tex :name]))
        ;        (log-i (str (nth (n-ints-from-buffer
        ;                      1
        ;                      (GLES20/glGetFramebufferAttachmentParameteriv
        ;                        GLES20/GL_FRAMEBUFFER
        ;                        GLES20/GL_COLOR_ATTACHMENT0
        ;                        GLES20/GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME)) 0)))
        ; Apply leave the framebuffer in a messy state.  Should come up with a more clojure-ey way to restore
        (print-gl-errors-checked)
        ; Restore state the the media effect may have mucked up, and clear
        (gl-calls (GLES20/glBindFramebuffer GLES20/GL_FRAMEBUFFER 0)
                  (GLES20/glViewport 0 0 (state :w) (state :h))
                  (GLES20/glScissor 0 0 (state :w) (state :h))
                  (GLES20/glClearColor 0.8 0.3 0.3 1.0)
                  (GLES20/glClear GLES20/GL_COLOR_BUFFER_BIT))
        (with-program (state :simple-prg)
                      (draw-fs-tex-rect (state :display-tex))))
      (onSurfaceChanged
        [_ state gl10 w h] 
        (assoc state
               :w w
               :h h))
      (onSurfaceCreated
        [renderer state gl10 egl-cfg] 
        (let [; cam-id 0
              ; cam (Camera/open cam-id)
              ; [w h] (get-preview-size cam)
              preview-tex (make-2x2-texture)
              display-tex (gen-texture)
              preview-st (doto (SurfaceTexture. (preview-tex :name))
                           (.setOnFrameAvailableListener
                             (on-frame-available
                               (log-i "preview frame incoming")
                               (.requestRender gl-surface-view))))
              saturate (-> (EffectContext/createWithCurrentGlContext)
                         .getFactory
                         (make-effect EffectFactory/EFFECT_SEPIA))
              simple-prg (load-program
                           ; vertex program
                           "attribute vec2 aPosition;
                           attribute vec2 aTexcoord0;

                           varying vec2 vTexcoord0;

                           void main() {
                           gl_Position = vec4(aPosition, 0, 1);
                           // Check to pass texcoord from v -> f
                           //vTexcoord0 = vec2(1.0, 0.0);
                           vTexcoord0 = aTexcoord0;
                           }"
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
                           }")]
          ; Enabling GL_TEXTURE_2D is not needed in ES 2.0.
          ; (gl-calls (GLES20/glEnable GLES20/GL_TEXTURE_2D))

          ; (.setParameter saturate "scale" (float -1))
          ;           (doto cam
          ;             (.setPreviewTexture preview-st)
          ;             ; (.setDisplayOrientation
          ;             ;   (front-facing-camera-correction cam-id rotation))
          ;             (.startPreview))
          (assoc state
                 :preview-tex preview-tex
                 :display-tex display-tex
                 :saturate saturate
                 :simple-prg simple-prg))))))

(defn setup-video-view [ctx]
  (fn [uri] 
    (with-activity
      ctx
      (doto (find-view (get-id :video_surf))
        (.setOnErrorListener
          (on-media-player-error-call throw-mp-error))
        (.setOnInfoListener
          (on-media-player-info-call log-mp-info))
        (.setOnPreparedListener
          (on-media-player-prepared
            (log-i "MediaPlayer prepared")))
        (.setOnCompletionListener
          (on-media-player-completion
            (log-i "MediaPlayer completion")))
        (.setVideoURI uri)
        (.start)))))

(defn -onCreate
  [^Activity this ^Bundle bundle] 
  (with-activity
    this
    (doto *activity*
      (.superOnCreate bundle)
      (.setContentView (get-layout :preview)))
    ; Test video playback
    ;
    (youtube-media-uri
      example-id "90" true
      (setup-video-view *activity*)
      (fn [e res] (throw e)))))

;    (let [preview-surface (find-view (get-id :preview_surface))]
;      (doto preview-surface
;        (.setEGLContextClientVersion 2)
;        (.setPreserveEGLContextOnPause false) ; start with the more bug-prone mode
;        (.setEGLConfigChooser false) ; no depth
;        ; Annoyingly, this is broken for ES2 funcs
;        ; (.setDebugFlags GLSurfaceView/DEBUG_CHECK_GL_ERROR)
;        (.setRenderer (make-preview-renderer
;                        preview-surface
;                        (get-display-rotation *activity*)))
;        (.setRenderMode GLSurfaceView/RENDERMODE_WHEN_DIRTY)))))
