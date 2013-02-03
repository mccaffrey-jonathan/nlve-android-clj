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
        [neko.listeners view]))

(deflog "ImageEffectActivity")

(defmacro ?
  [val]
  `(let [x# ~val]
      (log-i (str '~val '~'is x#))
      x#))


(defmacro do-let
  [[binding-form init-expr] & body]
  `(let [~binding-form ~init-expr]
     ~@body
     ~binding-form))

(defmacro do-rev
  "Kind of like do, but return the first expr in the body, instead of the last.
  Useful for appending logging/debug statements to calculations."
  [first-expr & rest]
  `(do-let [res# ~first-expr]
           ~@rest))

(defmacro n-ints-from-buffer
  [n body]
  (let [arr (gensym)
        gb (gensym)]
    `(do-let [~arr (int-array ~n)]
             (let [~gb (IntBuffer/wrap ~arr)]
               ~(if (seq? body)
                  `(~@body ~gb)
                  `(~body ~gb))))))

(def 
  ^{:doc "The current EGL to operate on"
    :dynamic true}
  *program*)

(def 
  ^{:doc "The current EGL to operate on"
    :dynamic true}
  *egl*)

(def 
  ^{:doc "The current EGL display to operate on"
    :dynamic true}
  *egl-display*)

(def 
  ^{:doc "Log GL compilation and linking errors."
    :dynamic true}
  *debug-gl-shader-log* true)

(def 
  ^{:doc "Obsessively check GL errors after each call.
          Disable for performance.
          Enable for sanity."
    :dynamic true}
  *debug-gl-check-errors* true)

(def 
  ^{:doc "Throw an exception whenever a GL error occurs."
    :dynamic true}
  *debug-exception-on-gl-errors* true)

(def 
  ^{:doc "Shrink fs rects to 3/4ths size of debugging"
    :dynamic true}
  *debug-shrink-fs-rects* true)

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

; Functions to deal with up-loading data to and from OpenGL
(def float-sz
  "Size in bytes of a floating point value"
   4)

(def int-sz
  "Size in bytes of an integer value"

  4)

(def ubyte-sz
  "Size in bytes of an unsigned-byte value.  I know it's obvious."
  1)

(defn ubyte-ify
  "Convert an numeric type to a byte in java, and clamp/shift the value
  appropriately so that when the 2's-complement signed byte is interpreted as an
  unsigned byte, it'll be the original value clamped to 0-255.

  Lack of unsigned bytes is a dubious Java design decision."
  [num]
  {:pre [(<=  num 255)
         (>= num 0)]
   :post [(instance? java.lang.Byte %)]}
  (byte (if (>= num 128) (- num 128) num)))

(defn vec-to-float-buffer
  [in]
  (doto (.asFloatBuffer
          (doto (ByteBuffer/allocateDirect (* (count in) float-sz))
            (.order (ByteOrder/nativeOrder))))
    (.put (float-array in))
    (.position 0)))

(defn vec-to-ubyte-buffer
  [in]
  (doto (ByteBuffer/allocateDirect (* (count in) ubyte-sz))
    (.order (ByteOrder/nativeOrder))
    (.put (byte-array (map ubyte-ify in)))
    (.position 0)))

; functions to deal with GL errors and debugging
(defn gl-get-actual-error
  "Get a non-trivial GL error."
  []
  (let [errno (GLES20/glGetError)]
    (if (not= errno GLES20/GL_NO_ERROR)
      (format "%02x" errno) nil)))

(defn print-gl-errors 
  "Print all current GL errors."
  [& op]
  (while
    (when-let [errno (gl-get-actual-error)]
      (if (and errno *debug-exception-on-gl-errors*)
        (throw (Throwable. (str "GL error: " errno))))
      (log-i (if op
               (str "GL errors after " op " " errno)
               (str "GL error: " errno))))))

(defn print-gl-errors-checked
  "Printf all current gl errors if 
  *debug-gl-check-errors* is true"
  []
  (if *debug-gl-check-errors* (print-gl-errors)))

(defmacro gl-calls
  "Wrap a sequence of GL call expression,
  optionally checking GL errors after each.
  Returns the value of the last expr."
  ([call-form]
   `(do-rev
      ~call-form
      (if *debug-gl-check-errors* (print-gl-errors))))
  ([call-form & more]
   `(do ~call-form
       (if *debug-gl-check-errors* (print-gl-errors))
       (gl-calls ~@more))))

; Functions to deal with creating and rendering OpenGL objects
(defn setup-attrib
  [kw]
  (log-i (str kw))
  (gl-calls (GLES20/glEnableVertexAttribArray (:location kw)))
  (apply #(GLES20/glVertexAttribPointer %1 %2 %3 %4 %5 %6)
         ((juxt :location :size :type :norm :stride :buffer) kw)))

(defn gen-texture
  "Return a texture name created by glGenTextures"
  []
  {:name 
   (let [textures (int-array 1)]
     (gl-calls (GLES20/glGenTextures 1 textures 0))
     (nth textures 0))})

(defn make-texture 
  [target]
  (do-let [tex (gen-texture)]
     (doto target
       (GLES20/glBindTexture (:name tex))
       ; TODO revisit filtering choice
       (GLES20/glTexParameterf GLES20/GL_TEXTURE_MIN_FILTER GLES20/GL_LINEAR)
       (GLES20/glTexParameterf GLES20/GL_TEXTURE_MAG_FILTER GLES20/GL_LINEAR)
       (GLES20/glTexParameteri GLES20/GL_TEXTURE_WRAP_S GLES20/GL_CLAMP_TO_EDGE)
       (GLES20/glTexParameteri GLES20/GL_TEXTURE_WRAP_T GLES20/GL_CLAMP_TO_EDGE))
     (print-gl-errors-checked)))

  (defn make-2x2-texture 
    []
    (assoc (do-let [tex (make-texture GLES20/GL_TEXTURE_2D)]
                   (gl-calls 
                     (GLES20/glBindTexture GLES20/GL_TEXTURE_2D (tex :name))
                     (GLES20/glTexImage2D
                       GLES20/GL_TEXTURE_2D
                       0
                       GLES20/GL_RGBA
                       2
                       2
                       0
                       GLES20/GL_RGBA
                       GLES20/GL_UNSIGNED_BYTE
                       (vec-to-ubyte-buffer
                         [255 255 255 255
                          255   0   0 255
                          0   255   0 255
                          0     0 255 255]))))
           ;        [0 0 0 255
           ;         0   0   0 255
           ;         0   0   0 255
           ;         0     0 0 255]
           :width 2 :height 2))

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

(def fs-pos-attrib
  {:size 2
   :type GLES20/GL_FLOAT
   :norm false
   :stride 0
   :buffer (vec-to-float-buffer
             (if *debug-shrink-fs-rects*
               [-0.75 -0.75,  -0.75,  0.75,  0.75  0.75,  0.75 -0.75]
               [-1. -1.,  -1.   1.,  1.  1.,  1. -1.]))})

(def fs-tex-attrib
  {:size 2
   :type GLES20/GL_FLOAT
   :norm false
   :stride 0
   :buffer (vec-to-float-buffer
             [0. 0.
              0. 1.
              1. 1.
              1. 0.])})

(defmacro with-program
  "Calls useProgram and binds *program* to program"
  [program & body]
  `(let [program# ~program]
     (gl-calls (GLES20/glUseProgram program#))
     (binding [*program* program#]
       ~@body)))

(defn assoc-location
  "Associate an attribute location to a map representing a vertex attribute."
  [kw loc-str]
  (assoc kw :location (gl-calls (GLES20/glGetAttribLocation *program* loc-str))))

(defn setup-texture
  "Setup a texture for rendering with a program."
  [tex unit uniform-name]
  (gl-calls 
    (GLES20/glActiveTexture (+ GLES20/GL_TEXTURE0 unit))
    (GLES20/glBindTexture GLES20/GL_TEXTURE_2D (tex :name))
    (GLES20/glUniform1i
      (GLES20/glGetUniformLocation *program* uniform-name)
      unit)))

(defn draw-fs-tex-rect
  "Draw a given texture on full-screen quad."
  [tex]
  (setup-texture tex 0 "uSampler")
  (setup-attrib (assoc-location fs-pos-attrib "aPosition"))
  (setup-attrib (assoc-location fs-tex-attrib "aTexcoord0"))
  (gl-calls (GLES20/glDrawArrays GLES20/GL_TRIANGLE_FAN 0 4)))

(defn load-shader
  "Create a shader of a given type from a string of source code,
  returning the compiled shader object."
  [type-code src]
  (do-let
    [shader (doto (GLES20/glCreateShader type-code)
              (GLES20/glShaderSource src)
              (GLES20/glCompileShader))]
    (print-gl-errors-checked)
    (when (and *debug-gl-shader-log*
               (= GLES20/GL_FALSE
                  (n-ints-from-buffer 1
                    (GLES20/glGetShaderiv shader GLES20/GL_COMPILE_STATUS))))
      (log-i (GLES20/glGetShaderInfoLog shader)))))

(defn load-program
  "Create a program from vertex and fragment shader source code,
  returning the linked program object."
  [vtx-src frg-src]
  (do-let [program
           (doto (GLES20/glCreateProgram)
             (GLES20/glAttachShader (load-shader GLES20/GL_VERTEX_SHADER vtx-src))
             (GLES20/glAttachShader (load-shader GLES20/GL_FRAGMENT_SHADER frg-src))
             (GLES20/glLinkProgram))]
          (print-gl-errors-checked)
          (when (and *debug-gl-shader-log*
                     (= GLES20/GL_FALSE
                        (n-ints-from-buffer 1
                          (GLES20/glGetProgramiv program GLES20/GL_LINK_STATUS))))
            (log-i (GLES20/glGetProgramInfoLog program)))))

(defn make-effect
  "Wrapper for MediaEffect creation"
  [factory str-name]
  {:pre [(EffectFactory/isEffectSupported str-name)]}
  (log-i (str "Making effect " str-name))
  (? (.createEffect factory str-name)))

(defn make-preview-renderer
  [gl-surface-view rotation]
  (let [gl-state (ref nil)]
    (reify android.opengl.GLSurfaceView$Renderer
      (onDrawFrame 
        [_ gl10] 
        (gl-calls
          (GLES20/glClearColor 0.8 0.3 0.3 1.0)
          (GLES20/glClear GLES20/GL_COLOR_BUFFER_BIT))
        (.apply (@gl-state :saturate)
                (get-in @gl-state [:preview-tex :name])
                (get-in @gl-state [:preview-tex :width])
                (get-in @gl-state [:preview-tex :height])
                (get-in @gl-state [:display-tex :name]))
        (with-program (@gl-state :simple-prg)
          (draw-fs-tex-rect (@gl-state :display-tex))))
      (onSurfaceChanged
        [_ gl10 w h] )
      (onSurfaceCreated
        [renderer gl10 egl-cfg] 
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
                saturate (do-let 
                           [saturate (-> (EffectContext/createWithCurrentGlContext)
                                       .getFactory
                                       (make-effect EffectFactory/EFFECT_SEPIA))]
                           (.apply saturate
                                   (preview-tex :name)
                                   (preview-tex :width)
                                   (preview-tex :height)
                                   (display-tex :name)))
                simple-prg (load-program
                             "attribute vec2 aPosition;
                             attribute vec2 aTexcoord0;

                             varying vec2 vTexcoord0;

                             void main() {
                             gl_Position = vec4(aPosition, 0, 1);
                             // Check to pass texcoord from v -> f
                             //vTexcoord0 = vec2(1.0, 0.0);
                             vTexcoord0 = aTexcoord0;
                             }"
                             "precision mediump float;
                             varying vec2 vTexcoord0;
                             uniform sampler2D uSampler;

                             void main() {
                             gl_FragColor = texture2D(uSampler, vTexcoord0);
                             //gl_FragColor = syntax err
                             // Check to make sure texcoord is valid
                             // gl_FragColor = vec4(vTexcoord0,1,1);
                             }")]
            ; Enabling GL_TEXTURE_2D is not needed in ES 2.0.
            ; (gl-calls (GLES20/glEnable GLES20/GL_TEXTURE_2D))
            
            ; (.setParameter saturate "scale" (float -1))
 ;           (doto cam
 ;             (.setPreviewTexture preview-st)
 ;             ; (.setDisplayOrientation
 ;             ;   (front-facing-camera-correction cam-id rotation))
 ;             (.startPreview))
            (dosync (ref-set
                      gl-state {:preview-tex preview-tex
                                :display-tex display-tex
                                :saturate saturate
                                :simple-prg simple-prg})))))))

(defn -onCreate
  [^Activity this ^Bundle bundle] 
  (with-activity
    this
    (doto *activity*
      (.superOnCreate bundle)
      (.setContentView (get-layout :preview)))
    (let [preview-surface (find-view (get-id :preview_surface))]
      (doto preview-surface
        (.setEGLContextClientVersion 2)
        (.setPreserveEGLContextOnPause false) ; start with the more bug-prone mode
        (.setEGLConfigChooser false) ; no depth
        ; Annoyingly, this is broken for ES2 funcs
        ; (.setDebugFlags GLSurfaceView/DEBUG_CHECK_GL_ERROR)
        (.setRenderer (make-preview-renderer
                        preview-surface
                        (get-display-rotation *activity*)))
        (.setRenderMode GLSurfaceView/RENDERMODE_WHEN_DIRTY)))))

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
