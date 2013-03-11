(ns com.mccaffrey.utils.gl

  (:import [android.opengl GLES20 GLES11Ext]
           [java.nio ByteBuffer IntBuffer ByteOrder Buffer])
  (:use [com.mccaffrey.utils general]
        [neko log]))

(set! *warn-on-reflection* true)

(deflog "utils/gl")

(def 
  ^{:doc "The current program to operate on"
    :dynamic true}
  *program*)

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

(def 
  ^{:doc "Print all gl-calls with args"
    :dynamic true}
  *debug-verbose-gl-print-calls* false)

(defn gl-error-to-str
  [errno]
  (case errno
    GLES20/GL_NO_ERROR "GL_NO_ERROR"
    GLES20/GL_INVALID_ENUM "GL_INVALID_ENUM"
    GLES20/GL_INVALID_VALUE "GL_INVALID_VALUE"
    GLES20/GL_INVALID_OPERATION "GL_INVALID_OPERATION"
    GLES20/GL_INVALID_FRAMEBUFFER_OPERATION "GL_INVALID_FRAMEBUFFER_OPERATION"
    GLES20/GL_OUT_OF_MEMORY "GL_OUT_OF_MEMORY"
    (format "0x%02x" errno)))

; functions to deal with GL errors and debugging
(defn gl-get-actual-error
  "Get a non-trivial GL error."
  []
  (let [errno (GLES20/glGetError)]
    (if (not= errno GLES20/GL_NO_ERROR)
      errno nil)))

(defn print-gl-errors 
  "Print all current GL errors."
  [& op]
  (while
    (when-let [errno (gl-get-actual-error)]
      (let [err-str (gl-error-to-str errno)
            full-str (if op
                       (str "GL errors after " op " " err-str)
                       (str "GL error: " err-str))]
        (if *debug-exception-on-gl-errors* (throw (Throwable. full-str)))
        (log-i full-str)))))

(defn print-gl-errors-checked
  "Printf all current gl errors if 
  *debug-gl-check-errors* is true"
  [& op]
  (if *debug-gl-check-errors*
    (if op (print-gl-errors op)
      (print-gl-errors))))

; log-i doesn't behavre very well with qualified macros, so use a separate
; print-fn
(defn macro-pr
  [txt]
  (log-i txt))

; TODO left off because flatten was flattening too much
(defn gl-calls-fn
  [calls]
  (mapcat (fn [call]
            `[(if *debug-verbose-gl-print-calls* (macro-pr ~(str call)))
              (do-let [res# ~call]
                      (print-gl-errors-checked '~call))])
          calls))

; How to remove extra parens...
(defmacro gl-calls
  "Wrap a sequence of GL call expression,
  optionally checking GL errors after each.
  Returns the value of the last expr."
  [& more]
  `(do
    ~@(gl-calls-fn more)))

(defn gen-framebuffer
  "Return a generated FBO"
  []
  (let [fbos (int-array 1)]
    (gl-calls (GLES20/glGenFramebuffers 1 fbos 0))
    (nth fbos 0)))

(defn delete-framebuffer
  "Delete a generated fbo"
  [fbo]
  (gl-calls (GLES20/glDeleteFramebuffers 1 (int-array [fbo]) 0)))

(defn check-framebuffer-status
  []
  (if (not (= (GLES20/glCheckFramebufferStatus GLES20/GL_FRAMEBUFFER)))
    (throw (Throwable. "Framebuffer not complete!"))))

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

; Functions to deal with creating and rendering OpenGL objects
(defn setup-attrib
  [kw]
  (gl-calls (GLES20/glEnableVertexAttribArray (:location kw))
            (GLES20/glVertexAttribPointer ^Integer (:location kw)
                                          ^Integer (:size kw)
                                          ^Integer (:type kw)
                                          ^Boolean (:norm kw)
                                          ^Integer (:stride kw)
                                          ^Buffer (:buffer kw))))

(defn gen-texture
  "Return a texture name created by glGenTextures"
  []
  {:name 
   (let [textures (int-array 1)]
     (gl-calls (GLES20/glGenTextures 1 textures 0))
     (nth textures 0))})

(defn delete-texture
  "Return a texture name created by glGenTextures"
  [tex]
  (GLES20/glDeleteTextures 1 (int-array [(tex :name)]) 0))

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

(defmacro with-framebuffer
  [fb & body]
  `(do
     (gl-calls (GLES20/glBindFramebuffer GLES20/GL_FRAMEBUFFER ~fb))
     ~@body
     (gl-calls (GLES20/glBindFramebuffer GLES20/GL_FRAMEBUFFER 0))))

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

; TODO is unbinding necessary?
(defmacro with-texture
  [target tex unit uniform-name & body]
  `(let [target# ~target]
     (setup-texture target# ~tex ~unit ~uniform-name)
     ~@body
    (GLES20/glBindTexture target# 0)))



(defn setup-texture
  "Setup a texture for rendering with a program."
  [target tex unit uniform-name]
  (gl-calls 
    (GLES20/glActiveTexture (+ GLES20/GL_TEXTURE0 unit))
    (GLES20/glBindTexture target (tex :name))
    (GLES20/glUniform1i
      (GLES20/glGetUniformLocation *program* uniform-name)
      unit)))

(defn draw-fs-tex-rect
  "Draw a given texture on full-screen quad."
  [target tex]
  (with-texture
    target tex 0 "uSampler"
    (setup-attrib (assoc-location fs-pos-attrib "aPosition"))
    (setup-attrib (assoc-location fs-tex-attrib "aTexcoord0"))
    (gl-calls (GLES20/glDrawArrays GLES20/GL_TRIANGLE_FAN 0 4))))

(defn fbo-tex
  [target fmt size-thingie width height]
  (let [fbo (gen-framebuffer)]
    [fbo
     (assoc
       (do-let [tex (make-texture GLES20/GL_TEXTURE_2D)]
               (gl-calls (GLES20/glTexImage2D
                           target
                           0 ; stupid border arg
                           fmt
                           width
                           height
                           0 ; stupid border arg
                           fmt
                           size-thingie
                           nil))
               (with-framebuffer
                 fbo
                 (gl-calls
                   (GLES20/glFramebufferTexture2D
                     GLES20/GL_FRAMEBUFFER
                     GLES20/GL_COLOR_ATTACHMENT0
                     GLES20/GL_TEXTURE_2D
                     (tex :name)
                     0))
                 (check-framebuffer-status)
                 ))
       :width width
       :height height)]))

; TODO take in re-useable texture ids?
(defn copy-sampler-external-to-tex
  [external-tex external-blit-prg fbo]
  ; TODO macro to gen/bind and unbind/destroy temp FBO?
  (with-framebuffer
    fbo
    ; TODO don't spam recompile programs
    (with-program 
      external-blit-prg
      (draw-fs-tex-rect
        GLES11Ext/GL_TEXTURE_EXTERNAL_OES
        external-tex))))
