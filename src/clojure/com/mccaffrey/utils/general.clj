(ns com.mccaffrey.utils.general
  (:gen-class :main false
              :extends android.app.Activity
              :exposes-methods {onCreate superOnCreate})
  (:import [java.nio ByteBuffer IntBuffer ByteOrder]
           [javax.microedition.khronos.egl EGLContext EGL10 EGL])
  (:use [neko log]))

(deflog "utils/general")

(defmacro do-let
  [[binding-form init-expr] & body]
  `(let [~binding-form ~init-expr]
     ~@body
     ~binding-form))

(defn pr-val
  [sym val]
  (log-i (str sym " is " val)))

(defmacro ?
  [val]
  `(do-let [x# ~val]
           (pr-val x# val)))

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
