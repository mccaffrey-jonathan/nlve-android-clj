(ns com.mccaffrey.utils.media
  (:import 
    [android.media MediaPlayer])
  (:use [com.mccaffrey.utils general]
        [neko log]))

(deflog "utils/media")

; TODO prints are still falling through, clean that up
(defmacro case-print
  "macro to generate named-val-to-string case blocks, defaulting to printing"
  [e & vals]
  `(let [e# ~e]
     (case e#
       ~@(interleave vals (map str vals))
       (str e#))))

(defn fmt-mp-error
  [what extra]
  (format "MediaPlayerError what=%s, extra=%s"
          (case what
            MediaPlayer/MEDIA_ERROR_UNKNOWN "MEDIA_ERROR_UNKNOWN"
            MediaPlayer/MEDIA_ERROR_SERVER_DIED "MEDIA_ERROR_SERVER_DIED"
            (format "%x" what))
          (case extra
            MediaPlayer/MEDIA_ERROR_IO "MEDIA_ERROR_IO"
            MediaPlayer/MEDIA_ERROR_MALFORMED "MEDIA_ERROR_MALFORMED"
            MediaPlayer/MEDIA_ERROR_UNSUPPORTED "MEDIA_ERROR_UNSUPPORTED"
            MediaPlayer/MEDIA_ERROR_TIMED_OUT "MEDIA_ERROR_TIMED_OUT"
            (format "%x" extra))))

;(defn fmt-mp-info
;  [what extra]
;  (format "MediaPlayerInfo what=%d, extra=%d" what extra))

(defn fmt-mp-info
  [what extra]
  (format "MediaPlayerInfo what=%s, extra=%d"
          (case what
            MediaPlayer/MEDIA_INFO_UNKNOWN "MEDIA_INFO_UNKNOWN"
            MediaPlayer/MEDIA_INFO_VIDEO_TRACK_LAGGING "MEDIA_INFO_VIDEO_TRACK_LAGGING"
            MediaPlayer/MEDIA_INFO_VIDEO_RENDERING_START "MEDIA_INFO_VIDEO_RENDERING_START"
            MediaPlayer/MEDIA_INFO_BUFFERING_START "MEDIA_INFO_BUFFERING_START"

            MediaPlayer/MEDIA_INFO_BUFFERING_END "MEDIA_INFO_BUFFERING_END"
            MediaPlayer/MEDIA_INFO_BAD_INTERLEAVING "MEDIA_INFO_BAD_INTERLEAVING"
            MediaPlayer/MEDIA_INFO_NOT_SEEKABLE "MEDIA_INFO_NOT_SEEKABLE"

            MediaPlayer/MEDIA_INFO_METADATA_UPDATE "MEDIA_INFO_METADATA_UPDATE"
            (format "%x" what) )
          extra))

(defn throw-mp-error
  [this mp what extra]
  (throw (Throwable. (fmt-mp-error what extra))))

(defn log-mp-info
  [this mp what extra]
  (log-i (fmt-mp-info what extra))
  true)

(defn log-mp-completion
  [this mp]
  (log-i "MediaPlayer completion"))

(defn generic-log-fn
  [msg]
  (fn [& rest]
    (log-i msg)))

(make-unary-ons media-player-error
                android.media.MediaPlayer$OnErrorListener
                onError
                [this mp what extra])

(make-unary-ons media-player-info
                android.media.MediaPlayer$OnInfoListener
                onInfo
                [this mp what extra])

(make-unary-ons media-player-completion
                android.media.MediaPlayer$OnCompletionListener
                onCompletion
                [this mp])

(make-unary-ons media-player-prepared 
                android.media.MediaPlayer$OnPreparedListener
                onPrepared
                [this mp])

(make-unary-ons media-player-buffering-update
                android.media.MediaPlayer$OnBufferingUpdateListener
                onBufferingUpdate
                [this mp percent])

(make-unary-ons media-player-seek-complete
                android.media.MediaPlayer$OnSeekCompleteListener
                onSeekComplete
                [this mp])

(make-unary-ons media-player-timed-text
                android.media.MediaPlayer$OnTimedTextListener
                onTimedText
                [this mp tt])

(make-unary-ons media-player-video-size-changed
                android.media.MediaPlayer$OnVideoSizeChangedListener
                onVideoSizeChanged
                [this mp w h])

; (defmacro make-on-body
;   [call-version-sym]
;   (let [macro-version 
;         (if-let [[whole fst & rest]
;                  (re-find #"(\S+)-call"
;                           (str call-version-sym))]
;           fst
;           (throw "bad func name!"))]
;     `(defmacro ~macro-version
;        (let [body-sym (gensym)]
;        `(~call-version-sym
;           (fn [ ] ~body-sym))))))
; 
; (defmacro on-media-player-error
;   [& body]
;   `(
; 
; 
;   )
;
