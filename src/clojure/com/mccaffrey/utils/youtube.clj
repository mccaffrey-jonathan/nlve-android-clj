(ns com.mccaffrey.utils.youtube
  (:import 
    [android.net Uri UrlQuerySanitizer]
    [java.net URLDecoder]
    [com.loopj.android.http AsyncHttpClient RequestParams AsyncHttpResponseHandler])
  (:use [com.mccaffrey.utils general]
        [neko log]))

(deflog "utils/youtube")

; You-Tube playback cribbed from (Apache licensed)
; http://code.google.com/p/android-youtube-player/

; stupid hack for lazy initialization
(def youtube-http (memoize #(AsyncHttpClient.)))

(def example-id "6BTCoT8ajbI")
(def info-url-base "http://www.youtube.com/get_video_info?&video_id=")

(defn on-http-call
  [success-cb & error-cb]
  (proxy [AsyncHttpResponseHandler] []
    (onSuccess [code res & rest]
      (log-i "about to call success-cb")
      (success-cb res))
    (onFailure [e res]
      (throw e))))

(defmacro on-http
  "Run bo(fmt-stream-map-to-url (first (map query-param-map streams)))dy asynchronously on success with res bound to the http response"
  [& body]
  `(on-http-call
    ; unquote-quote to escape symbol qualification, I think
    (fn [~'res] ~@body)))

(defn query-param-map
  "Parse a query-formatted string into a map."
  [query-str]
  (let [url (doto (UrlQuerySanitizer.)
              (.setAllowUnregisteredParamaters true)
              (.setUnregisteredParameterValueSanitizer (UrlQuerySanitizer/getUrlLegal))
              (.parseQuery query-str))]
    ;(.parseQuery url (.unescape url query-str))
    (reduce #(assoc %1 %2 (.getValue url %2))
            {}
            (.getParameterSet url))))

; Taken from:
; https://github.com/rb2k/viddl-rb/issues/52
(def valido-params
  ["algorithm" "burst" "cp" "cpn" "expire" "factor" "fexp" "id"
   "ip" "ipbits" "itag" "keepalive" "key" "ms" "mt" "mv" "newshard" "range"
   "ratebypass" "signature" "source" "sparams" "sver" "upn"])

; Some of the params change names
(def param-version-map
  {"signature", "sig"})

(defn fmt-stream-map-to-url
  [fmt-stream-map-to-url]
  (str (fmt-stream-map-to-url "url")
       (apply str
              (filter (complement nil?)
                      (for [param valido-params]
                        (let [param-version (param-version-map param)]
                          (if-let [value (fmt-stream-map-to-url (or param-version param))]
                            (str "&" param "=" value)
                            nil)))))))

; TODO, should we return some metadata (size, fmt, qual) as well?
(defn youtube-media-uri
  [video-id fmt-quality fall-back success-cb & error-cb]
  {:pre [(fn? success-cb)
         (string? fmt-quality)
         (string? video-id)]}
  (letfn [(info-cb [res]
            ; TODO 'invert' this function
            (let [res-map (query-param-map res)
                  fmts (.split (res-map "fmt_list") ",")]
              (success-cb
                (do-let [uri (Uri/parse
                               (fmt-stream-map-to-url
                                    ; just first for now
                                    (first 
                                      (pr-seq-with-newlines
                                        (map query-param-map
                                             (map #(URLDecoder/decode % "utf-8")
                                                  (.split (res-map "url_encoded_fmt_stream_map")
                                                          ",")))))))]
                        (log-i (.toString uri))
                        ))))]
    ; http call, so we should do it async
    (.get (youtube-http)
      (str info-url-base video-id)
      (RequestParams.) ; TODO can we re-use the default params?
      (on-http-call info-cb error-cb))))

; */
; public static String calculateYouTubeUrl(String pYouTubeFmtQuality, boolean pFallback,
;                 String pYouTubeVideoId) throws IOException,
;                 ClientProtocolException, UnsupportedEncodingException {
; 
;         String lUriStr = null;
;         HttpClient lClient = new DefaultHttpClient();
;         
;         HttpGet lGetMethod = new HttpGet(OpenYouTubePlayerActivity.YOUTUBE_VIDEO_INFORMATION_URL + 
;                                                                          pYouTubeVideoId);
;         
;         HttpResponse lResp = null;
; 
;         lResp = lClient.execute(lGetMethod);
;                 
;         ByteArrayOutputStream lBOS = new ByteArrayOutputStream();
;         String lInfoStr = null;
;                 
;         lResp.getEntity().writeTo(lBOS);
;         lInfoStr = new String(lBOS.toString("UTF-8"));
;         
;         String[] lArgs=lInfoStr.split("&");
;         Map<String,String> lArgMap = new HashMap<String, String>();
;         for(int i=0; i<lArgs.length; i++){
;                 String[] lArgValStrArr = lArgs[i].split("=");
;                 if(lArgValStrArr != null){
;                         if(lArgValStrArr.length >= 2){
;                                 lArgMap.put(lArgValStrArr[0], URLDecoder.decode(lArgValStrArr[1]));
;                         }
;                 }
;         }
;         
;         //Find out the URI string from the parameters
;         
;         //Populate the list of formats for the video
;         String lFmtList = URLDecoder.decode(lArgMap.get("fmt_list"));
;         ArrayList<Format> lFormats = new ArrayList<Format>();
;         if(null != lFmtList){
;                 String lFormatStrs[] = lFmtList.split(",");
;                 
;                 for(String lFormatStr : lFormatStrs){
;                         Format lFormat = new Format(lFormatStr);
;                         lFormats.add(lFormat);
;                 }
;         }
;         
;         //Populate the list of streams for the video
;         String lStreamList = lArgMap.get("url_encoded_fmt_stream_map");
;         if(null != lStreamList){
;                 String lStreamStrs[] = lStreamList.split(",");
;                 ArrayList<VideoStream> lStreams = new ArrayList<VideoStream>();
;                 for(String lStreamStr : lStreamStrs){
;                         VideoStream lStream = new VideoStream(lStreamStr);
;                         lStreams.add(lStream);
;                 }       
;                 
;                 //Search for the given format in the list of video formats
;                 // if it is there, select the corresponding stream
;                 // otherwise if fallback is requested, check for next lower format
;                 int lFormatId = Integer.parseInt(pYouTubeFmtQuality);
;                 
;                 Format lSearchFormat = new Format(lFormatId);
;                 while(!lFormats.contains(lSearchFormat) && pFallback ){
;                         int lOldId = lSearchFormat.getId();
;                         int lNewId = getSupportedFallbackId(lOldId);
;                         
;                         if(lOldId == lNewId){
;                                 break;
;                         }
;                         lSearchFormat = new Format(lNewId);
;                 }
;                 
;                 int lIndex = lFormats.indexOf(lSearchFormat);
;                 if(lIndex >= 0){
;                         VideoStream lSearchStream = lStreams.get(lIndex);
;                         lUriStr = lSearchStream.getUrl();
;                 }
;                 
;         }               
;         //Return the URI string. It may be null if the format (or a fallback format if enabled)
;         // is not found in the list of formats for the video
;         return lUriStr;
; }

