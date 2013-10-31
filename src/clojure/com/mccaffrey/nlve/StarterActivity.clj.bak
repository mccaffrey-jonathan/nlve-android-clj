(ns com.mccaffrey.nlve.StarterActivity
  (:use [neko
         find-view
         context
         activity])
  (:use neko.listeners.view)
  ;(:import com.mccaffrey.nlve.ImageEffectActivity)
  (:import [android.content
            Intent])
  (:import [com.mccaffrey.nlve
            ImageEffectActivity
            R$layout])
  (:gen-class :main false
              :extends android.app.Activity
              :exposes-methods {onCreate superOnCreate})
  )

(defn -onCreate
  [this bundle]
  (doto this
    (.superOnCreate bundle)
    (.setContentView R$layout/main))
  (.setOnClickListener
    (find-view this :button-0)
    (on-click
      (.startActivity
        this
        (Intent.
          this
          ImageEffectActivity)
        )
      )
    )
  )


