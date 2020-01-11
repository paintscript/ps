 (ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]
            [paintscript.ctrl :as ctrl]))

(def init-clear
  {:defs {}
   :script []})

(def init-config
  {:canvas
   {:dims [100 100] :scale 4}

   :styles
   {"outline" {:stroke "black" :fill "none"}
    "solid"   {:stroke "none"  :fill "black"}}})

(defn- root []
  [canvas ctrl/params-init init-config])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
