(ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:dims   [100 100]
           :script [[:path
                     {}
                     [:M [65 15]]
                     [:C [5 32] [54 34] [73 55]]
                     [:S [10 56] [51 89]]]
                    [:path
                     {}
                     [:M [69 5]]
                     [:C [11 15] [67 28] [83 45]]
                     [:S [0 52] [32 83]]]]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
