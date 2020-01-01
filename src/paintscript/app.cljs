(ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:dims   [100 100]
           :script [[:path
                     {:mirror :merged}
                     [:M [83 6]]
                     [:C [32 15] [79 34] [73 47]]
                     [:S [18 72] [50 92]]]
                    [:path
                     {:mirror :separate}
                     [:M [11 16]]
                     [:C [34 21] [55 71] [64 45]]
                     [:S [7 68] [36 92]]]
                    [:path {} [:M [10 30]] [:L [10 50]]]]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
