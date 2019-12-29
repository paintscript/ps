(ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:dims [100 100]
           :script [[:line [60 14]]
                    [:curve-C [5 30] [64 31] [60 48]]
                    [:curve-C [49 65] [22 38] [19 70]]]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
