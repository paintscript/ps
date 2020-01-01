 (ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:dims   [100 100]
           :script [[:path
                     {:mirror :separate}
                     [:M [76 11]]
                     [:C [8 26] [79 65] [79 49]]
                     [:S [10 69] [74 89]]]]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
