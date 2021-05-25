(ns paintscript.xspace-view
  (:require [reagent.core :as r]
            [svg-hiccup-kit.core :refer [tf d]]
            [paintscript.core :as core]))

(defn xspace-paintscript [ctx c args]
  (r/with-let [{:as args* :keys [op view-opts opts path pnts center factor =>]}
               (merge (:args ctx) args)

               opts' (merge view-opts
                            opts)]
    [:div.xspace-paintscript
     (when =>
       (condp #(contains? %1 %2) op
         #{:path}
         [:svg {:width 100 :height 100}
          [core/path-builder opts' path]
          ; [:path {:stroke "red" :fill "none"
          ;         :d (apply d =>)}]
          ]

         #{:scale-p-els}
         [:div
          [:svg {:width 100 :height 100}
           [core/path-builder opts' path]]

          [:svg {:width 100 :height 100}
           [core/path-builder opts' (core/scale-p-els path center factor)]]]
         nil))]))
