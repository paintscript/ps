(ns paintscript.canvas
  (:require [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [paintscript.core :as ps]
            [svg-hiccup-kit.core :refer [tf d d2]]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn canvas [{:keys [dims]}]
  (r/with-let [!script (r/atom
                        [;; bot-c
                         [:line [50 54.5]]
                         [:curve-C [41 54.5] [35 58] [35 58]]
                         [:curve-C [35 58] [25 55] [25 42]]
                         [:curve-C [27 29] [40 25] [50 15]]
                         [:line* [50 15]]])
               !sc     (r/atom 4)]
    (let [sc @!sc
          [w h] (->> dims (mapv #(* % sc)))]
      [:div.canvas
       [:div.script
        [:textarea
         {:value     (pprint' @!script)
          :on-change #(reset! !script (read-string %))}]]
       [:div.paint
        [:svg {:style {:width w :height h}}
         [tf {:sc [sc sc]}
          [ps/path-builder {:debug? true :coord-size 2 :scaled [sc sc]}
                           @!script]]]]])))
