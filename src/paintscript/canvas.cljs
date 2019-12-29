(ns paintscript.canvas
  (:require [clojure.string :as str]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [paintscript.core :as ps]
            [svg-hiccup-kit.core :refer [tf d d2]]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn canvas [{:keys [dims script]}]
  (r/with-let [!script (r/atom script)
               sc 4
               [report! dnd-fns] (ps/drag-and-drop-fns [sc sc] !script)]
    (let [[w h] (->> dims (mapv #(* % sc)))]
      [:div.canvas
       [:div.script
        [:textarea
         {:value     (str/trim (pprint' @!script))
          :on-change #(reset! !script (-> % .-target .-value read-string))}]]
       [:div.paint
        [:svg (merge {:style {:width w :height h}}
                     dnd-fns)
         [tf {:sc [sc sc]}
          [ps/path-builder {:debug?     true
                            :coord-size 2
                            :scaled     [sc sc]
                            :atom?      true
                            :attrs      {:stroke "black"
                                         :fill "hsl(0,0%,80%)"}
                            :report!    report!}
                           @!script]]]]])))
