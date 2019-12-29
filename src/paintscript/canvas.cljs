(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
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
    (let [[w h] (->> dims (mapv #(* % sc)))
          script @!script]
      [:div.canvas
       [:div.script
        [:textarea
         {:value     (str/trim (pprint' @!script))
          :on-change #(reset! !script (-> % .-target .-value read-string))}]]
       [:div.paint
        [:svg (merge {:style {:width w :height h}}
                     dnd-fns)
         [tf {:sc [sc sc]}
          [ps/path-builder {:attrs {:stroke "black"
                                    :fill "hsl(0,0%,80%)"}}
                           script]]

         [ps/path-builder {:debug?     true
                           :scaled     sc
                           :coord-size 10
                           :report!    report!}
                          script]]]])))
