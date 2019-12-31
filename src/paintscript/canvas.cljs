(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [paintscript.core :as ps]
            [svg-hiccup-kit.core :refer [tf tf* d d2]]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn get-path-segment [script-norm pth-vec-i]
  (let [pth-vec-prev    (get script-norm (dec pth-vec-i))
        [k :as pth-vec] (get script-norm pth-vec-i)]
    (list
     [:line (last pth-vec-prev)]
     pth-vec)))

(defn canvas [{:keys [dims script]}]
  (r/with-let [!script (r/atom script)
               !sel    (r/atom nil)
               sc 4
               [report! dnd-fns] (ps/drag-and-drop-fns [sc sc] !script)
               report!' (fn [ii] (reset! !sel ii) (report! ii))]
    (let [[w h] (->> dims (mapv #(* % sc)))
          script @!script
          [pth-vec-i pnt-i :as sel] @!sel

          [pnt-tups points] (->> script
                                 (ps/path {:debug? true}))
          !script'          (delay
                             (->> script
                                  (take (inc pth-vec-i))
                                  ps/normalize-curves))]
      [:div.canvas
       [:div.script
        [:textarea
         {:value     (str/trim (pprint' @!script))
          :on-change #(reset! !script (-> % .-target .-value read-string))}]
        [:div.status
         (when sel
           (let [[k & pnts] (nth script pth-vec-i)]
             [:div.selection
              [:span.pth-k (pr-str k)]
              (for [[i pnt] (map-indexed vector pnts)]
                ^{:key (hash [sel i pnt])}
                [:span {:class (when (= pnt-i (inc i)) "selected")}
                 (pr-str pnt)])]))]]

       [:div.paint
        [:svg (merge {:style {:width w :height h}}
                     dnd-fns)
         [tf* {:sc [sc sc]}
          [:g.main
           [ps/path-builder {} script]]
          (when sel
            [:g.sel
             [ps/path-builder {} (get-path-segment @!script' pth-vec-i)]])]

         [:g.coords
          (ps/plot-coords {:scaled     sc
                           :coord-size 10
                           :report!    report!'
                           :sel        sel}
                          script pnt-tups)]]]])))
