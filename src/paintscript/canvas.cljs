(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [paintscript.core :as ps]
            [svg-hiccup-kit.core :refer [tf tf* d d2]]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn get-path-segment [pth-vecs pth-vec-i]
  (let [pth-vec-prev    (get pth-vecs (dec pth-vec-i))
        [k :as pth-vec] (get pth-vecs pth-vec-i)]
    (list
     [:M (last pth-vec-prev)]
     pth-vec)))

(defn canvas [{:keys [dims script]}]
  (r/with-let [!script (r/atom script)
               !sel    (r/atom nil)
               sc 4
               [report! dnd-fns] (ps/drag-and-drop-fns [sc sc] !script)
               report!' (fn [iii] (reset! !sel iii) (report! iii))]
    (let [[w h] (->> dims (mapv #(* % sc)))
          script @!script
          [pth-i pth-vec-i pnt-i :as sel] @!sel

          out-tups (->> script
                        (map-indexed
                         (fn [pth-i [_ opts & pth-vv :as path]]
                           [pth-i opts pth-vv
                            (ps/path (merge opts {:debug? true}) pth-vv)])))]
      [:div.canvas
       [:div.script
        [:textarea
         {:value     (str/trim (pprint' script))
          :on-change #(reset! !script (-> % .-target .-value read-string))}]
        [:div.status
         (when sel
           (let [[k & pnts] (get-in script [pth-i pth-vec-i])]
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
           (for [[pth-i opts pth-vv
                  [pnt-tups pnts] :as out-tup] out-tups]
             ^{:key pth-i}
             [ps/path-builder opts pth-i pth-vv])]

          (when (and sel (> pth-vec-i ps/i-pth-vec0))
            (let [pth-vv' (->> (get script pth-i)
                               (take (inc pth-vec-i))
                               (drop ps/i-pth-vec0)
                               ps/normalize-path)]
              [:g.sel
               [ps/path-builder {} pth-i
                (get-path-segment pth-vv' (- pth-vec-i ps/i-pth-vec0))]]))]

         [:g.coords
          (for [[pth-i opts pth-vv
                 [pnt-tups pnts] :as out-tup] out-tups]
            ^{:key pth-i}
            [:g
             (ps/plot-coords {:scaled     sc
                              :coord-size 10
                              :report!    report!'
                              :sel        sel}
                             pth-i pth-vv pnt-tups)])]]]])))
