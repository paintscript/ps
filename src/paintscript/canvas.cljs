(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [z-com.core :as zc]
            [svg-hiccup-kit.core :refer [tf tf* d d2]]

            [paintscript.ops :as ops]
            [paintscript.pth-vecs :as pth-vecs]
            [paintscript.core :as ps]))

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
          [pth-i-sel pth-vec-i-sel pnt-i-sel :as sel] @!sel

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
           (let [[_ opts :as path] (get script pth-i-sel)
                 [k & pnts]        (get path pth-vec-i-sel)]
             [:div.selection-stack
              [:div.selection-level.path
               [:span (pr-str opts)]
               [:div.controls.crud
                [zc/button
                 :label "add"
                 :on-click #(do
                              (reset! !sel nil)
                              (swap! !script ops/append-pth pth-i-sel))]
                [zc/button
                 :label "del"
                 :on-click #(do
                              (reset! !sel nil)
                              (swap! !script ops/del-pth pth-i-sel))]]]

              [:div.selection-level.path-vec
               [:span.pth-k (pr-str k)]
               [:div.controls.crud
                [zc/button
                 :label "add"
                 :on-click #(do
                              (reset! !sel nil)
                              (swap! !script update-in [pth-i-sel]
                                     ops/append-pth-vec pth-vec-i-sel))]
                [zc/button
                 :label "del"
                 :on-click #(do
                              (reset! !sel nil)
                              (swap! !script update-in [pth-i-sel]
                                     ops/del-pth-vec pth-vec-i-sel))]]]

              [:div.selection-level.point
               [:span (pr-str (nth pnts (- pnt-i-sel pth-vecs/i-pnt0)))]
               (when (contains? #{:L :arc} k)
                 [:div.controls.crud
                  [zc/button
                   :label "add"
                   :on-click #(do
                                (reset! !sel nil)
                                (swap! !script update-in [pth-i-sel]
                                       ops/append-pnt pth-vec-i-sel))]
                  [zc/button
                   :label "del"
                   :on-click #(do
                                (reset! !sel nil)
                                (swap! !script update-in [pth-i-sel]
                                       ops/del-pnt pth-vec-i-sel
                                       (- pnt-i-sel pth-vecs/i-pnt0)))]])]]))]]

       [:div.paint
        [:svg (merge {:style {:width w :height h}}
                     dnd-fns)
         [tf* {:sc [sc sc]}

          [:g.main
           (for [[pth-i opts pth-vv
                  [pnt-tups pnts] :as out-tup] out-tups]
             ^{:key pth-i}
             [ps/path-builder opts pth-i pth-vv])]

          (when (and sel (> pth-vec-i-sel pth-vecs/i-pth-vec0))
            (let [pth-vv' (->> (get script pth-i-sel)
                               (take (inc pth-vec-i-sel))
                               (drop pth-vecs/i-pth-vec0)
                               pth-vecs/normalize-path-vecs)]
              [:g.sel
               [ps/path-builder {} pth-i-sel
                (get-path-segment pth-vv' (- pth-vec-i-sel pth-vecs/i-pth-vec0))]]))]

         (let []
           [:g.coords
            (for [[pth-i opts pth-vv _] out-tups]
              (let [pth-vv' (pth-vecs/attach-normalized-meta pth-vv
                                                       (pth-vecs/normalize-path-vecs pth-vv))
                    [pnt-tups pnts] (ps/path (merge opts {:debug? true}) pth-vv')]
                ^{:key pth-i}
                [:g
                 (ps/plot-coords {:scaled     sc
                                  :coord-size 10
                                  :report!    report!'
                                  :sel        sel
                                  :controls?  (= pth-i-sel pth-i)}
                                 pth-i pth-vv' pnt-tups)]))])]]])))
