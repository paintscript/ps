(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [z-com.core :as zc]
            [svg-hiccup-kit.core :refer [tf tf* d d2]]
            [keybind.core :as key]

            [paintscript.ops :as ops]
            [paintscript.pth-vecs :as pth-vecs]
            [paintscript.core :as ps]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn get-path-segment [pth-vecs pth-vec-i]
  (let [pth-vec-prev    (get pth-vecs (dec pth-vec-i))
        [k :as pth-vec] (get pth-vecs pth-vec-i)]
    (concat
     (when-not (= :M (first pth-vec))
       (list [:M (last pth-vec-prev)]))
     (list pth-vec))))

(defn canvas [{:keys [dims script]}]
  (r/with-let [!script (r/atom script)
               !sel    (r/atom nil)
               !hov    (r/atom nil)
               sc 4
               report!     (fn [iii] (reset! !sel iii))
               dispatch!   (partial ps/dispatch! !script !sel)
               dnd-fns     (ps/drag-and-drop-fns [sc sc] !script !sel dispatch!)
               kb-fns      (ps/keybind-fns       [sc sc] !script !sel dispatch!)
               report-hov! (fn [iii val]
                             (swap! !hov #(cond
                                            val iii
                                            (= iii %) nil
                                            :else %)))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))]

    (let [[w h] (->> dims (mapv #(* % sc)))

          script @!script

          [pth-i-sel
           pth-vec-i-sel
           pnt-i-sel :as sel] @!sel

          hov @!hov

          out-tups (->> (:script script)
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
           (let [[_ opts :as path] (get-in script [:script pth-i-sel])
                 [k & pnts]        (get path pth-vec-i-sel)]
             [:div.selection-stack
              [:div.selection-level.path
               [:span (pr-str opts)]
               [:div.controls.crud
                [zc/button :label "add" :on-click #(dispatch! [:pth-append])]
                [zc/button :label "del" :on-click #(dispatch! [:pth-del])]]]

              [:div.selection-level.path-vec
               [:span.pth-k (pr-str k)]
               [:div.controls.crud
                [zc/button :label "add" :on-click #(dispatch! [:pth-vec-append])]
                [zc/button :label "del" :on-click #(dispatch! [:pth-vec-del])]]]

              [:div.selection-level.point
               [:span (pr-str (nth pnts (- pnt-i-sel pth-vecs/i-pnt0)))]
               (when (contains? #{:L :arc} k)
                 [:div.controls.crud
                  [zc/button :label "add" :on-click #(dispatch! [:pnt-append])]
                  [zc/button :label "del" :on-click #(dispatch! [:pnt-del])]])]]))]]

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
            (let [pth-vv' (->> (get-in script [:script pth-i-sel])
                               (take (inc pth-vec-i-sel))
                               (drop pth-vecs/i-pth-vec0)
                               pth-vecs/normalize-path-vecs)]
              [:g.sel
               [ps/path-builder {} pth-i-sel
                (get-path-segment pth-vv' (- pth-vec-i-sel pth-vecs/i-pth-vec0))]]))]

         [:g.coords
          (for [[pth-i opts pth-vv _] out-tups]
            (let [pth-vv'   (pth-vecs/attach-normalized-meta
                             pth-vv
                             (pth-vecs/normalize-path-vecs pth-vv))
                  [pnt-tups
                   pnts]    (ps/path (merge opts {:debug? true :coords? true}) pth-vv')]
              ^{:key pth-i}
              [:g
               (ps/plot-coords {:scaled        sc
                                :coord-size    10
                                :report!       report!
                                :report-hover! report-hov!
                                :sel           sel
                                :hov           hov
                                :controls?     (= pth-i-sel pth-i)}
                               pth-i pth-vv' pnt-tups)]))]]]])))
