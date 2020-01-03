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
            [paintscript.els :as els]
            [paintscript.core :as ps]
            [paintscript.nav :as nav]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn get-path-segment [els eli]
  (let [el-prev    (nav/els-prev els :eli eli)
        [k :as el] (nav/els>     els :eli eli)]
    (concat
     (when (and el-prev
                (not= :M (first el)))
       (list [:M (last el-prev)]))
     (list el))))

(defn canvas [{:keys [dims script]}]
  (r/with-let [!script     (r/atom script)
               !sel        (r/atom nil)
               !hov        (r/atom nil)
               sc          4
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

          [pi-sel
           eli-sel
           xyi-sel :as sel] @!sel

          hov @!hov

          out-tups (->> (:script script)
                        (map-indexed
                         (fn [pi [_ opts & els :as path]]
                           [pi opts els
                            (ps/path (merge opts {:debug? true}) els)])))]
      [:div.canvas
       [:div.script
        [:textarea
         {:value     (str/trim (pprint' script))
          :on-change #(reset! !script (-> % .-target .-value read-string))}]

        [:div.status
         (when sel
           (let [[_ opts :as p] (nav/script> script :pi  pi-sel)
                 [k & xys]      (nav/p>      p      :eli eli-sel)]
             [:div.selection-stack
              [:div.selection-level.path
               [:span (pr-str opts)]
               [:div.controls.crud
                [zc/button :label "add" :on-click #(dispatch! [:pth-append])]
                [zc/button :label "del" :on-click #(dispatch! [:pth-del])]]]

              [:div.selection-level.path-vec
               [:span.pth-k (pr-str k)]
               [:div.controls.crud
                [zc/button :label "add" :on-click #(dispatch! [:el-append])]
                [zc/button :label "del" :on-click #(dispatch! [:el-del])]]]

              [:div.selection-level.point
               [:span (pr-str (nav/xys> xys :xyi xyi-sel))]
               (when (contains? #{:L :arc} k)
                 [:div.controls.crud
                  [zc/button :label "add" :on-click #(dispatch! [:xy-append])]
                  [zc/button :label "del" :on-click #(dispatch! [:xy-del])]])]]))]]

       [:div.paint
        [:svg (merge {:style {:width w :height h}}
                     dnd-fns)
         [tf* {:sc [sc sc]}

          [:g.main
           (for [[pi opts els
                  [pnt-tups xys] :as out-tup] out-tups]
             ^{:key pi}
             [ps/path-builder opts pi els])]

          (when (and sel (> eli-sel nav/eli0))
            (let [els' (->> (nav/script> script :pi pi-sel)
                               (take (inc eli-sel))
                               (drop nav/eli0)
                               els/normalize-els)]
              [:g.sel
               [ps/path-builder {} pi-sel
                (get-path-segment els' eli-sel)]]))]

         [:g.coords
          (for [[pi opts els _] out-tups]
            (let [els' (els/attach-normalized-meta
                        els
                        (-> els els/normalize-els))
                  [pnt-tups
                   xys]     (ps/path (merge opts {:debug? true :coords? true}) els')]
              ^{:key pi}
              [:g
               (ps/plot-coords {:scaled        sc
                                :coord-size    10
                                :report!       report!
                                :report-hover! report-hov!
                                :sel           sel
                                :hov           hov
                                :controls?     (= pi-sel pi)}
                               pi els' pnt-tups)]))]]]])))
