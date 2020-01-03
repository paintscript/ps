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

(defn get-path-segment [src-k-sel els eli]
  (let [el-prev    (nav/els-prev els (case src-k-sel :defs :eln :eli) eli)
        [k :as el] (nav/els>     els (case src-k-sel :defs :eln :eli) eli)]
    (concat
     (when (and el-prev
                (not= :M (first el)))
       (list [:M (last el-prev)]))
     (list el))))

(defn canvas [params-init]
  (r/with-let [!config     (r/atom {:dims [100 100]
                                    :styles
                                    {:outline {:stroke "black" :fill "none"}
                                     :solid   {:stroke "none"  :fill "black"}}})
               !params     (r/atom params-init)
               !sel        (r/atom nil)
               !hov        (r/atom nil)
               !tab        (r/atom :script)
               sc          4
               report!     (fn [iii] (reset! !sel iii))
               dispatch!   (partial ps/dispatch! !params !sel)
               dnd-fns     (ps/drag-and-drop-fns [sc sc] !params !sel dispatch!)
               kb-fns      (ps/keybind-fns       [sc sc] !params !sel dispatch!)
               report-hov! (fn [iii val]
                             (swap! !hov #(cond
                                            val iii
                                            (= iii %) nil
                                            :else %)))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))]

    (let [{:as params
           :keys [dims]
           :or {dims [100 100]}} (-> @!params
                                     (update :script els/attach-iii-meta*))

          config @!config

          [w h] (->> dims (mapv #(* % sc)))

          tab @!tab

          [src-k-sel
           pi-sel
           eli-sel
           xyi-sel :as sel] @!sel

          hov @!hov

          out-tups (->> (:script params)
                        (map-indexed
                         (fn [pi [_ p-opts & els :as path]]
                           (let [p-opts' (-> p-opts
                                             (assoc :defs (:defs params)))]
                             [pi p-opts' els
                              (ps/path (merge p-opts' {:debug? true})
                                       els)]))))]
      [:div.canvas
       [:div.script {:class (when sel "with-status")}
        [:div.controls
         (for [tab-k [:script :config]]
           ^{:key tab-k}
           [zc/button
            :label (name tab-k)
            :active? (= tab-k tab)
            :on-click #(reset! !tab tab-k)])]

        (case tab
          :script [:textarea
                   {:value     (str/trim (pprint' params))
                    :on-change #(reset! !params (-> % .-target .-value read-string))}]
          :config [:textarea
                   {:value     (str/trim (pprint' config))
                    :on-change #(reset! !config (-> % .-target .-value read-string))}])

        [:div.status
         (when sel
           (let [[_ opts :as p] (nav/params> params :src-k src-k-sel :pi  pi-sel)
                 [k & xys]      (nav/p>      p      :eli   eli-sel)]
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
           [ps/paint (merge config params)]]

          (when (and sel (or (= :defs src-k-sel)
                             (> eli-sel nav/eli0)))
            (let [els' (->> (nav/params> params :src-k src-k-sel :pi pi-sel)
                            (take (inc eli-sel))
                            (drop (if (= :defs src-k-sel) 0 nav/eli0))
                            els/normalize-els)
                  els-seg (get-path-segment src-k-sel els' eli-sel)]
              [:g.sel
               [ps/path-builder {} pi-sel els-seg]]))]

         [:g.coords
          (for [[pi p-opts els _] out-tups]
            (let [els'           (->> els
                                      (els/resolve-refs (:defs p-opts))
                                      (els/attach-normalized-meta))
                  [data-svg-tups
                   _svg-seq]     (ps/path (merge p-opts {:debug? true
                                                         :coords? true})
                                          els')]
              ^{:key pi}
              [:g
               (ps/plot-coords {:scaled        sc
                                :coord-size    10
                                :report!       report!
                                :report-hover! report-hov!
                                :sel           sel
                                :hov           hov
                                :controls?     (= pi-sel pi)}
                               pi els' data-svg-tups)]))]]]])))
