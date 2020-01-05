(ns paintscript.core
  (:require #?(:cljs [reagent.core :as r :refer [atom]])
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.string :as str]
            [clojure.walk :as w]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]
            [paintscript.el :as el]
            [paintscript.els :as els]
            [paintscript.ops :as ops]
            [paintscript.nav :as nav]))

(defn- xy-mouse [ev]
  [(-> ev .-clientX)
   (-> ev .-clientY)])

(defn read-xy-str [[x y]]
  [(or (some-> x read-string) 0)
   (or (some-> y read-string) 0)])

(defn dispatch! [!params !sel [op-k & [arg :as args]]]
  (let [[src-k-sel
         pi-sel
         eli-sel
         xyi-sel :as sel] @!sel]
    (case op-k
      :set-xy     (do
                    (swap! !params assoc-in @!sel arg))
      :cmd        (let [[cmd & args] (str/split arg #" ")
                        cmd (keyword cmd)]
                    (if (el/el? cmd)
                      (let [vecs (->> args (partition 2) (map read-xy-str))
                            el   (vec (cons cmd vecs))
                            [src-k px eli :as sel] (or @!sel
                                                       (ops/tail-iii @!params))]
                        (do
                          (swap! !params update-in [src-k px]
                                 ops/append-el eli el)
                          (reset! !sel [src-k px (or (some-> eli inc) 0)])))

                      (case cmd
                        :absolute
                        (if-let [iii @!sel]
                          (swap! !params ops/absolute (take 2 iii))
                          (swap! !params ops/absolute))

                        :round
                        (let [n   (first args)
                              iii @!sel]
                          (swap! !params
                                 (fn [s]
                                   (w/prewalk
                                    (case n
                                      "1" #(-> % (cond-> (number? %) u/round1))
                                      "2" #(-> % (cond-> (number? %) u/round2))
                                      #(-> % (cond-> (number? %) u/round)))
                                    s))))

                        :translate
                        (let [iii @!sel]
                          (swap! !params ops/tl-pth (take 2 iii) (read-xy-str args)))

                        :clear
                        (do
                          (reset! !sel nil)
                          (swap! !params merge
                                 {:defs {}
                                  :script [[:path {:variant-k "outline" :class-k "outline"}
                                            [:M [10 10]]]]}))

                        :def
                        (let [[pk] args
                              {:keys [defs]} @!params]
                          (if-let [els (get defs pk)]
                            (let [eli (-> els count (- 1) (max 0))]
                              (reset! !sel [:defs pk eli]))
                            (do
                              (swap! !params
                                     #(-> %
                                          (assoc-in [:defs pk] [])
                                          (update :script conj [:path {} [:ref pk]])))
                              (reset! !sel [:defs pk nil]))))

                        :script
                        (let [iii (cons :script (map read-string args))]
                          (reset! !sel iii))


                        ;; p-opts
                        :mirror
                        (let [[mode-str] args
                              mode-k (or (some-> mode-str keyword) :separate)]
                          (swap! !params ops/update-p-opts @!sel
                                 assoc :mirror mode-k))

                        :class-k
                        (let [[class] args
                              class-k (or class "outline")]
                          (swap! !params ops/update-p-opts @!sel
                                 assoc :class-k class-k))

                        :variant-k
                        (let [[variant] args
                              variant-k (or variant "outline")]
                          (swap! !params ops/update-p-opts @!sel
                                 assoc :variant-k variant-k))

                        (println (str "command not found: " cmd)))))

      :pth-append (do
                    (reset! !sel nil)
                    (swap! !params update :script ops/append-pth pi-sel))
      :pth-del    (do
                    (reset! !sel nil)
                    (swap! !params update :script ops/del-pth pi-sel))
      :el-append  (do
                    (reset! !sel nil)
                    (swap! !params update-in [src-k-sel pi-sel]
                           ops/append-el eli-sel))
      :el-del     (do
                    (reset! !sel nil)
                    (swap! !params update-in [src-k-sel pi-sel]
                           ops/del-el eli-sel))
      :xy-append  (do
                     (reset! !sel nil)
                     (swap! !params update-in [:script pi-sel]
                            ops/append-pnt eli-sel))
      :xy-del     (do
                     (reset! !sel nil)
                     (swap! !params update-in [:script pi-sel]
                            ops/del-pnt eli-sel
                            (- xyi-sel nav/xyi0))))))

(defn drag-and-drop-fns [!scale !params !sel dispatch!]
  (let [!snap  (atom nil)
        get!   #(get-in @!params @!sel)]
    {:on-mouse-down #(swap! !snap merge {:sel @!sel :xy0 (get!) :m0 (xy-mouse %)})
     :on-mouse-move (fn [ev]
                      (let [{:as snap :keys [xy0 m0]} @!snap
                            scale @!scale]
                        (when xy0
                          (let [m1  (xy-mouse ev)
                                d   (mapv - m1 m0)
                                d'  (mapv / d [scale scale])
                                d'  (mapv u/round d')
                                xy' (mapv + xy0 d')]
                            (dispatch! [:set-xy xy'])))))
     :on-mouse-up   #(let [{:keys [sel sel-prev xy0]} @!snap
                           xy (get!)]
                       (reset! !snap {:sel-prev sel})
                       (when (and sel sel-prev xy0
                                  (= sel sel-prev)
                                  (= xy0 xy))
                         (reset! !sel  nil)
                         (reset! !snap nil)))}))

(defn keybind-fns [!params !sel dispatch!]
  (let [upd! #(swap! !params assoc-in @!sel %)
        get! #(get-in @!params @!sel)]
    {"left"      #(when-let [[x y] (get!)] (dispatch! [:set-xy [(- x 1) y]]))
     "right"     #(when-let [[x y] (get!)] (dispatch! [:set-xy [(+ x 1) y]]))
     "up"        #(when-let [[x y] (get!)] (dispatch! [:set-xy [x (- y 1)]]))
     "down"      #(when-let [[x y] (get!)] (dispatch! [:set-xy [x (+ y 1)]]))
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:xy-del]))}))

(def ^:private relative? #{:c :s})

(defn coord [{:keys [scaled report! report-hover! coord-size controls? hov]
              [src-k-sel pi-sel eli-sel xyi-sel :as sel] :sel}
             els el-k el
             xy [src-k pi eli _ :as iii]]
  (#?(:cljs r/with-let :clj let) [!hover? (atom false)]
    (let [[x y] (->> (-> xy (cond-> (relative? el-k) els/abs-meta))
                     (mapv #(* % scaled)))
          i-tgt     (-> xy meta :i-tgt)
          cp?       (some? i-tgt)
          hover?    (= iii hov)
          sel-pv?   (and (= pi-sel  pi)
                         (= eli-sel eli))
          sel-pnt?  (= iii sel)]
      (when (or (not cp?) sel-pv?)
        [:g
         (when cp?
           (let [[x2 y2] (-> els
                             (nth i-tgt)
                             last
                             (cond-> (relative? el-k)
                                     (#(-> % els/abs-meta (or %))))
                             (->> (mapv #(* % scaled))))]
            [:g
             [:line.ctrl-target.under {:x1 x :y1 y :x2 x2 :y2 y2}]
             [:line.ctrl-target.over  {:x1 x :y1 y :x2 x2 :y2 y2}]]))
         [:g {:style {:cursor "pointer" :text-select "none"}
              :on-mouse-down #(report!       iii)
              :on-mouse-over #(report-hover! iii true)
              :on-mouse-out  #(report-hover! iii false)
              :class (str (if i-tgt "control" "target")
                          (when hover? " hover")
                          (when (= sel iii) " selected"))}
          (if cp?
            [:g.cp
             [:rect {:x (- x (/ coord-size 2))
                     :y (- y (/ coord-size 2))
                     :width  coord-size
                     :height coord-size}]]

            (if (or sel-pnt? hover?)
              [:g
               [:circle {:cx x :cy y :r (* coord-size 1.8)}]
               (when-not (or sel-pnt? cp?)
                 [:text {:x x :y y
                         :fill "white"
                         :font-size coord-size
                         :text-anchor "middle"
                         :dominant-baseline "middle"
                         :style {:user-select "none"}}
                  (str/join " " xy)])]

              [:circle {:cx x :cy y :r coord-size}]))]]))))

(defn plot-coords [opts pi els data-svg-tups]
  (for [[args _eli xyi0 el-k el] (map first data-svg-tups)
        [xyi xy] (reverse (map-indexed vector args))
        :let [iii (some-> el meta :ii (concat [(+ xyi xyi0)]))
              _   (assert iii)]]
    ^{:key (hash iii)}
    [coord opts els el-k el xy iii]))

(def path els/path)

(defn path-builder
  ([opts els] (path-builder opts 0 els))
  ([{:as opts :keys [debug? attrs]}
    pi els]
   (let [[data-svg-tups svg-seq] (els/path (assoc opts :debug? true) els)]
     [:g
      (if debug?
        (plot-coords opts pi els data-svg-tups)
        [:path (merge attrs {:d (apply d svg-seq)})])])))

(defn paint [{:as script-opts :keys [variant defs styles attrs script data?]}]
  [:g
   (for [[pi [obj-k {:as p-opts :keys [variant-k class-k]} & els :as obj]]
         (map-indexed vector script)
         :when (or (not variant)
                   (not variant-k)
                   (= variant variant-k))
         :let [styles-attrs (if class-k
                              (get styles class-k
                                   (get styles "outline"))
                              (get styles "outline"))
               p-opts' (-> p-opts
                           (merge script-opts)
                           (update :attrs merge styles-attrs))]]

     (with-meta
       (case obj-k
         :path (path-builder p-opts' pi els)
         (-> obj
             (update 1 #(-> %
                            (dissoc :class-k :variant-k)
                            (merge attrs
                                   styles-attrs)))))
       {:key pi}))])
