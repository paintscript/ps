(ns paintscript.core
  (:require #?(:cljs [reagent.core :as r :refer [atom]])
            [clojure.string :as str]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]
            [paintscript.els :as els]
            [paintscript.ops :as ops]
            [paintscript.nav :as nav]))

(defn- xy-mouse [ev]
  [(-> ev .-clientX)
   (-> ev .-clientY)])

(defn dispatch! [!script !sel [op-k & [arg :as args]]]
  (let [[pi-sel
         eli-sel
         xyi-sel :as sel] @!sel]
    (case op-k
      :set-xy     (swap! !script assoc-in (cons :script @!sel) arg)
      :pth-append (do
                    (reset! !sel nil)
                    (swap! !script update :script ops/append-pth pi-sel))
      :pth-del    (do
                    (reset! !sel nil)
                    (swap! !script update :script ops/del-pth pi-sel))
      :el-append  (do
                    (reset! !sel nil)
                    (swap! !script update-in [:script pi-sel]
                           ops/append-el eli-sel))
      :el-del     (do
                    (reset! !sel nil)
                    (swap! !script update-in [:script pi-sel]
                           ops/del-el eli-sel))
      :xy-append  (do
                     (reset! !sel nil)
                     (swap! !script update-in [:script pi-sel]
                            ops/append-pnt eli-sel))
      :xy-del     (do
                     (reset! !sel nil)
                     (swap! !script update-in [:script pi-sel]
                            ops/del-pnt eli-sel
                            (- xyi-sel nav/xyi0))))))

(defn drag-and-drop-fns [scaled !script !sel dispatch!]
  (let [!snap  (atom nil)
        get!   #(get-in @!script (cons :script @!sel))]
    {:on-mouse-down #(reset! !snap {:xy0 (get!) :m0 (xy-mouse %)})
     :on-mouse-move (fn [ev]
                      (when-let [{:as snap :keys [xy0 m0]} @!snap]
                        (let [m1  (xy-mouse ev)
                              d   (mapv - m1 m0)
                              d'  (mapv / d scaled)
                              d'  (mapv u/round d')
                              xy' (mapv + xy0 d')]
                          (dispatch! [:set-xy xy']))))
     :on-mouse-up   #(reset! !snap nil)}))

(defn keybind-fns [scaled !script !sel dispatch!]
  (let [upd! #(swap! !script assoc-in @!sel %)
        get! #(get-in @!script (cons :script @!sel))]
    {"left"      #(when-let [[x y] (get!)] (dispatch! [:set-xy [(- x 1) y]]))
     "right"     #(when-let [[x y] (get!)] (dispatch! [:set-xy [(+ x 1) y]]))
     "up"        #(when-let [[x y] (get!)] (dispatch! [:set-xy [x (- y 1)]]))
     "down"      #(when-let [[x y] (get!)] (dispatch! [:set-xy [x (+ y 1)]]))
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:xy-del]))}))

(def ^:private relative? #{:c :s})

(defn coord [{:keys [scaled report! report-hover! coord-size controls? hov]
              [pi-sel eli-sel xyi-sel :as sel] :sel}
             el-k els xy [pi eli _ :as iii]]
  (#?(:cljs r/with-let :clj let) [!hover? (atom false)]
    (let [[x y] (->> (if (relative? el-k)
                       (let [out (-> xy els/abs-meta)]
                         (assert out)
                         out)
                       xy)
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

(defn plot-coords [opts pi els pnt-tups]
  (for [[args eli xyi0 el-k] (map first pnt-tups)
        [xyi xy] (map-indexed vector args)
        :let [iii [pi (+ eli nav/eli0) (+ xyi xyi0)]]]
    ^{:key (hash iii)}
    [coord opts el-k els xy iii]))

(def path els/path)

(defn path-builder
  ([opts els] (path-builder opts 0 els))
  ([{:as opts :keys [debug? attrs]}
    pi els]
   (let [[pnt-tups points] (els/path (assoc opts :debug? true) els)]
     [:g
      (if debug?
        (plot-coords opts pi els pnt-tups)
        [:path (merge attrs
                      {:d (apply d points)})])])))

(defn paint [{:as script-opts :keys [styles]} script]
  [:g
   (for [[pi {:as path-opts :keys [class-k]} els]
         (->> script
              (map-indexed
               (fn [pi [_ path-opts & els :as path]]
                 [pi path-opts els])))
         :let [path-opts' (-> path-opts
                              (cond-> class-k
                                      (update :attrs merge (get styles class-k))))]]
     ^{:key pi}
     [path-builder path-opts' pi els])])
