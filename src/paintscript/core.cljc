(ns paintscript.core
  (:require #?(:cljs [reagent.core :as r :refer [atom]])
            [clojure.string :as str]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]
            [paintscript.pth-vecs :as pth-vecs]
            [paintscript.ops :as ops]))

(defn- xy-mouse [ev]
  [(-> ev .-clientX)
   (-> ev .-clientY)])

(defn dispatch! [!script !sel [k & [arg :as args]]]
  (let [[pth-i-sel
         pth-vec-i-sel
         pnt-i-sel :as sel] @!sel]
    (case k
      :set-xy         (swap! !script assoc-in (cons :script @!sel) arg)
      :pth-append     (do
                        (reset! !sel nil)
                        (swap! !script update :script ops/append-pth pth-i-sel))
      :pth-del        (do
                        (reset! !sel nil)
                        (swap! !script update :script ops/del-pth pth-i-sel))
      :pth-vec-append (do
                        (reset! !sel nil)
                        (swap! !script update-in [:script pth-i-sel]
                               ops/append-pth-vec pth-vec-i-sel))
      :pth-vec-del    (do
                        (reset! !sel nil)
                        (swap! !script update-in [:script pth-i-sel]
                               ops/del-pth-vec pth-vec-i-sel))
      :pnt-append     (do
                        (reset! !sel nil)
                        (swap! !script update-in [:script pth-i-sel]
                               ops/append-pnt pth-vec-i-sel))
      :pnt-del        (do
                        (reset! !sel nil)
                        (swap! !script update-in [:script pth-i-sel]
                               ops/del-pnt pth-vec-i-sel
                               (- pnt-i-sel pth-vecs/i-pnt0))))))

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
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:pnt-del]))}))

(def ^:private relative? #{:c :s})

(defn coord [{:keys [scaled report! report-hover! coord-size controls? hov]
              [i-pth-sel i-pth-vec-sel i-pnt-sel :as sel] :sel}
             k pth-vecs xy [i-pth i-pth-vec _ :as iii]]
  (#?(:cljs r/with-let :clj let) [!hover? (atom false)]
    (let [[x y] (->> (if (relative? k)
                       (-> xy pth-vecs/abs-meta)
                       xy)
                     (mapv #(* % scaled)))
          i-tgt     (-> xy meta :i-tgt)
          cp?       (some? i-tgt)
          hover?    (= iii hov)
          sel-pv?   (and (= i-pth-sel     i-pth)
                         (= i-pth-vec-sel i-pth-vec))
          sel-pnt?  (= iii sel)]
      (when (or (not cp?) sel-pv?)
        [:g
         (when cp?
           (let [[x2 y2] (-> pth-vecs
                             (nth i-tgt)
                             last
                             (cond-> (relative? k)
                                     (#(-> % pth-vecs/abs-meta (or %))))
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

(defn plot-coords [opts pth-i pth-vecs pnt-tups]
  (for [[args i-pth-vec i-pnt0 k] (map first pnt-tups)
        [i-pnt pnt] (map-indexed vector args)
        :let [iii [pth-i (+ i-pth-vec pth-vecs/i-pth-vec0) (+ i-pnt0 i-pnt)]]]
    ^{:key (hash iii)}
    [coord opts k pth-vecs pnt iii]))

(def path pth-vecs/path)

(defn path-builder
  ([opts pth-vecs] (path-builder opts 0 pth-vecs))
  ([{:as opts :keys [debug? attrs]}
    pth-i pth-vecs]
   (let [[pnt-tups points] (pth-vecs/path (assoc opts :debug? true) pth-vecs)]
     [:g
      (if debug?
        (plot-coords opts pth-i pth-vecs pnt-tups)
        [:path (merge attrs
                      {:d (apply d points)})])])))

(defn paint [{:as script-opts :keys [styles]} script]
  [:g
   (for [[pth-i {:as path-opts :keys [class-k]} pth-vv]
         (->> script
              (map-indexed
               (fn [pth-i [_ path-opts & pth-vv :as path]]
                 [pth-i path-opts pth-vv])))
         :let [path-opts' (-> path-opts
                              (cond-> class-k
                                      (update :attrs merge (get styles class-k))))]]
     ^{:key pth-i}
     [path-builder path-opts' pth-i pth-vv])])
