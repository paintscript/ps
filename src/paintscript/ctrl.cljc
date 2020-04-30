(ns paintscript.ctrl
  (:require [clojure.pprint :refer [pprint]]
            #?(:cljs [reagent.core :as r :refer [atom]])
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as w]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]
            [paintscript.el :as el]
            [paintscript.ops :as ops]
            [paintscript.nav :as nav]
            [paintscript.conv :as conv]))

(def params-init
  {:defs {},
   :script
   [[:path
     {}
     [:M [14 18]]
     [:C [45 17] [27 48] [59 42]]
     [:C [50 63] [84 73] [59 74]]]]})

(defn- xy-mouse [ev]
  [(-> ev .-clientX)
   (-> ev .-clientY)])

(defn- read-xy-str [[x y]]
  [(or (some-> x read-string) 0)
   (or (some-> y read-string) 0)])

(defn parse-cmd-line [cmd-line]
  (let [[cmd-str & args] (str/split cmd-line #" ")
        cmd-k (keyword cmd-str)]
    (if (el/el? cmd-k)
      (let [num-vecs (->> args (partition 2) (map read-xy-str))
            el       (vec (cons cmd-k num-vecs))]
        [:el-append el])
      (case cmd-str
        "undo"        [:undo]

        ;; --- mutate
        ("abs"
         "absolute")  [:rel->abs]
        "full"        [:short->full]
        ("norm"
         "normalize") [:normalize]
        "round"       (let [[?n] args]
                        [:round ?n])
        ("tl"
         "translate") (let [xy (read-xy-str args)]
                        [:translate xy])
        ("sc"
         "scale")     (let [ctr (read-xy-str (take 2 args))
                            n   (read-string (last args))]
                        [:scale ctr n])
        "mirror"      (let [[axis pos] args]
                        [:mirror (some-> axis read-string) (some-> pos read-string)])

        "to"          [:el-tf (-> args first keyword)]

        ;; --- configure
        "p-mirror"    (let [[?mode-str] args]
                        [:set-p-opts [:mirror {:mode (or (some-> ?mode-str keyword) :separate)}]])
        "class-k"     (let [[?class] args
                            class-k (or ?class "outline")]
                        [:set-p-opts [:class-k class-k]])
        "variant-k"   (let [[?variant] args
                            variant-k (or ?variant "outline")]
                        [:set-p-opts [:variant-k variant-k]])
        "disable"     [:set-p-opts [:disabled? true]]
        "enable"      [:set-p-opts [:disabled? false]]

        ;; --- nav
        "clear"       [:clear]
        "def"         (let [[pk] args]
                        [:def pk])
        "script"      (let [sel-path (map read-string args)]
                        [:sel (cons :script sel-path)])
        "svg"         [:svg-path (str/join " " args)]
        "snap"        [:toggle-snap]
        ("i"
         "insert")    [:toggle-insert]

        ;; else:
        (println (str "command not found: " cmd-line))))))

(defn- handle-op [params ui [op-k & [arg :as args] :as op]]
  (let [[src-k-sel
         pi-sel
         eli-sel
         xyi-sel :as sel] (:sel ui)]
    (case op-k
      :set-sel-d  (let [{:keys [params0]} (:snap ui)]
                    (let [params (or params0  ;; dnd
                                     params)] ;; kb
                      {:params
                       (reduce (fn [acc' {:as sel-item :keys [ii-xy main?]}]
                                 (-> acc'
                                     (assoc-in ii-xy
                                               (mapv + (get-in params ii-xy) arg))))
                               params
                               (:sel-set ui))}))

      :pth-append {:params (-> params (update :script ops/append-pth pi-sel))
                   :ui     (-> ui (merge {:sel nil :snap nil}))}

      :pth-del    {:params (-> params (update :script ops/del-pth pi-sel))
                   :ui     (-> ui (merge {:sel nil :snap nil}))}

      :el-append  (let [[src-k px eli] (or sel (ops/tail-iii params))
                        eli'           (or (some-> eli inc) 0)]
                    (-> (if-let [el arg]
                          {:params (-> params (update-in [src-k px] ops/append-el eli el))}
                          {:params (-> params (update-in [src-k px] ops/append-el eli))})
                        (assoc :ui (-> ui (merge {:sel [src-k px eli']})))))

      :el-del     (let [eli' (max (dec eli-sel) 0)]
                    {:params (-> params (update-in [src-k-sel pi-sel] ops/del-el eli-sel))
                     :ui     (-> ui (merge {:sel [src-k-sel pi-sel eli']}))})

      :el-tf      (let [to arg]
                    {:params (-> params (update-in [src-k-sel pi-sel]
                                                   ops/transform-el eli-sel to))
                     :ui     (-> ui (update :sel #(take 3 %)))})

      :xy-append  {:params (-> params (update-in [:script pi-sel]
                                                 ops/append-pnt eli-sel))
                   :ui     (-> ui (merge {:sel nil :snap nil}))}

      :xy-del     {:params (-> params (update-in [:script pi-sel]
                                                 ops/del-pnt eli-sel
                                                 (- xyi-sel nav/xyi0)))
                   :ui     (-> ui (merge {:sel nil :snap nil}))}

      :rel->abs   {:params (-> params (ops/absolute  sel))}
      :short->full {:params (-> params (ops/full      sel))}
      :normalize  {:params (-> params (ops/normalize sel))}
      :scale      (let [[ctr k] args]
                    {:params (-> params (ops/scale sel ctr k))})
      :mirror     (let [[axis pos] args]
                    {:params (-> params (ops/mirror (or axis 0) (or pos 100) sel))})

      :round      (let [n arg]
                    {:params
                     (-> params
                         ((fn [s]
                            (w/prewalk
                             (case n
                               "1" #(-> % (cond-> (number? %) u/round1))
                               "2" #(-> % (cond-> (number? %) u/round2))
                               #(-> % (cond-> (number? %) u/round)))
                             s))))})

      :translate  {:params (-> params (ops/translate sel arg))}

      :clear      {:params (-> params (merge params-init))
                   :ui     (-> ui (merge {:sel nil :snap nil}))}

      :def        (let [[pk] args
                        {:keys [defs]} params]
                    (if-let [els (get defs pk)]
                      ;; --- select
                      (let [eli (-> els count (- 1) (max 0))]
                        {:ui (-> ui (merge {:sel [:defs pk eli]}))})

                      ;; --- create
                      {:params (-> params (#(-> %
                                                (assoc-in [:defs pk] [])
                                                (update :script conj [:path {} [:ref pk]]))))
                       :ui     (-> ui (merge {:sel [:defs pk nil]}))}))

      :svg-path   (let [[svg-path] args]
                    (println :svg-path svg-path)
                    (let [p-new (conv/path-d->path svg-path)]
                      (pprint p-new)
                      {:params (-> params
                                   (update :script conj p-new))}))

      :sel        {:ui (-> ui
                           (merge {:sel arg})
                           (cond-> (nil? arg)
                                   (assoc :snap nil)))}

      :set-p-opts (let [[k v] arg]
                    {:params (-> params (ops/update-p-opts sel assoc k v))})

      :toggle-snap   {:ui (-> ui (update :snap-to-grid? not))}
      :toggle-insert {:ui (-> ui (update :insert-mode? not))})))

(defn dispatch! [!params !state-log !ui [op-k arg :as op]]
  (case op-k
    :cmd  (when-let [op-vec (parse-cmd-line arg)]
            (dispatch! !params !state-log !ui op-vec))

    :undo (let [_ (swap! !state-log rest)
                state-prev (first @!state-log)]
            (reset! !params (:params state-prev))
            (reset! !ui     (:ui     state-prev)))

    ;; else:
    (let [params @!params
          ui     @!ui]
      (try
        (let [{params' :params
               ui'     :ui} (handle-op params ui op)]
          (do
            (when params' (reset! !params params'))
            (when ui'     (reset! !ui ui'))
            (when-not (= :set-xy op-k)
              (swap! !state-log conj {:params (or params' params)
                                      :ui     (or ui'     ui)}))))
        (catch #?(:cljs :default :clj Exception) e
          (do
            (println :handle-op-exception e)
            #?(:cljs (js/console.log e))
            (reset! !params params)
            (reset! !ui ui)))))))

(defn- pth->cp-ii [pth eli]
  (let [eli' (inc eli)]
    (concat
     (when-let [cp-i (some-> (get pth eli)  (el/el->cp-i :term))] [[eli  cp-i]])
     (when-let [cp-i (some-> (get pth eli') (el/el->cp-i :init))] [[eli' cp-i]]))))

#?(:cljs
 (defn drag-and-drop-fns
   "attached to SVG element"
   [!scale !params !ui dispatch!]
   (let [!snap    (r/cursor !ui [:snap])
         !sel     (r/cursor !ui [:sel])
         !sel-set (r/cursor !ui [:sel-set])]
     {;; NOTE: invoked after canvas/report!
      :on-mouse-down #(let [{:keys [xy-svg sel sel-set insert-mode?]} @!ui
                            xy  (xy-mouse %)
                            scale @!scale
                            {:keys [main? shift?]} (meta sel)]
                        (cond
                          (= 4 (count sel))
                          ;; --- sel
                          (do
                            (let [sel-set'
                                  (into #{{:ii-xy sel :main? main?}}
                                        (map (fn [eli-cpi]
                                               {:ii-xy (concat (take 2 sel) eli-cpi)}))
                                        (when main?
                                          (pth->cp-ii (get-in @!params (take 2 sel))
                                                      (nth sel 2))))]
                              (if shift?
                                (swap!  !sel-set set/union sel-set')
                                (reset! !sel-set sel-set')))
                            (swap! !snap merge
                                   {:params0 @!params :m0 xy}))

                          insert-mode?
                          ;; --- insert
                          (let [xy  (xy-mouse %)
                                xy' (as-> (xy-mouse %) xy'
                                          (mapv - xy' xy-svg)
                                          (mapv / xy' [scale scale])
                                          (mapv u/round xy'))]
                            (dispatch! [:el-append [:L xy']]))))
      :on-mouse-move (fn [ev]
                       (let [{:keys [snap-to-grid?]
                              {:as snapshot :keys [params0 m0]} :snap} @!ui
                             scale @!scale]
                         (when params0
                           (let [m1  (xy-mouse ev)
                                 d   (mapv - m1 m0)
                                 d'  (mapv / d [scale scale])
                                 d'  (if snap-to-grid?
                                       (mapv u/round d')
                                       d')]
                             (dispatch! [:set-sel-d d'])))))
      :on-mouse-up   #(let [{:keys [sel sel-prev params0]} @!snap
                            params @!params]
                        (reset! !snap {:sel-prev sel})
                        (when (and sel sel-prev params0
                                   (= sel sel-prev)
                                   (= params0 params))
                          (reset! !sel  nil)
                          (reset! !snap nil)))})))

(defn keybind-fns [!params !ui dispatch!]
  (let [upd! #(swap! !params assoc-in (:sel @!ui) %)
        get! #(get-in @!params (:sel @!ui))]
    {"left"      #(dispatch! [:set-sel-d [-1 0]])
     "right"     #(dispatch! [:set-sel-d [1 0]])
     "up"        #(dispatch! [:set-sel-d [0 -1]])
     "down"      #(dispatch! [:set-sel-d [0 1]])
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:xy-del]))}))
