(ns paintscript.app.ctl
  (:require [clojure.pprint :refer [pprint]]
            #?(:cljs [reagent.core :as r :refer [atom]])
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as w]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]
            [paintscript.el-path :as el]
            [paintscript.ops :as ops]
            #?(:cljs [paintscript.app.ops-svg :as ops-svg])
            [paintscript.nav :as nav]
            [paintscript.conv :as conv]
            [paintscript.app.s-log :as s-log]))

(def cmpt-clear {:defs {} :script []})

(def cmpt0
  {:canvas {:dims [100 100]},
   :defs
   {:paintings
    {"reps"
     {:canvas {:dims [100 100]},
      :script
      [[:path
        {:class-k "solid", :repeat {:translate [30 0], :times 2}}
        [:circle {:center [20 50], :r 5}]]
       [:path
        {:class-k "solid",
         :repeat {:rotate {:degree 60, :center [50 50]}, :times 6}}
        [:M [50 8]]
        [:L [39 15] [61 15]]]]}}},
   :script
   [[:layout {} [:ref "reps"]]
    [:path
     {:class-k "outline",
      :repeat
      {:rotate {:degree 60, :center [50 50]}, :times 5, :mode :fuse}}
     [:M [57 37]]
     [:L [50 20]]
     [:L [43 37]]]

    [:path
     {:attrs {:style {:stroke "red"}}}
     [:M [10 10]]
     [:C [10 10] [90 10] [90 10]]]]})

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
        "log-clear"   [:log-clear]
        "log-clear<"  [:log-clear<]
        "log-clear>"  [:log-clear>]

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
        ("rt"
         "rotate")    (let [[a cx cy] args
                            alpha  (read-string a)
                            center (if (and cx cy)
                                     (read-xy-str [cx cy])
                                     [50 50])]
                        [:rotate alpha center])
        ("sc"
         "scale")     (let [[n cx cy] args
                            n      (read-string n)
                            center (if (and cx cy)
                                     (read-xy-str [cx cy])
                                     [50 50])]
                        [:scale center n])
        "mirror"      (let [[axis pos] args]
                        [:mirror
                         (some-> axis read-string)
                         (some-> pos  read-string)])

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
        ".png"        [:dl-png]
        ".svg"        [:dl-svg]

        ;; else:
        (println (str "command not found: " cmd-line))))))

(defn- handle-op [cmpt ui [op-k & [arg :as args] :as op]]
  (let [[src-k-sel
         pi-sel
         eli-sel
         xyi-sel :as sel] (:sel ui)]
    (case op-k
      :set-sel-d   (let [{:keys [cmpt0]} (:snap ui)
                         cmpt (or cmpt0  ;; dnd
                                  cmpt)] ;; kb
                     {:cmpt
                      (reduce (fn [acc' {:as sel-item :keys [ii-xy main?]}]
                                (-> acc'
                                    (assoc-in ii-xy
                                              (mapv + (get-in cmpt ii-xy) arg))))
                              cmpt
                              (:sel-set ui))})

      :pth-append  {:cmpt (-> cmpt (update :script ops/append-pth pi-sel))
                    :ui   (-> ui (merge {:sel nil :snap nil}))}

      :pth-del     {:cmpt (-> cmpt (update :script ops/del-pth pi-sel))
                    :ui   (-> ui (merge {:sel nil :snap nil}))}

      :el-append   (let [el             arg
                         [src-k px eli] (or sel
                                            (ops/tail-iii cmpt))
                         eli'           (or (some-> eli inc) 0)
                         sel'           [src-k px eli']
                         init?          (not (seq (:script cmpt)))]
                     {:cmpt (cond
                                init? (-> cmpt (update :script conj [:path {} el]))
                                el    (-> cmpt (update-in [src-k px] ops/append-el eli el))
                                :else (-> cmpt (update-in [src-k px] ops/append-el eli)))
                      :ui   (-> ui (assoc :sel sel'))})

      :el-del      (let [eli' (max (dec eli-sel) 0)]
                     {:cmpt (-> cmpt (update-in [src-k-sel pi-sel] ops/del-el eli-sel))
                      :ui   (-> ui   (merge {:sel [src-k-sel pi-sel eli']}))})

      :el-tf       (let [to arg]
                     {:cmpt (-> cmpt (update-in [src-k-sel pi-sel]
                                                    ops/transform-el eli-sel to))
                      :ui   (-> ui   (update :sel #(take 3 %)))})

      :xy-append   {:cmpt (-> cmpt (update-in [:script pi-sel]
                                                  ops/append-pnt eli-sel))
                    :ui   (-> ui   (merge {:sel nil :snap nil}))}

      :xy-del      {:cmpt (-> cmpt (update-in [:script pi-sel]
                                                  ops/del-pnt eli-sel
                                                  (- xyi-sel nav/xyi0)))
                    :ui   (-> ui   (merge {:sel nil :snap nil}))}

      :rel->abs    {:cmpt (-> cmpt (ops/absolute  sel))}
      :short->full {:cmpt (-> cmpt (ops/full      sel))}
      :normalize   {:cmpt (-> cmpt (ops/normalize sel))}
      :scale       (let [[ctr k] args]
                     {:cmpt (-> cmpt (ops/scale sel ctr k))})
      :mirror      (let [[axis pos] args]
                     {:cmpt (-> cmpt (ops/mirror (or axis 0) (or pos 100) sel))})

      :round       (let [n arg]
                     {:cmpt
                      (-> cmpt
                          ((fn [s]
                             (w/prewalk
                              (case n
                                "1" #(-> % (cond-> (number? %) u/round1))
                                "2" #(-> % (cond-> (number? %) u/round2))
                                #(-> % (cond-> (number? %) u/round)))
                              s))))})

      :translate   {:cmpt (-> cmpt (ops/translate sel arg))}

      :rotate      (let [[alpha center] args]
                     {:cmpt (-> cmpt (ops/rotate sel center alpha))})

      :clear       {:cmpt (-> cmpt (merge cmpt-clear))
                    :ui   (-> ui (merge {:sel nil :snap nil}))}

      :def         (let [[pk] args
                         {:keys [defs]} cmpt]
                     (if-let [els (get defs pk)]
                       ;; --- select
                       (let [eli (-> els count (- 1) (max 0))]
                         {:ui (-> ui (merge {:sel [:defs pk eli]}))})

                       ;; --- create
                       {:cmpt (-> cmpt (#(-> %
                                                 (assoc-in [:defs pk] [])
                                                 (update :script conj [:path {} [:ref pk]]))))
                        :ui   (-> ui (merge {:sel [:defs pk nil]}))}))

      :svg-path    (let [[svg-path] args]
                     (println :svg-path svg-path)
                     (let [p-new (conv/path-d->path svg-path)]
                       (pprint p-new)
                       {:cmpt (-> cmpt
                                    (update :script conj p-new))}))

      :sel         {:ui (-> ui
                            (merge {:sel arg})
                            (cond-> (nil? arg)
                                    (assoc :snap nil)))}

      :set-p-opts  (let [[k v] arg]
                     {:cmpt (-> cmpt (ops/update-p-opts sel assoc k v))})

      :toggle-snap   {:ui (-> ui (update :snap-to-grid? not))}
      :toggle-insert {:ui (-> ui (update :insert-mode? not))})))

(defn- get-wh [cmpt]
  (let [{:keys [dims scale]} (:canvas cmpt)]
    (mapv (partial * scale) dims)))

(defn dispatch! [!config !cmpt !s-log !ui
                 [op-k arg :as op]]
  (case op-k
    :cmd (when-let [op-vec (parse-cmd-line arg)]
           (dispatch! !config !cmpt !s-log !ui op-vec))

    :undo       (s-log/op !s-log !cmpt !ui :undo)
    :log-clear  (s-log/op !s-log !cmpt !ui :clear)
    :log-clear< (s-log/op !s-log !cmpt !ui :clear<)
    :log-clear> (s-log/op !s-log !cmpt !ui :clear>)

    (:dl-png
     :dl-svg)   #?(:cljs ((case op-k :dl-png
                            ops-svg/to-png!
                            ops-svg/to-svg)
                          (get-wh (merge @!config
                                         @!cmpt))
                          (:svg-dom @!ui)
                          #(-> % (ops-svg/download!
                                  (str "paintscript"
                                       (case op-k :dl-png ".png" ".svg")))))
                         :clj nil)

    ;; else:
    (let [cmpt @!cmpt
          ui   @!ui]
      (try
        (let [{cmpt' :cmpt
               ui'   :ui} (handle-op cmpt ui op)]
          (do
            (when cmpt' (reset! !cmpt cmpt'))
            (when ui'     (reset! !ui ui'))
            (s-log/add !s-log {:op   op
                               :cmpt (or cmpt' cmpt)
                               :ui   (-> (or ui'     ui)
                                         (cond-> (= :set-sel-d op-k)
                                                 (assoc :snap nil)))})))
        (catch #?(:cljs :default :clj Exception) e
          (do
            (println :handle-op-exception e)
            #?(:cljs (js/console.log e))
            (reset! !cmpt cmpt)
            (reset! !ui ui)))))))

(defn- pth->cp-ii [pth eli]
  (let [eli' (inc eli)]
    (concat
     (when-let [cp-i (some-> (get pth eli)  (el/el->cp-i :term))] [[eli  cp-i]])
     (when-let [cp-i (some-> (get pth eli') (el/el->cp-i :init))] [[eli' cp-i]]))))

#?(:cljs
 (defn drag-and-drop-fns
   "attached to SVG element"
   [!scale !cmpt !ui dispatch!]
   (let [!snap    (r/cursor !ui [:snap])
         !sel     (r/cursor !ui [:sel])
         !sel-set (r/cursor !ui [:sel-set])]
     {;; NOTE: invoked after canvas/report!
      :on-mouse-down #(let [{:keys
                             [xy-svg!
                              sel
                              sel-set
                              insert-mode?]} @!ui
                            xy               (xy-mouse %)
                            scale            @!scale
                            {:keys
                             [main?
                              shift?]}       (meta sel)]
                        (cond
                          (= 4 (count sel))
                          ;; --- sel
                          (do
                            (let [sel-set'
                                  (into #{{:ii-xy sel
                                           :main? main?}}
                                        (map (fn [eli-cpi]
                                               {:ii-xy (concat (take 2 sel) eli-cpi)}))
                                        (when main?
                                          (pth->cp-ii (get-in @!cmpt (take 2 sel))
                                                      (nth sel 2))))]
                              (if shift?
                                (swap!  !sel-set set/union sel-set')
                                (reset! !sel-set sel-set')))
                            (swap! !snap merge
                                   {:cmpt0 @!cmpt :m0 xy}))

                          insert-mode?
                          ;; --- insert
                          (let [xy  (xy-mouse %)
                                xy' (as-> (xy-mouse %) xy'
                                          (mapv - xy' (xy-svg!))
                                          (mapv / xy' [scale scale])
                                          (mapv u/round xy'))
                                el-k (if (seq (:script @!cmpt))
                                       :L
                                       :M)]
                            (dispatch! [:el-append [el-k xy']]))))
      :on-mouse-move (fn [ev]
                       (let [{:keys [snap-to-grid?]
                              {:as snapshot :keys [cmpt0 m0]} :snap} @!ui
                             scale @!scale]
                         (when cmpt0
                           (let [m1  (xy-mouse ev)
                                 d   (mapv - m1 m0)
                                 d'  (mapv / d [scale scale])
                                 d'  (if snap-to-grid?
                                       (mapv u/round d')
                                       d')]
                             (dispatch! [:set-sel-d d'])))))
      :on-mouse-up   #(let [{:keys [sel sel-prev cmpt0]} @!snap
                            cmpt @!cmpt]
                        (reset! !snap {:sel-prev sel})
                        (when (and sel sel-prev cmpt0
                                   (= sel sel-prev)
                                   (= cmpt0 cmpt))
                          (reset! !sel  nil)
                          (reset! !snap nil)))})))

(defn keybind-fns [!cmpt !ui dispatch!]
  (let [upd! #(swap! !cmpt assoc-in (:sel @!ui) %)
        get! #(get-in @!cmpt (:sel @!ui))]
    {"left"      #(dispatch! [:set-sel-d [-1 0]])
     "right"     #(dispatch! [:set-sel-d [1 0]])
     "up"        #(dispatch! [:set-sel-d [0 -1]])
     "down"      #(dispatch! [:set-sel-d [0 1]])
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:xy-del]))}))
