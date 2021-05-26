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

            [paintscript.nav :as nav]
            [paintscript.conv :as conv]
            #?(:cljs [paintscript.app.ops-svg :as ops-svg])
            [paintscript.app.s-log :as s-log]))

(def cmpt-clear {:defs {} :script []})

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

        "reverse"     [:reverse]

        "mirror"      (let [[axis pos] args]
                        [:mirror
                         (some-> axis read-string)
                         (some-> pos  read-string)])

        "to"          [:el-tf (-> args first keyword)]

        ;; --- configure
        "p-mirror"    (let [[?mode-str] args]
                        [:set-p-opts [:mirror {:mode (or (some-> ?mode-str keyword) :separate)}]])
        "attr-class"     (let [[?class] args
                            attr-class (or ?class "outline")]
                        [:set-p-opts [:attr-class attr-class]])
        "variant-key" (let [[?variant] args
                            variant-key (or ?variant "outline")]
                        [:set-p-opts [:variant-key variant-key]])
        "disable"     [:set-p-opts [:disabled? true]]
        "enable"      [:set-p-opts [:disabled? false]]

        ;; --- nav
        "clear"       [:clear]
        "def"         (let [[pk] args]
                        [:def pk])
        "script"      (let [sel-path (map read-string args)]
                        [:sel-rec (cons :script sel-path)])
        "svg"         [:svg-path (str/join " " args)]
        "snap"        [:toggle-snap]
        ("i"
         "insert")    [:toggle-insert]
        ".png"        [:dl-png]
        ".svg"        [:dl-svg]

        ;; else:
        (println (str "command not found: " cmd-line))))))

(defn- handle-op
  [cmpt {:as ui :keys [sel-rec]} [op-k & [arg :as args] :as op]]
  (case op-k
    :set-sel-d   (let [{:keys [cmpt0]} (:snap ui)
                       cmpt (or cmpt0  ;; dnd
                                cmpt)] ;; kb
                   {:cmpt
                    (reduce (fn [acc' {:as sel-item :keys [pth-rec main?]}]
                              (let [pth-vec (nav/pth-rec->vec pth-rec)]
                                (-> acc'
                                    (assoc-in pth-vec
                                              (mapv + (get-in cmpt pth-vec) arg)))))
                            cmpt
                            (:sel-set ui))})

    :pth-append  {:cmpt (-> cmpt (update :script ops/append-pth (:x-el-k sel-rec)))
                  :ui   (-> ui   (merge {:sel-rec nil :snap nil}))}

    :pth-del     {:cmpt (-> cmpt (update :script ops/del-pth (:x-el-k sel-rec)))
                  :ui   (-> ui   (merge {:sel-rec nil :snap nil}))}

    :el-append   (let [el         arg
                       {:as sel-rec
                        :keys
                        [src-k
                         x-el-k
                         p-el-i]} (or sel-rec
                                      (-> cmpt
                                          ops/tail-iii
                                          nav/pth-vec->rec))
                       sel-rec'   (-> sel-rec
                                      (update :p-el-i #(or (some-> % inc)
                                                           0)))
                       init?      (not (seq (:script cmpt)))]
                   {:cmpt (cond
                            init? (-> cmpt (update :script conj [:path {} el]))
                            el    (-> cmpt (update-in [src-k x-el-k] ops/append-el p-el-i el))
                            :else (-> cmpt (update-in [src-k x-el-k] ops/append-el p-el-i)))
                    :ui   (-> ui (assoc :sel-rec sel-rec'))})

    :el-del      (let [eli' (max (dec (:p-el-i sel-rec)) 0)]
                   {:cmpt (-> cmpt (update-in [(:src-k sel-rec) (:x-el-k sel-rec)] ops/del-el (:p-el-i sel-rec)))
                    :ui   (-> ui   (merge {:sel-rec [(:src-k sel-rec) (:x-el-k sel-rec) eli']}))})

    :el-tf       (let [to arg]
                   {:cmpt (-> cmpt (update-in [(:src-k sel-rec) (:x-el-k sel-rec)]
                                              ops/transform-el (:p-el-i sel-rec) to))
                    :ui   (-> ui   (update :sel-rec #(take 3 %)))})

    :xy-append   {:cmpt (-> cmpt (update-in [:script (:x-el-k sel-rec)]
                                            ops/append-pnt (:p-el-i sel-rec)))
                  :ui   (-> ui   (merge {:sel-rec nil :snap nil}))}

    :xy-del      {:cmpt (-> cmpt (update-in [:script (:x-el-k sel-rec)]
                                            ops/del-pnt (:p-el-i sel-rec)
                                            (- (:xy-i sel-rec) nav/xy-i0)))
                  :ui   (-> ui   (merge {:sel-rec nil :snap nil}))}

    :rel->abs    {:cmpt (-> cmpt (ops/absolute  sel-rec))}
    :short->full {:cmpt (-> cmpt (ops/full      sel-rec))}
    :normalize   {:cmpt (-> cmpt (ops/normalize sel-rec))}
    :scale       (let [[ctr k] args]
                   {:cmpt (-> cmpt (ops/scale sel-rec ctr k))})
    :mirror      (let [[axis pos] args]
                   {:cmpt (-> cmpt (ops/mirror (or axis 0) (or pos 100) sel-rec))})

    :reverse     {:cmpt (-> cmpt (ops/reverse-path sel-rec))}

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

    :translate   {:cmpt (-> cmpt (ops/translate sel-rec arg))}

    :rotate      (let [[alpha center] args]
                   {:cmpt (-> cmpt (ops/rotate sel-rec center alpha))})

    :clear       {:cmpt (-> cmpt (merge cmpt-clear))
                  :ui   (-> ui (merge {:sel-rec nil :snap nil}))}

    :def         (let [[pk] args
                       {:keys [defs]} cmpt]
                   (if-let [els (get defs pk)]
                     ;; --- select
                     (let [eli (-> els count (- 1) (max 0))]
                       {:ui (-> ui (merge {:sel-rec [:defs pk eli]}))})

                     ;; --- create
                     {:cmpt (-> cmpt (#(-> %
                                           (assoc-in [:defs pk] [])
                                           (update :script conj [:path {} [:ref pk]]))))
                      :ui   (-> ui (merge {:sel-rec [:defs pk nil]}))}))

    :svg-path    (let [[svg-path] args]
                   (println :svg-path svg-path)
                   (let [p-new (conv/path-d->path svg-path)]
                     (pprint p-new)
                     {:cmpt (-> cmpt
                                (update :script conj p-new))}))

    :sel-rec     {:ui (-> ui
                          (merge {:sel-rec arg})
                          (cond-> (nil? arg)
                                  (assoc :snap nil)))}

    :sel-vec     (handle-op cmpt ui [:sel-rec (nav/pth-vec->rec arg)])

    :set-p-opts  (let [[k v] arg]
                   {:cmpt (-> cmpt (ops/update-p-opts sel-rec assoc k v))})

    :toggle-snap   {:ui (-> ui (update :snap-to-grid? not))}
    :toggle-insert {:ui (-> ui (update :insert-mode? not))}))

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
     [!cmpt !ui dispatch!]
     (fn _derive-dnd-fns [!svg-dom scale]
       (let [!snap    (r/cursor !ui [:snap])
             !sel-rec (r/cursor !ui [:sel-rec])
             !sel-set (r/cursor !ui [:sel-set])
             xy-svg!  (fn []
                        (let [rect (-> @!svg-dom (.getBoundingClientRect))]
                          [(-> rect .-left)
                           (-> rect .-top)]))]
         {;; NOTE: invoked after canvas/report!
          :on-mouse-down #(let [{:keys
                                 [sel-rec
                                  sel-set
                                  insert-mode?]} @!ui
                                xy               (xy-mouse %)
                                {:keys
                                 [main?
                                  shift?]}       (meta sel-rec)
                                ]
                            (cond
                              (nav/xy-pth? sel-rec)
                              ;; --- sel
                              (do
                                (let [sel-vec (-> sel-rec
                                                  nav/pth-rec->vec)
                                      sel-set'
                                      (into #{{:pth-rec sel-rec
                                               :main?   main?}}
                                            (map (fn [eli-cpi]
                                                   {:pth-rec (-> sel-rec
                                                                 (assoc :xy-i eli-cpi))}))
                                            (when main?
                                              (pth->cp-ii (get-in @!cmpt (take 2 sel-vec))
                                                          (nth sel-vec 2))))]
                                  (if shift?
                                    (swap!  !sel-set set/union sel-set')
                                    (reset! !sel-set sel-set')))
                                (swap! !snap merge
                                       {:cmpt0 @!cmpt
                                        :m0    xy}))

                              insert-mode?
                              ;; --- insert
                              (let [xy-ptr (xy-mouse %)
                                    xy-svg (xy-svg!)
                                    xy'    (as-> xy-ptr xy*
                                                 (mapv - xy* xy-svg)
                                                 (mapv / xy* [scale scale])
                                                 (mapv u/round xy*))
                                    p-el-k   (if (seq (:script @!cmpt))
                                             :L
                                             :M)]
                                (dispatch! [:el-append [p-el-k xy']]))))

          :on-mouse-move (fn [ev]
                           (let [{:keys [snap-to-grid?]
                                  {:as snapshot :keys [cmpt0 m0]} :snap} @!ui]
                             (when cmpt0
                               (let [m1  (xy-mouse ev)
                                     d   (mapv - m1 m0)
                                     d'  (mapv / d [scale scale])
                                     d'  (if snap-to-grid?
                                           (mapv u/round d')
                                           d')]
                                 (dispatch! [:set-sel-d d'])))))

          :on-mouse-up   #(let [{:keys [sel-rec
                                        sel-prev cmpt0]} @!snap
                                cmpt @!cmpt]
                            (reset! !snap {:sel-prev sel-rec})
                            (when (and sel-rec
                                       sel-prev cmpt0
                                       (= sel-rec
                                          sel-prev)
                                       (= cmpt0 cmpt))
                              (reset! !sel-rec nil)
                              (reset! !snap    nil)))}))))

(defn keybind-fns [!cmpt !ui dispatch!]
  (let [upd! #(swap! !cmpt assoc-in (:sel-rec @!ui) %)
        get! #(get-in @!cmpt (:sel-rec @!ui))]
    {"left"      #(dispatch! [:set-sel-d [-1 0]])
     "right"     #(dispatch! [:set-sel-d [1 0]])
     "up"        #(dispatch! [:set-sel-d [0 -1]])
     "down"      #(dispatch! [:set-sel-d [0 1]])
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:xy-del]))
     "c"         #(dispatch! [:el-tf :C])
     "q"         #(dispatch! [:el-tf :Q])
     "s"         #(dispatch! [:el-tf :S])}))
