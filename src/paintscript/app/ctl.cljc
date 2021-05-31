(ns paintscript.app.ctl
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.edn :as edn]

            #?(:cljs [reagent.core :as r :refer [atom]])
            [svg-hiccup-kit.core :refer [d d2]]

            [paintscript.util :as u]

            [paintscript.data :as data]
            [paintscript.ops.ops-path :as ops-path]
            [paintscript.ops.ops-path-tf :as ops-path-tf]
            [paintscript.ops.ops-cmpt :as ops-cmpt]

            [paintscript.nav :as nav]
            [paintscript.conv :as conv]

            [paintscript.app.s-log :as s-log]
            #?(:cljs [paintscript.app.ops-svg :as ops-svg])
            #?(:cljs [paintscript.app.s-app :as s-app])))

(def cmpt-clear {:defs {} :script []})

(defn- xy-mouse [ev]
  [(-> ev .-clientX)
   (-> ev .-clientY)])

(defn- read-xy-str [[x y]]
  [(or (some-> x edn/read-string) 0)
   (or (some-> y edn/read-string) 0)])

(defn parse-cmd-line [cmd-line]
  (let [[cmd-str & args] (str/split cmd-line #" ")
        cmd-k (keyword cmd-str)]
    (if (ops-path/el? cmd-k)
      (let [num-vecs (->> args (partition 2) (map read-xy-str))
            el       (vec (cons cmd-k num-vecs))]
        [:el-append el])
      (case cmd-str
        "undo"        [:op.s-log/undo]
        "log-clear"   [:op.s-log/clear]
        "log-clear<"  [:op.s-log/clear<]
        "log-clear>"  [:op.s-log/clear>]

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
                            alpha  (edn/read-string a)
                            center (if (and cx cy)
                                     (read-xy-str [cx cy])
                                     [50 50])]
                        [:rotate alpha center])
        ("sc"
         "scale")     (let [[n cx cy] args
                            n      (edn/read-string n)
                            center (if (and cx cy)
                                     (read-xy-str [cx cy])
                                     [50 50])]
                        [:scale center n])

        "reverse"     [:reverse]

        "mirror"      (let [[axis pos] args]
                        [:mirror
                         (some-> axis edn/read-string)
                         (some-> pos  edn/read-string)])

        "to"          [:el-tf (-> args first keyword)]
        "d"           [:toggle-d]

        ;; --- configure
        "p-mirror"    (let [[?mode-str] args]
                        [:set-p-opts [:mirror {:mode (or (some-> ?mode-str keyword) :separate)}]])
        "attr-class"  (let [[?class] args
                            attr-class (or ?class "outline")]
                        [:set-p-opts [:attr-class attr-class]])
        "rm-attrs"    [:set-p-opts [:attrs nil]]
        "variant-key" (let [[?variant] args
                            variant-key (or ?variant "outline")]
                        [:set-p-opts [:variant-key variant-key]])
        "disable"     [:set-p-opts [:disabled? true]]
        "enable"      [:set-p-opts [:disabled? false]]
        "doc"         [:set-p-opts [:doc (first args)]]

        ;; --- nav
        "clear"       [:op.s-log/clear]
        "def"         (let [[pk] args]
                        [:def pk])
        "script"      (let [sel-path (map edn/read-string args)]
                        [:navr-sel (cons :script sel-path)])
        "svg"         [:svg-path (str/join " " args)]
        "snap"        [:toggle-snap]
        ("i"
         "insert")    [:toggle-insert]
        ".png"        [:dl-png]
        ".svg"        [:dl-svg]

        ;; else:
        (println (str "command not found: " cmd-line))))))

(def s-log-ops
  #{:op.s-log/undo
    :op.s-log/activate
    :op.s-log/preview
    :op.s-log/clear
    :op.s-log/clear<
    :op.s-log/clear>})

(defn- handle-op
  [s-log cmpt {:as ui :keys [navr-sel]}
   [op-k & [arg :as args] :as op]]
  (case op-k
    :op/set-cmpt-str   (let [cmpt-edn (->> (edn/read-string arg)
                                           (data/parse-cmpt (-> navr-sel
                                                                data/navr->locr)))]
                         {:cmpt (if (:cmpt-pth navr-sel)
                                  (nav/update-in-nav* cmpt navr-sel :cmpt-pth
                                                      (fn [cmpt-prev]
                                                        cmpt-edn))
                                  cmpt-edn)})
    :op/set-config-str {:conf (edn/read-string arg)}

    :set-sel-d   (let [{:keys [cmpt0]} (:snap ui)
                       cmpt (or cmpt0  ;; dnd
                                cmpt)] ;; kb

                   {:cmpt
                    (reduce (fn [acc' {:as sel-item :keys [nav-rec main?]}]

                              (let [pth-vec (nav/nav-rec->data-vec nav-rec)
                                    v-curr  (get-in cmpt pth-vec)
                                    v-next  (mapv + v-curr arg)]
                                (-> acc'
                                    (assoc-in pth-vec v-next))))
                            cmpt
                            (:sel-set ui))})

    :pth-append  {:cmpt (-> cmpt (update :script ops-cmpt/append-pth (:x-el-k navr-sel)))
                  :ui   (-> ui   (merge {:navr-sel nil :snap nil}))}

    :pth-del     {:cmpt (-> cmpt (update :script ops-cmpt/del-pth (:x-el-k navr-sel)))
                  :ui   (-> ui   (merge {:navr-sel nil :snap nil}))}

    :el-append   (let [el         arg
                       {:keys
                        [p-el-i]} navr-sel
                       init?      (not (seq (:script cmpt)))
                       cmpt'      (cond
                                    init? (-> cmpt (update :script conj
                                                           (data/elemv :path [el])))
                                    el    (-> cmpt (nav/update-in-nav* navr-sel :x-el-k ops-path-tf/append-p-el p-el-i el))
                                    :else (-> cmpt (nav/update-in-nav* navr-sel :x-el-k ops-path-tf/append-p-el p-el-i)))
                       navr-sel'   (-> navr-sel
                                       (update :p-el-i #(some-> % inc)))]
                   {:cmpt cmpt'
                    :ui   (-> ui (assoc :navr-sel navr-sel'))})

    :del-sel     (let [[k1 k2]    (drop-while #(not (get navr-sel %)) nav/kk-rev)
                       i-del      (get navr-sel k1)
                       pth-vec    (nav/nav-rec->data-vec navr-sel)
                       navr-sel'  (-> navr-sel (assoc k1 nil))
                       coll-pth   (-> navr-sel' nav/nav-rec->data-vec)
                       navr-sel'' (-> navr-sel' (update k2 dec))]

                   ;; TODO: when coll is empty delete k2 as well
                   {:cmpt (-> cmpt (update-in coll-pth ops-path-tf/del-el i-del))
                    :ui   (-> ui   (merge {:navr-sel (-> navr-sel )}))})

    :el-tf       (let [to arg]
                   {:cmpt (-> cmpt (nav/update-in-nav* navr-sel :x-el-k
                                                       ops-path-tf/transform-el (:p-el-i navr-sel) to))
                    :ui   (-> ui   (update :navr-sel nav/nav-truncate :p-el-i))})

    :xy-append   {:cmpt (-> cmpt (update-in [:script (:x-el-k navr-sel)]
                                            ops-path-tf/append-pnt (:p-el-i navr-sel)))
                  :ui   (-> ui   (merge {:navr-sel nil :snap nil}))}

    :xy-del      {:cmpt (-> cmpt (update-in [:script (:x-el-k navr-sel)]
                                            ops-path-tf/del-pnt (:p-el-i navr-sel)
                                            (:xy-i navr-sel)))
                  :ui   (-> ui   (merge {:navr-sel nil :snap nil}))}

    :rel->abs    {:cmpt (-> cmpt (ops-cmpt/absolute  navr-sel))}
    :short->full {:cmpt (-> cmpt (ops-cmpt/full      navr-sel))}
    :normalize   {:cmpt (-> cmpt (ops-cmpt/normalize navr-sel))}
    :scale       (let [[ctr k] args]
                   {:cmpt (-> cmpt (ops-cmpt/scale navr-sel ctr k))})
    :mirror      (let [[axis pos] args]
                   {:cmpt (-> cmpt (ops-cmpt/mirror (or axis 0) (or pos 100) navr-sel))})

    :reverse     {:cmpt (-> cmpt (ops-cmpt/reverse-path navr-sel))}

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

    :translate   {:cmpt (-> cmpt (ops-cmpt/translate navr-sel arg))}

    :rotate      (let [[alpha center] args]
                   {:cmpt (-> cmpt (ops-cmpt/rotate navr-sel center alpha))})

    :clear       {:cmpt (-> cmpt (merge cmpt-clear))
                  :ui   (-> ui (merge {:navr-sel nil :snap nil}))}

    :def         (let [[pk] args
                       {:keys [defs]} cmpt]
                   (if-let [els (get defs pk)]
                     ;; --- select
                     (let [eli (-> els count (- 1) (max 0))]
                       {:ui (-> ui (merge {:navr-sel [:defs pk eli]}))})

                     ;; --- create
                     {:cmpt (-> cmpt (#(-> %
                                           (assoc-in [:defs pk] [])
                                           (update :script conj
                                                   (data/elemv :path [(data/elemv :ref [pk])])))))
                      :ui   (-> ui (merge {:navr-sel [:defs pk nil]}))}))

    :svg-path    (let [[svg-path] args]
                   (println :svg-path svg-path)
                   (let [p-new (conv/path-d->path svg-path)]
                     (pprint p-new)
                     {:cmpt (-> cmpt
                                (update :script conj p-new))}))

    :navr-sel     {:ui (-> ui
                           (assoc :navr-sel arg)
                           (cond-> (nil? arg)
                                   (assoc :snap nil)))}

    :nav-into-ref     (handle-op s-log cmpt ui
                                 [:navr-sel (-> navr-sel (nav/nav-into-ref cmpt arg))])

    :sel-rel     (handle-op s-log cmpt ui
                            [:navr-sel
                             (case arg
                               :next (-> navr-sel (nav/nav-next cmpt))
                               :prev (-> navr-sel nav/nav-prev)
                               :up   (-> navr-sel (nav/nav-up :drop-src-k? true))
                               :open (-> navr-sel (nav/nav-into cmpt)))])

    :disable-ref-pth {:ui (-> ui
                              (update :navr-sel #(-> %
                                                     (assoc :cmpt-pth0 (:cmpt-pth %)
                                                            :ref-pth   nil))))}

    :sel-cmpt-id (handle-op s-log cmpt ui
                            (let [cmpt-pth' (-> (:cmpt-pth navr-sel)
                                                (u/conjv arg))]
                              [:navr-sel (nav/nav-rec :cmpt-pth0 cmpt-pth'
                                                      :cmpt-pth  cmpt-pth')]))

    :set-p-opts  (let [[k v] arg]
                   {:cmpt (if v
                            (-> cmpt (ops-cmpt/update-p-opts navr-sel assoc  k v))
                            (-> cmpt (ops-cmpt/update-p-opts navr-sel dissoc k)))})

    :toggle-d    {:cmpt (-> cmpt (ops-cmpt/toggle-d navr-sel))}

    :toggle-snap   {:ui (-> ui (update :snap-to-grid? not))}
    :toggle-insert {:ui (-> ui (update :insert-mode? not))}
    :set-full-svg-scale {:ui (-> ui
                                 (assoc :full-svg-scale arg))}))

(defn- get-wh [cmpt]
  (let [{:keys [dims scale]} (:canvas cmpt)]
    (mapv (partial * scale) dims)))

(defn dispatch! [!s-app
                 ; !config !cmpt !s-log !ui
                 [op-k arg :as op]]
  (case op-k
    :cmd (when-let [op-vec (parse-cmd-line arg)]
           (dispatch! !s-app op-vec))

    (:dl-png
     :dl-svg)   #?(:cljs ((case op-k :dl-png
                            ops-svg/to-png!
                            ops-svg/to-svg)
                          (let [{:keys [conf-ext
                                        cmpt-root]} @!s-app]
                            (get-wh (merge conf-ext
                                           cmpt-root)))
                          (get-in @!s-app [:ui :svg-dom])
                          #(-> % (ops-svg/download!
                                  (str "paintscript"
                                       (case op-k :dl-png ".png" ".svg")))))
                         :clj nil)

    ;; else:
    (let [{:as s-app
           :keys [conf-ext
                  cmpt-root
                  ui
                  s-log]} @!s-app]
      (try
        (let [{cmpt'  :cmpt
               ui'    :ui
               conf'  :conf
               s-log' :s-log} (if (-> op-k s-log-ops)
                                (s-log/handle-op s-log cmpt-root ui op)
                                (handle-op s-log cmpt-root ui op))]
          (do
            (swap! !s-app
                   (fn [s-app]
                     (let [s-app' (-> s-app
                                      (cond-> conf'  (assoc :conf-ext  conf')
                                              cmpt'  (assoc :cmpt-root cmpt')
                                              ui'    (assoc :ui        ui')
                                              s-log' (assoc :s-log     s-log')))]
                       (-> s-app'
                           (cond-> (not (-> op-k s-log-ops))
                                   (update :s-log s-log/add
                                           {:op    op
                                            :s-app (-> s-app'
                                                       (cond-> (= :set-sel-d op-k)
                                                               (update :ui assoc :snap nil)))}))))))))
        (catch #?(:cljs :default :clj Exception) e
          (do
            (println "op failed: " (pr-str op))
            #?(:cljs (js/console.log e))
            (reset! !s-app s-app)))))))

(defn- pth->cp-ii [p-el p-el-i]
  (let [p-el-i' (inc p-el-i)]
    (concat
     (when-let [cp-i (some-> (get-in p-el [:el-argv p-el-i])  (ops-path/el->cp-i :term))] [[p-el-i  cp-i]])
     (when-let [cp-i (some-> (get-in p-el [:el-argv p-el-i']) (ops-path/el->cp-i :init))] [[p-el-i' cp-i]]))))

#?(:cljs
   (defn drag-and-drop-fns
     "attached to SVG element"
     [!cmpt !ui dispatch!]
     (fn _derive-dnd-fns [!s-app !svg-dom {:as canvas :keys [full-screen? scale]}]
       (let [!snap     (r/cursor !ui [:snap])
             !navr-sel (r/cursor !ui [:navr-sel])
             !sel-set  (r/cursor !ui [:sel-set])
             xy-svg!   (fn []
                         (let [rect (-> @!svg-dom (.getBoundingClientRect))
                               xy0  @(r/cursor !s-app [:ui :full-svg-params :canvas-xy0])]
                           (-> [(-> rect .-left)
                                (-> rect .-top)]
                               (cond-> full-screen?
                                       (#(mapv + % xy0))))))]
         {;; NOTE: invoked after canvas/report-down!
          :on-mouse-down #(let [{:keys
                                 [navr-sel
                                  sel-set
                                  insert-mode?]} @!ui
                                xy               (xy-mouse %)
                                {:keys
                                 [main?
                                  shift?]}       (meta navr-sel)]
                            (cond
                              (nil? navr-sel) nil

                              (nav/xy-nav? navr-sel)
                              ;; --- sel
                              (do
                                (let [sel-set'
                                      (into #{{:nav-rec navr-sel
                                               :main?   main?}}
                                            (map (fn [[p-el-i cp-i]]
                                                   (assert (number? cp-i))
                                                   {:nav-rec (-> navr-sel
                                                                 (assoc :p-el-i p-el-i
                                                                        :xy-i   cp-i))}))
                                            (when main?
                                              (let [pth    (nav/get-in-nav @!cmpt navr-sel :x-el-k)
                                                    p-el-i (:p-el-i navr-sel)
                                                    cp-ii  (pth->cp-ii pth p-el-i)]
                                                ;; --- also move CPs
                                                (pth->cp-ii pth p-el-i))))]
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
                                    p-el-k (if (seq (:script @!cmpt))
                                             :L
                                             :M)]
                                (dispatch! [:el-append (data/elemv p-el-k [xy'])]))))

          :on-mouse-move (fn [ev]
                           (let [{:keys [snap-to-grid?]
                                  {:as snapshot :keys [cmpt0 m0]} :snap} @!ui
                                 scale (s-app/derive-scale !s-app canvas)]
                             (when cmpt0
                               (let [m1  (xy-mouse ev)
                                     d   (mapv - m1 m0)
                                     d'  (mapv / d [scale scale])
                                     d'  (if snap-to-grid?
                                           (mapv u/round d')
                                           d')]
                                 (dispatch! [:set-sel-d d'])))))

          :on-mouse-up   #(let [{:keys [navr-sel
                                        sel-prev cmpt0]} @!snap
                                cmpt @!cmpt]
                            (reset! !snap {:sel-prev navr-sel})
                            (when (and navr-sel
                                       sel-prev cmpt0
                                       (= navr-sel
                                          sel-prev)
                                       (= cmpt0 cmpt))
                              (reset! !navr-sel nil)
                              (reset! !snap    nil)))}))))

(defn keybind-fns [!cmpt !ui dispatch!]
  {"left"      #(dispatch! [:set-sel-d [-1 0]])
   "right"     #(dispatch! [:set-sel-d [1 0]])
   "up"        #(dispatch! [:set-sel-d [0 -1]])
   "down"      #(dispatch! [:set-sel-d [0 1]])
   "backspace" #(when (:navr-sel @!ui) (dispatch! [:del-sel]))
   "j"         #(dispatch! [:sel-rel :next])
   "k"         #(dispatch! [:sel-rel :prev])
   "u"         #(dispatch! [:sel-rel :up])
   "o"         #(dispatch! [:sel-rel :open])
   "d"         #(dispatch! [:toggle-d])
   "c"         #(dispatch! [:el-tf :C])
   "q"         #(dispatch! [:el-tf :Q])
   "s"         #(dispatch! [:el-tf :S])
   "l"         #(dispatch! [:el-tf :L])
   "1"         #(dispatch! [:set-full-svg-scale 1])
   "2"         #(dispatch! [:set-full-svg-scale 2])
   "3"         #(dispatch! [:set-full-svg-scale 3])
   "4"         #(dispatch! [:set-full-svg-scale 4])
   "5"         #(dispatch! [:set-full-svg-scale 5])
   "6"         #(dispatch! [:set-full-svg-scale 6])
   "7"         #(dispatch! [:set-full-svg-scale 7])
   "8"         #(dispatch! [:set-full-svg-scale 8])
   "9"         #(dispatch! [:set-full-svg-scale 9])
   "0"         #(dispatch! [:set-full-svg-scale nil])})
