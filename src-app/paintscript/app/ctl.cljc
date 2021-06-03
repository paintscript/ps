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
        [:op/el-append el])
      (case cmd-str
        "undo"        [:op.s-log/undo]
        "log-clear"   [:op.s-log/clear]
        "log-clear<"  [:op.s-log/clear<]
        "log-clear>"  [:op.s-log/clear>]

        ;; --- mutate
        ("abs"
         "absolute")  [:op/rel->abs]
        "full"        [:op/short->full]
        ("norm"
         "normalize") [:op/normalize]
        "round"       (let [[?n] args]
                        [:op/round ?n])
        ("tl"
         "translate") (let [xy (read-xy-str args)]
                        [:op/translate xy])
        ("rt"
         "rotate")    (let [[a cx cy] args
                            alpha  (edn/read-string a)
                            center (if (and cx cy)
                                     (read-xy-str [cx cy])
                                     [50 50])]
                        [:op/rotate alpha center])
        ("sc"
         "scale")     (let [[n cx cy] args
                            n      (edn/read-string n)
                            center (when (and cx cy)
                                     (read-xy-str [cx cy]))]
                        [:op/scale center n])

        "reverse"     [:op/reverse]

        "mirror"      (let [[axis pos] args]
                        [:op/mirror
                         (some-> axis edn/read-string)
                         (some-> pos  edn/read-string)])

        "to"          [:op/el-tf (-> args first keyword)]
        "d"           [:op/toggle-d]

        ;; --- configure
        "p-mirror"    (let [[?mode-str] args]
                        [:op/set-p-opts [:mirror {:mode (or (some-> ?mode-str keyword) :separate)}]])
        "attr-class"  (let [[?class] args
                            attr-class (or ?class "outline")]
                        [:op/set-p-opts [:attr-class attr-class]])
        "rm-attrs"    [:op/set-p-opts [:attrs nil]]
        "variant-key" (let [[?variant] args
                            variant-key (or ?variant "outline")]
                        [:op/set-p-opts [:variant-key variant-key]])
        "disable"     [:op/set-p-opts [:disabled? true]]
        "enable"      [:op/set-p-opts [:disabled? false]]
        "doc"         [:op/set-p-opts [:doc (first args)]]

        ;; --- nav
        "clear"       [:op/clear]
        "def"         (let [[pk] args]
                        [:op/def pk])
        "script"      (let [sel-path (map edn/read-string args)]
                        [:op/navr-sel (cons :script sel-path)])
        "svg"         [:op/svg-path (str/join " " args)]
        "snap"        [:op/toggle-snap]
        ("i"
         "insert")    [:op/toggle-insert]
        ".png"        [:op/dl-png]
        ".svg"        [:op/dl-svg]

        ;; else:
        (println (str "command not found: " cmd-line))))))

(defn- handle-op
  [s-log cmpt {:as ui :keys [navr-sel]}
   [op-k & [arg :as args] :as op]]
  (case op-k
    :op/set-cmpt-str    (let [cmpt-edn (->> (edn/read-string arg)
                                            (data/parse-cmpt (-> navr-sel
                                                                 nav/navr->locr)))]
                          {:cmpt (if (:cmpt-pth navr-sel)
                                   (nav/update-in-nav* cmpt navr-sel
                                                       {:trunc-k :cmpt-pth}
                                                       (fn [cmpt-prev]
                                                         cmpt-edn))
                                   cmpt-edn)})
    :op/set-conf-str    {:conf (edn/read-string arg)}

    :op/set-sel-d       (let [{:keys [cmpt0]} (:snap ui)
                              cmpt (or cmpt0  ;; dnd
                                       cmpt)] ;; kb

                          {:cmpt
                           (reduce (fn [acc' {:as sel-item :keys [nav-rec main?]}]

                                     (let [pth-vec (nav/nav-rec->data-pth nav-rec)
                                           v-curr  (get-in cmpt pth-vec)
                                           v-next  (mapv + v-curr arg)]
                                       (-> acc'
                                           (assoc-in pth-vec v-next))))
                                   cmpt
                                   (:sel-set ui))})

    :op/pth-append      {:cmpt (-> cmpt (update :script ops-cmpt/append-pth (:x-el-k navr-sel)))
                         :ui   (-> ui   (assoc :navr-sel nil :snap nil))}

    :op/pth-del         {:cmpt (-> cmpt (update :script ops-cmpt/del-pth (:x-el-k navr-sel)))
                         :ui   (-> ui   (assoc :navr-sel nil :snap nil))}

    :op/el-append       (let [el         (-> arg
                                             (cond-> (vector? arg)
                                                     data/elvv->rr))
                              [navr-coll
                               el-i]     (cond
                                           (-> (nav/get-in-nav cmpt navr-sel) :el-k (= :path))
                                           [navr-sel nil]

                                           (-> (nav/get-in-nav cmpt (-> navr-sel
                                                                        nav/nav-up)) :el-k (= :path))
                                           [(-> navr-sel nav/nav-up)
                                            (:p-el-i navr-sel)])]
                          {:cmpt (-> cmpt
                                     (nav/update-in-nav* navr-coll {:refresh-locr? true}
                                                         ops-path-tf/insert-el :el-i el-i :el el))})

    :op/svg-path        (let [[svg-path] args]
                          (println :op/svg-path svg-path)
                          (let [p-new (-> svg-path
                                          conv/path-d->path
                                          data/elvv->rr)
                                navr  (or navr-sel
                                          (nav/nav-rec :src-k :script))]
                            ; (pprint p-new)
                            {:cmpt (-> cmpt
                                       (nav/update-in-nav* navr {:refresh-locr? true}
                                                           ops-path-tf/insert-el
                                                           :el p-new))}))

    :op/del-sel         (let [k-el      (-> navr-sel nav/nav-head-k)
                              i-del     (-> navr-sel (get   k-el))
                              navr-sel' (-> navr-sel (assoc k-el nil))]

                          ;; TODO: when coll is empty delete k2 as well
                          {:cmpt (-> cmpt (nav/update-in-nav navr-sel' ops-path-tf/del-el i-del))
                           :ui   (-> ui   (assoc :navr-sel navr-sel'))})

    :op/el-tf           (let [to arg]
                          {:cmpt (-> cmpt (nav/update-in-nav* navr-sel {:trunc-k :x-el-k}
                                                              ops-path-tf/change-pcmd-k
                                                              (:p-el-i navr-sel) to))
                           :ui   (-> ui   (update :navr-sel nav/nav-truncate :p-el-i))})

    :op/xy-append       {:cmpt (-> cmpt (update-in [:script (:x-el-k navr-sel)]
                                                   ops-path-tf/append-pnt (:p-el-i navr-sel)))
                         :ui   (-> ui   (merge {:navr-sel nil :snap nil}))}

    :op/xy-del          {:cmpt (-> cmpt (update-in [:script (:x-el-k navr-sel)]
                                                   ops-path-tf/del-pnt (:p-el-i navr-sel)
                                                   (:xy-i navr-sel)))
                         :ui   (-> ui   (merge {:navr-sel nil :snap nil}))}

    :op/rel->abs        {:cmpt (-> cmpt (ops-cmpt/absolute  navr-sel))}
    :op/short->full     {:cmpt (-> cmpt (ops-cmpt/full      navr-sel))}
    :op/normalize       {:cmpt (-> cmpt (ops-cmpt/normalize navr-sel))}
    :op/scale           (let [[ctr k] args]
                          {:cmpt (-> cmpt (ops-cmpt/scale navr-sel ctr k))})
    :op/mirror          (let [[axis pos] args]
                          {:cmpt (-> cmpt (ops-cmpt/mirror (or axis 0) (or pos 100) navr-sel))})

    :op/reverse         {:cmpt (-> cmpt (ops-cmpt/reverse-path navr-sel))}

    :op/round           (let [n arg]
                          {:cmpt
                           (-> cmpt
                               ((fn [s]
                                  (w/prewalk
                                   (case n
                                     "1" #(-> % (cond-> (number? %) u/round1))
                                     "2" #(-> % (cond-> (number? %) u/round2))
                                     #(-> % (cond-> (number? %) u/round)))
                                   s))))})

    :op/translate       {:cmpt (-> cmpt (ops-cmpt/translate navr-sel arg))}

    :op/rotate          (let [[alpha center] args]
                          {:cmpt (-> cmpt (ops-cmpt/rotate navr-sel center alpha))})

    :op/clear           {:cmpt (-> cmpt (merge cmpt-clear))
                         :ui   (-> ui (assoc :navr-sel nil :snap nil))}

    :op/def             (let [[pk] args
                              {:keys [defs]} cmpt]
                          (if-let [els (get defs pk)]
                            ;; --- select
                            (let [eli (-> els count (- 1) (max 0))]
                              {:ui (-> ui (merge {:navr-sel [:defs pk eli]}))})

                            ;; --- create
                            (let [el (data/elemv :path [(data/elemv :ref [pk])])]
                              {:cmpt (-> cmpt (#(-> %
                                                    (assoc-in [:defs pk] [])
                                                    (update :script conj el))))
                               :ui   (-> ui (assoc :navr-sel [:defs pk nil]))})))

    :op/navr-sel        {:ui (-> ui
                                 (assoc :navr-sel arg)
                                 (cond-> (nil? arg)
                                         (assoc :snap nil)))}

    :op/nav-into-ref    (handle-op s-log cmpt ui
                                   [:op/navr-sel (-> navr-sel (nav/nav-into-ref cmpt arg))])

    :op/sel-rel         (handle-op s-log cmpt ui
                                   [:op/navr-sel
                                    (case arg
                                      :next (-> navr-sel (nav/nav-next cmpt))
                                      :prev (-> navr-sel nav/nav-prev)
                                      :up   (-> navr-sel (nav/nav-up :drop-src-k? true))
                                      :open (-> navr-sel (nav/nav-into cmpt)))])

    :op/disable-ref-pth {:ui (-> ui
                                 (update :navr-sel #(-> %
                                                        (assoc :cmpt-pth0 (:cmpt-pth %)
                                                               :ref-pth   nil))))}

    :op/sel-cmpt-id     (handle-op s-log cmpt ui
                                   (let [cmpt-pth' (-> (:cmpt-pth navr-sel)
                                                       (u/conjv arg))]
                                     [:op/navr-sel (nav/nav-rec :cmpt-pth0 cmpt-pth'
                                                                :cmpt-pth  cmpt-pth')]))

    :op/set-p-opts      (let [[k v] arg]
                          {:cmpt (if v
                                   (-> cmpt (ops-cmpt/update-el navr-sel update :el-opts assoc  k v))
                                   (-> cmpt (ops-cmpt/update-el navr-sel update :el-opts dissoc k)))})

    :op/toggle-d        {:cmpt (-> cmpt (ops-cmpt/toggle-d navr-sel))}

    :op/toggle-snap     {:ui (-> ui (update :snap-to-grid? not))}
    :op/toggle-insert   {:ui (-> ui (update :insert-mode?  not))}
    :op/full-svg-scale  {:ui (-> ui (assoc  :full-svg-scale arg))}))

(defn- get-wh [cmpt]
  (let [{:keys [dims scale]} (:canvas cmpt)]
    (mapv (partial * scale) dims)))

(defn dispatch! [!s-app
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
               s-log' :s-log} (if (-> op-k s-log/s-log-ops)
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
                           (cond-> (not (-> op-k s-log/s-log-ops))
                                   (update :s-log s-log/add
                                           {:op       op
                                            :navr-sel (:navr-sel ui)
                                            :s-app    (-> s-app'
                                                          (cond-> (= :op/set-sel-d op-k)
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
                                (dispatch! [:op/el-append (data/elemv p-el-k [xy'])]))))

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
                                 (dispatch! [:op/set-sel-d d'])))))

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
  {"left"      #(dispatch! [:op/set-sel-d [-1 0]])
   "right"     #(dispatch! [:op/set-sel-d [1 0]])
   "up"        #(dispatch! [:op/set-sel-d [0 -1]])
   "down"      #(dispatch! [:op/set-sel-d [0 1]])
   "backspace" #(when (:navr-sel @!ui)
                  (dispatch! [:op/del-sel]))
   "j"         #(dispatch! [:op/sel-rel :next])
   "k"         #(dispatch! [:op/sel-rel :prev])
   "u"         #(dispatch! [:op/sel-rel :up])
   "o"         #(dispatch! [:op/sel-rel :open])
   "d"         #(dispatch! [:op/toggle-d])
   "c"         #(dispatch! [:op/el-tf :C])
   "q"         #(dispatch! [:op/el-tf :Q])
   "s"         #(dispatch! [:op/el-tf :S])
   "l"         #(dispatch! [:op/el-tf :L])
   "1"         #(dispatch! [:op/full-svg-scale 1])
   "2"         #(dispatch! [:op/full-svg-scale 2])
   "3"         #(dispatch! [:op/full-svg-scale 3])
   "4"         #(dispatch! [:op/full-svg-scale 4])
   "5"         #(dispatch! [:op/full-svg-scale 5])
   "6"         #(dispatch! [:op/full-svg-scale 6])
   "7"         #(dispatch! [:op/full-svg-scale 7])
   "8"         #(dispatch! [:op/full-svg-scale 8])
   "9"         #(dispatch! [:op/full-svg-scale 9])
   "0"         #(dispatch! [:op/full-svg-scale nil])})
