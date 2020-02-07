(ns paintscript.ctrl
  (:require #?(:cljs [reagent.core :as r :refer [atom]])
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.string :as str]
            [clojure.walk :as w]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]
            [paintscript.el :as el]
            [paintscript.ops :as ops]
            [paintscript.nav :as nav]))

(def params-init
  {:defs {}
   :script [[:path {:variant-k "outline" :class-k "outline"}
             [:M [50 50]]]]})

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
        "undo"     [:undo]
        "absolute" [:absolute]
        "round"    (let [[?n] args]
                     [:round ?n])
        "translate" (let [xy (read-xy-str args)]
                      [:translate xy])
        "scale"     (let [ctr (read-xy-str (take 2 args))
                          n   (read-string (last args))]
                      [:scale ctr n])
        "to"        [:el-tf (-> args first keyword)]
        "clear"     [:clear]
        "def"       (let [[pk] args]
                      [:def pk])
        "script"    (let [sel-path (map read-string args)]
                      [:sel (cons :script sel-path)])
        "mirror"    (let [[?mode-str] args]
                      [:set-p-opts [:mirror (or (some-> ?mode-str keyword) :separate)]])
        "class-k"   (let [[?class] args
                          class-k (or ?class "outline")]
                      [:set-p-opts [:class-k class-k]])
        "variant-k" (let [[?variant] args
                          variant-k (or ?variant "outline")]
                      [:set-p-opts [:variant-k variant-k]])
        "disable"   [:set-p-opts [:disabled? true]]
        "enable"    [:set-p-opts [:disabled? false]]

        ;; else:
        (println (str "command not found: " cmd-line))))))

(defn- handle-op [params ui [op-k & [arg :as args] :as op]]
  (let [[src-k-sel
         pi-sel
         eli-sel
         xyi-sel :as sel] (:sel ui)]
    (case op-k
      :set-xy     {:params (-> params (assoc-in (:sel ui) arg))}

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

      ; :cmd        (when-let [op-vec (parse-cmd-line arg)]
      ;               (handle-op params ui op-vec))

      :absolute   {:params (if sel
                             (-> params (ops/absolute [src-k-sel pi-sel]))
                             (-> params (ops/absolute)))}

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

      :translate  {:params (-> params (ops/tl-pth sel arg))}

      :scale      (let [[ctr k] args]
                    {:params (-> params (ops/sc-pth sel ctr k))})

      :clear      {:params (-> params (merge params-init))
                   :ui     (-> ui (merge {:sel nil :snap nil}))}

      :def        (let [[pk] args
                        {:keys [defs]} params]
                    (if-let [els (get defs pk)]
                      ;; select
                      (let [eli (-> els count (- 1) (max 0))]
                        {:ui (-> ui (merge {:sel [:defs pk eli]}))})
                      ;; create
                      {:params (-> params (#(-> %
                                                (assoc-in [:defs pk] [])
                                                (update :script conj [:path {} [:ref pk]]))))
                       :ui     (-> ui (merge {:sel [:defs pk nil]}))}))

      :sel        {:ui (-> ui
                           (merge {:sel arg})
                           (cond-> (nil? arg)
                                   (assoc :snap nil)))}

      :set-p-opts (let [[k v] arg]
                    {:params (-> params (ops/update-p-opts sel assoc k v))}))))

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

#?(:cljs
 (defn drag-and-drop-fns [!scale !params !ui dispatch!]
   (let [!snap  (r/cursor !ui [:snap])
         !sel   (r/cursor !ui [:sel])
         get!   #(get-in @!params @!sel)]
     {:on-mouse-down #(let [{:keys [xy-svg sel]} @!ui
                            xy  (xy-mouse %)
                            scale @!scale]
                        (if (= 4 (count sel))
                          (swap! !snap merge
                                 {:sel sel :xy0 (get!) :m0 xy})
                          ;; insert
                          (let [xy  (xy-mouse %)
                                xy' (as-> (xy-mouse %) xy'
                                          (mapv - xy' xy-svg)
                                          (mapv / xy' [scale scale])
                                          (mapv u/round xy'))]
                            (dispatch! [:el-append [:L xy']]))))
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
                          (reset! !snap nil)))})))

(defn keybind-fns [!params !ui dispatch!]
  (let [upd! #(swap! !params assoc-in (:sel @!ui) %)
        get! #(get-in @!params @!ui)]
    {"left"      #(when-let [[x y] (get!)] (dispatch! [:set-xy [(- x 1) y]]))
     "right"     #(when-let [[x y] (get!)] (dispatch! [:set-xy [(+ x 1) y]]))
     "up"        #(when-let [[x y] (get!)] (dispatch! [:set-xy [x (- y 1)]]))
     "down"      #(when-let [[x y] (get!)] (dispatch! [:set-xy [x (+ y 1)]]))
     "backspace" #(when-let [[x y] (get!)] (dispatch! [:xy-del]))}))