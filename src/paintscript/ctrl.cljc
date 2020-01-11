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
        "absolute" [:absolute]
        "round"    (let [[?n] args]
                     [:round ?n])
        "translate" (let [xy (read-xy-str args)]
                      [:translate xy])
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

        ;; else:
        (println (str "command not found: " cmd-line))))))

(defn dispatch! [!params !ui [op-k & [arg :as args]]]
  (let [[src-k-sel
         pi-sel
         eli-sel
         xyi-sel :as sel] (:sel @!ui)]
    (case op-k
      :set-xy     (do
                    (swap! !params assoc-in (:sel @!ui) arg))

      :pth-append (do
                    (swap! !ui merge {:sel nil :snap nil})
                    (swap! !params update :script ops/append-pth pi-sel))

      :pth-del    (do
                    (swap! !ui merge {:sel nil :snap nil})
                    (swap! !params update :script ops/del-pth pi-sel))

      :el-append  (let [[src-k px eli] (or sel (ops/tail-iii @!params))
                        eli'           (or (some-> eli inc) 0)]
                    (if-let [el arg]
                      (swap! !params update-in [src-k px] ops/append-el eli el)
                      (swap! !params update-in [src-k px] ops/append-el eli))
                    (swap! !ui merge {:sel [src-k px eli']}))

      :el-del     (let [eli' (max (dec eli-sel) 0)]
                    (swap! !ui merge {:sel [src-k-sel pi-sel eli']})
                    (swap! !params update-in [src-k-sel pi-sel] ops/del-el eli-sel))

      :el-tf      (let [to arg]
                    (swap! !params update-in [src-k-sel pi-sel]
                           ops/transform-el eli-sel to))

      :xy-append  (do
                    (swap! !ui merge {:sel nil :snap nil})
                    (swap! !params update-in [:script pi-sel]
                           ops/append-pnt eli-sel))

      :xy-del     (do
                    (swap! !ui merge {:sel nil :snap nil})
                    (swap! !params update-in [:script pi-sel]
                           ops/del-pnt eli-sel
                           (- xyi-sel nav/xyi0)))

      :cmd        (when-let [op-vec (parse-cmd-line arg)]
                    (dispatch! !params !ui op-vec))

      :absolute   (if sel
                    (swap! !params ops/absolute [src-k-sel pi-sel])
                    (swap! !params ops/absolute))

      :round      (let [n arg]
                    (swap! !params
                           (fn [s]
                             (w/prewalk
                              (case n
                                "1" #(-> % (cond-> (number? %) u/round1))
                                "2" #(-> % (cond-> (number? %) u/round2))
                                #(-> % (cond-> (number? %) u/round)))
                              s))))

      :translate  (swap! !params ops/tl-pth sel arg)

      :clear      (do
                    (swap! !ui merge {:sel nil :snap nil})
                    (swap! !params merge params-init))

      :def        (let [[pk] args
                        {:keys [defs]} @!params]
                    (if-let [els (get defs pk)]
                      ;; select
                      (let [eli (-> els count (- 1) (max 0))]
                        (swap! !ui merge {:sel [:defs pk eli]}))
                      ;; create
                      (do
                        (swap! !params
                               #(-> %
                                    (assoc-in [:defs pk] [])
                                    (update :script conj [:path {} [:ref pk]])))
                        (swap! !ui merge {:sel [:defs pk nil]}))))

      :sel        (do
                    (swap! !ui merge {:sel arg})
                    (when (nil? arg)
                      (swap! !ui assoc :snap nil)))

      :set-p-opts (let [[k v] arg]
                    (swap! !params ops/update-p-opts sel assoc k v)))))

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
