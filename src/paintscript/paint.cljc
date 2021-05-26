(ns paintscript.paint
  (:require [svg-hiccup-kit.core :as shk :refer [d tf tf*]]

            [paintscript.util :as u]
            [paintscript.el-path :as el-path]
            [paintscript.els :as els]

            [paintscript.render :as render]))

(declare paint)

(def svg-renderer
  (reify render/Renderer
    (p-els->out [_ els]      (el-path/p-els->out els))
    (group      [_ els]      (vec (cons :g els)))
    (group      [_ opts els] (vec (concat [:g opts] els)))
    (tf         [_ opts el]  (shk/tf opts el))
    (tf*        [_ opts els] (apply shk/tf* opts els))
    (paint      [_ ps]       (paint ps))
    (paint*     [_ ps-out _ _] ps-out)))

(defn path-builder
  ([opts els]       (path-builder nil   nil opts 0 els))
  ([c-fns opts els] (path-builder c-fns nil opts 0 els))
  ([c-fns
    {:as cmpt :keys [debug?]}
    {:as p-opts :keys [d attrs]}
    pi
    els]
   (cond
     d      (let [d' (-> d (cond-> (el-path/ref? d)
                                   (->> (els/resolve-d-ref (:defs cmpt)))))
                  {:keys [translate]
                   {scale-factor :factor} :scale} p-opts

                  pth [:path (merge attrs {:d d'})]]
              (if (or translate scale-factor)
                [tf {:tl translate
                     :sc (some-> scale-factor vector)} pth]
                pth))

     debug? (let [pnts-seq (render/path-pnts cmpt p-opts els)]
              ((:plot-coords c-fns) p-opts pi els pnts-seq))

     :else  (let [out-seq (render/path svg-renderer cmpt p-opts els)]
              [:path (merge attrs {:d (apply shk/d out-seq)})]))))

(defn- scale->tl
  [canvas-dims {:as scale :keys [factor center] :or {center [0 0]}}]
  (let [ratio      (mapv / center canvas-dims)
        dims-delta (mapv #(-> % (* (- factor 1))) canvas-dims)
        tl         (mapv #(-> %1 (* %2) -) dims-delta ratio)]
    tl))

(defn- margin-side [margin side-k]
  (get margin (case side-k
                :left  (case (count margin)
                         2 1
                         4 3)
                :right 1)))

(defn- derive-cmpt-tf [{:as tf-params
                        :keys [margin translate]
                        {:as scale
                         sc-ctr :center
                         sc-fct :factor} :scale} cmpt]
  {:tl (-> translate
           (cond->  margin (update 0 + (margin-side margin :left)))
           (cond->> scale  (mapv + (scale->tl (get-in cmpt [:canvas :dims])
                                              scale))))
   :sc (when scale
         [sc-fct])})

(defn- layout-builder
  "compose a sequence of components into one, offset each by the width of prior ones"
  [c-fns {:as cmpt-ctx :keys [defs]}
   obj-opts' els]
  [:g (->> els
           (reduce (fn [[out offset] el]
                     (assert (el-path/ref? el))
                     (let [{:as cmpt-ref
                            {:keys [dims]} :canvas}  (els/resolve-cmpt-ref defs el)

                           cmpt* (merge cmpt-ctx
                                        cmpt-ref)

                           {:as tf-params
                            :keys [margin translate]

                            {:as scale
                             sc-ctr :center
                             sc-fct :factor} :scale} (els/get-opts el)

                           tf-params' (-> tf-params
                                          (update :translate #(->> (or % [0 0])
                                                                   (mapv + [offset 0]))))

                           out+
                           ^{:key (hash [offset el])}
                           [tf (derive-cmpt-tf tf-params' cmpt*)
                            (paint c-fns cmpt*)]

                           offset+ (-> (first dims)
                                       (cond-> translate (+ (first translate))
                                               scale     (* sc-fct)
                                               margin    (+ (margin-side margin :left)
                                                            (margin-side margin :right))))]
                       [(conj out out+)
                        (+ offset offset+)]))
                   [nil 0])
           first)])

(defn- derive-attrs
  [cmpt {:as obj-opts
         :keys [attr-class]}]
  (u/deep-merge (:attrs cmpt) ;; includes inherited via configs
                (get (:attr-classes cmpt)
                     (:attr-class   obj-opts))
                (:attrs obj-opts)))

(defn- add-tf-params [params0 params1]
  (-> params0
      (merge params1)
      (cond-> (and (:tl params0)
                   (:tl params1)) (assoc :tl (mapv +
                                                   (:tl params0)
                                                   (:tl params1)))
              (and (:rt params0)
                   (:rt params1)) (assoc :rt (+ (:rt params0)
                                                (:rt params1)))
              (and (:sc params0)
                   (:sc params1)) (assoc :sc (* (:sc params0)
                                                (:sc params1))))))

(defn tf-chain
  [component param-chain]
  (let [[_ cc] (reduce (fn [[params0 cc] {:as params :keys [reference]
                                          :or {reference :prev}}]
                         (case reference
                           :prev (let [params* (add-tf-params params0
                                                              params)]
                                   [params*
                                    (conj cc [tf params* component])])
                           :none [params
                                  (conj cc [tf params component])]))
                       [nil []]
                       param-chain)]
    (vec (cons :g cc))))

(defn- derive-obj-hiccup
  [c-fns
   {:as cmpt :keys [defs script data?]}
   obj-i [obj-k obj-opts & els :as obj]]
  (let [attrs      (derive-attrs cmpt obj-opts)
        obj-opts'  (-> obj-opts
                       (dissoc :attr-class
                               :variant-key
                               :disabled?))

        obj-opts'  (case obj-k
                     (:path
                      :layout) (-> obj-opts'
                                   (assoc  :attrs attrs)
                                   (dissoc :disabled?))
                     :ref      obj-opts'
                     (-> obj-opts'
                         (->> (merge attrs))))]
    (case obj-k
      :path   (path-builder   c-fns cmpt obj-opts' obj-i els)
      :layout (layout-builder c-fns cmpt obj-opts' els)
      :ref    (let [cmpt-ref    (els/resolve-cmpt-ref (:defs cmpt) obj)
                    cmpt*       (u/deep-merge cmpt
                                              cmpt-ref)
                    cmpt-hiccup (paint c-fns cmpt*)
                    opts        (els/get-opts obj)]
                (if-let [{:keys [tfs+]} (:repeat opts)]
                  (cond
                    ;; NOTE: uses svg-hiccup-kit's (different) tf params
                    tfs+ (tf-chain cmpt-hiccup tfs+))
                  [tf (derive-cmpt-tf opts cmpt*)
                   cmpt-hiccup]))

      (:rect
       :circle
       :ellipse) (-> obj (assoc 1 obj-opts')))))

(defn paint
  ([cmpt] (paint nil cmpt))
  ([c-fns cmpt]
   [:g
    (for [[obj-i
           [obj-k
            obj-opts
            :as obj]] (->> (:script cmpt)
                           (map-indexed vector))
          :when       (and (not (:disabled? obj-opts))
                           (or (not (:variant-active cmpt))
                               (not (:variant-key    obj-opts))
                               (= (:variant-active cmpt)
                                  (:variant-key    obj-opts))))]
      ^{:key obj-i}
      [derive-obj-hiccup c-fns cmpt obj-i obj])]))
