(ns paintscript.paint
  (:require [svg-hiccup-kit.core :as sk :refer [d tf tf*]]
            [svg-hiccup-kit.pattern :as sk-pat]

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
    (tf         [_   tfs el]  (sk/tf   tfs el))
    (tf         [_ c tfs el]  (sk/tf c tfs el))
    (tf*        [_   tfs els] (apply sk/tf*   tfs els))
    (tf*        [_ c tfs els] (apply sk/tf* c tfs els))
    (paint      [_ ps]       (paint ps))
    (paint*     [_ ps-out _ _] ps-out)))

(defn path-str [cmpt p-opts p-els]
  (->> (render/path svg-renderer cmpt p-opts p-els)
       (apply sk/d)))

(defn path-builder
  ([opts p-els]       (path-builder nil   nil opts 0 p-els))
  ([c-fns opts p-els] (path-builder c-fns nil opts 0 p-els))
  ([c-fns
    {:as cmpt :keys [interactive?]}
    {:as p-opts :keys [d attrs]}
    pi
    p-els]
   (cond
     d            (let [d' (-> d (cond-> (el-path/ref? d)
                                         (->> (els/resolve-d-ref (:defs cmpt)))))
                        {:keys [translate]
                         {scale-factor :factor} :scale} p-opts

                        pth [:path (merge attrs {:d d'})]]
                    (if (or translate scale-factor)
                      [tf {:tl translate
                           :sc (some-> scale-factor vector)} pth]
                      pth))

     interactive? (let [pnts-seq (render/path-pnts cmpt p-opts p-els)]
                    ((:plot-coords c-fns) p-opts pi p-els pnts-seq))

     :else        [:path (merge attrs {:d (path-str cmpt p-opts p-els)})])))

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
   s-el-opts' els]
  {:pre [(every? el-path/ref? els)]}
  [:g (->> els
           (reduce (fn [[out offset] el]
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
  [cmpt {:as s-el-opts
         :keys [attr-class]}]
  (u/deep-merge (:attrs cmpt) ;; includes inherited via configs
                (get (:attr-classes cmpt)
                     (:attr-class   s-el-opts))
                (:attrs s-el-opts)))

(defn normalize-tf-params [{:keys [translate scale rotate]}]
  (-> {}
      (cond-> translate (assoc :tl translate)
              scale     (assoc :sc (-> scale
                                       (cond-> (map? scale)
                                               ;; TODO: incorporate :center
                                               :factor)))
              rotate    (assoc :rt (-> rotate
                                       (cond-> (map? scale)
                                               ;; TODO: incorporate :center
                                               :degree))))))

(defn- derive-s-el-hiccup
  [c-fns
   {:as cmpt :keys [defs script data?]}
   s-el-i [s-el-k
           s-el-opts & x-els :as s-el]]
  (let [attrs      (derive-attrs cmpt s-el-opts)
        s-el-opts' (-> s-el-opts
                       (dissoc :attr-class
                               :variant-key
                               :disabled?))

        s-el-opts' (case s-el-k
                     (:path
                      :layout) (-> s-el-opts'
                                   (assoc  :attrs attrs)
                                   (dissoc :disabled?))
                     :ref      s-el-opts'
                     (-> s-el-opts'
                         (->> (merge attrs))))]
    (case s-el-k
      :path    (path-builder    c-fns cmpt s-el-opts' s-el-i x-els)
      :layout  (layout-builder  c-fns cmpt s-el-opts' x-els)
      ; :pattern (pattern-builder c-fns cmpt s-el-opts' x-els)
      :ref     (if-let [cmpt-ref (els/resolve-cmpt-ref (:defs cmpt) s-el)]
                 (let [cmpt*       (u/deep-merge cmpt
                                                 cmpt-ref)
                       cmpt-hiccup (paint c-fns cmpt*)
                       opts        (els/get-opts s-el)]
                   (if-let [{:keys [tfs+]} (:repeat opts)]
                     (cond
                       ;; NOTE: uses svg-hiccup-kit's (different) tf params
                       tfs+ (sk-pat/tf-cascade cmpt-hiccup tfs+))
                     [tf (derive-cmpt-tf opts cmpt*)
                      cmpt-hiccup]))
                 (println "error: referenced component not found" (pr-str s-el)))

      (:rect
       :circle
       :ellipse) (-> s-el (assoc 1 s-el-opts')))))

(defn paint
  ([cmpt] (paint nil cmpt))
  ([c-fns cmpt]
   [:g
    (for [[s-el-i
           [s-el-k
            s-el-opts
            :as s-el]] (->> (:script cmpt)
                            (map-indexed vector))

          :when (and (not (:disabled? s-el-opts))
                     (or (not (:variant-active cmpt))
                         (not (:variant-key    s-el-opts))
                         (= (:variant-active cmpt)
                            (:variant-key    s-el-opts))))]
      ^{:key s-el-i}
      [derive-s-el-hiccup c-fns cmpt s-el-i s-el])]))
