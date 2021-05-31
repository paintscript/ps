(ns paintscript.render
  (:require [paintscript.data-ops :as data-ops]))

(defprotocol Renderer
  (p-els->out [_ els])
  (group      [_ els]
              [_ opts els])
  (tf         [_ tf-params el]
              [_ config tf-params el])
  (tf*        [_ tf-params els]
              [_ config tf-params els])
  (paint      [_ ps])
  (paint*     [_ ps-out container-size el-size]))

(defn path-pnts [cmpt opts p-els]
  (let [p-els'    (data-ops/apply-path-opts cmpt opts p-els)
        ; p-el-recs (map data-ops/el-vec-->el-rec p-els')
        ]
    (data-ops/p-el->pnts p-els')))

(defn path
  ([r p-els]        (path r nil nil    p-els))
  ([r p-opts p-els] (path r nil p-opts p-els))
  ([r cmpt p-opts p-els & {:keys [source?]}]
   ;; TODO: does this receive source data or recs?
   (let [p-els'    (data-ops/apply-path-opts cmpt p-opts p-els)
         ; p-el-recs (map data-ops/el-vec-->el-rec p-els')
         out-seq   (p-els->out r p-els')]
     (if (:interactive? cmpt)
       [out-seq (data-ops/p-el->pnts p-els')]
       out-seq))))
