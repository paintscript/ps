(ns paintscript.render
  (:require [paintscript.els :as els]))

(defprotocol Renderer
  (p-els->out [_ els])
  (group    [_ els]
            [_ opts els])
  (tf       [_ opts el])
  (tf*      [_ opts els])
  (paint    [_ ps])
  (paint*   [_ ps-out container-size el-size]))

(defn path-pnts [cmpt opts p-els]
  (let [p-els'    (els/apply-path-opts cmpt opts p-els)
        p-el-recs (map els/el-vec-->el-rec p-els')]
    (els/p-el->pnts p-el-recs)))

(defn path
  ([r p-els]        (path r nil nil    p-els))
  ([r p-opts p-els] (path r nil p-opts p-els))
  ([r cmpt p-opts p-els]
   (let [p-els'    (els/apply-path-opts cmpt p-opts p-els)
         p-el-recs (map els/el-vec-->el-rec p-els')
         out-seq   (p-els->out r p-el-recs)]
     (if (:debug? cmpt)
       [out-seq (els/p-el->pnts p-el-recs)]
       out-seq))))
