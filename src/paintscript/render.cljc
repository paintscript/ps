(ns paintscript.render
  (:require [paintscript.els :as els]))

(defprotocol Renderer
  (els->out [_ els])
  (group    [_ els]
            [_ opts els])
  (tf       [_ opts el])
  (tf*      [_ opts els])
  (paint    [_ ps])
  (paint*   [_ ps-out container-size el-size]))

(defn path-pnts [cmpt opts els]
  (let [els'    (els/apply-path-opts cmpt opts els)
        el-recs (map els/el-vec-->el-rec els')]
    (els/el-pnts el-recs)))

(defn path
  ([r els]      (path r nil nil  els))
  ([r opts els] (path r nil opts els))
  ([r cmpt opts els]
   (let [els'    (els/apply-path-opts cmpt opts els)
         el-recs (map els/el-vec-->el-rec els')
         out-seq (els->out r el-recs)]
     (if (:debug? cmpt)
       [out-seq (els/el-pnts el-recs)]
       out-seq))))
