(ns paintscript.render
  (:require [paintscript.els :as els]))

(defprotocol Renderer
  (els->out [_ els]))

(defn path-pnts [params opts els]
  (let [els'    (els/apply-path-opts params opts els)
        el-recs (map els/el-vec-->el-rec els')]
    (els/el-pnts el-recs)))

(defn path
  ([r els]      (path r nil nil  els))
  ([r opts els] (path r nil opts els))
  ([r params opts els]
   (let [els'    (els/apply-path-opts params opts els)
         el-recs (map els/el-vec-->el-rec els')
         out-seq (els->out r el-recs)]
     (if (:debug? params)
       [out-seq (els/el-pnts el-recs)]
       out-seq))))