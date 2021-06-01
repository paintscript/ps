(ns paintscript.render
  (:require [paintscript.ops.ops-path-tf :as ops-path-tf]))

(defprotocol Renderer
  (p-els->out [_ els])
  (group      [_ els]
              [_ opts els])
  (tf         [_     tf-params el]
              [_ cfg tf-params el])
  (tf*        [_     tf-params els]
              [_ cfg tf-params els])
  (paint      [_ ps])
  (paint*     [_ ps-out container-size el-size]))

(defn path-pnts [cmpt opts p-els]
  (let [p-els' (ops-path-tf/apply-path-opts cmpt opts p-els)]
    (ops-path-tf/p-el->pnts p-els')))

(defn path
  ([r p-els]        (path r nil nil    p-els))
  ([r p-opts p-els] (path r nil p-opts p-els))
  ([r cmpt p-opts p-els & {:keys [source?]}]
   (let [p-els'    (ops-path-tf/apply-path-opts cmpt p-opts p-els)
         out-seq   (p-els->out r p-els')]
     (if (:interactive? cmpt)
       [out-seq (ops-path-tf/p-el->pnts p-els')]
       out-seq))))
