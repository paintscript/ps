(ns paintscript.core
  (:require [paintscript.render :as render]
            [paintscript.paint :as paint]
            [paintscript.data :as data]))

(defn path
  ([p-els]        (path nil nil    p-els))
  ([p-opts p-els] (path nil p-opts p-els))
  ([cmpt p-opts p-els & args]
   (let [p-recs (mapv data/elv->r p-els)]
     (apply render/path paint/svg-renderer cmpt p-opts p-recs args))))

(def path-builder paint/path-builder)

(defn paint
  ([cmpt] (paint nil cmpt))
  ([c-fns cmpt] (paint/paint c-fns (data/parse-cmpt cmpt))))
