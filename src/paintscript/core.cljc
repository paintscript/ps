(ns paintscript.core
  (:require [paintscript.render :as render]
            [paintscript.paint :as paint]
            #?(:cljs [paintscript.canvas :as canvas])))

(def path         (partial render/path paint/svg-renderer))
(def path-builder paint/path-builder)
(def paint        paint/paint)
(def plot-coords  #?(:cljs canvas/plot-coords))
