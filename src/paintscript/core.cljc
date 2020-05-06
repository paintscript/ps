(ns paintscript.core
  (:require [paintscript.render :as render]
            [paintscript.render-svg :as render-svg]
            #?(:cljs [paintscript.render-svg :as render-svg])))

(def path (partial render/path render-svg/svg-renderer))

(def path-builder #?(:cljs render-svg/path-builder))
(def paint        #?(:cljs render-svg/paint))
(def plot-coords  #?(:cljs render-svg/plot-coords))
