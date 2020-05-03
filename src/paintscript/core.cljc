(ns paintscript.core
  (:require [paintscript.render :as render]
            [paintscript.render-svg :as render-svg]
            #?(:cljs [paintscript.render-svg :as render-svg])))

(def path (partial render/path render-svg/svg-renderer))

#?(:cljs (def path-builder render-svg/path-builder))
#?(:cljs (def paint        render-svg/paint))
#?(:cljs (def plot-coords  render-svg/plot-coords))
