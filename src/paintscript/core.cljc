(ns paintscript.core
  (:require [paintscript.render :as render]
            [paintscript.render-svg :as render-svg]
            #?(:cljs [paintscript.render-svg-web :as render-svg-web])))

(def path (partial render/path render-svg/svg-renderer))
#?(:cljs (def path-builder render-svg-web/path-builder))
#?(:cljs (def paint        render-svg-web/paint))
#?(:cljs (def plot-coords  render-svg-web/plot-coords))
