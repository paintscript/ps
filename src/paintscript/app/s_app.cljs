(ns paintscript.app.s-app
  (:require [reagent.core :as r]))

(defn derive-scale [!s-app {:as canvas :keys [full-screen? scale]}]
  (or (when full-screen?
        @(r/cursor !s-app [:ui :full-svg-scale]))
      scale))

(defn- init-full-svg-params
  "used by:
   - canvas
   - ctl/drag-and-drop-fns xy-svg!"
  [!s-app svg-bounds {:as canvas :keys [dims scale]}]
  (let [scale      (or @(r/cursor !s-app [:ui :full-svg-scale])
                       scale)
        canvas-wh  (->> dims (mapv #(* % scale)))
        ;; NOTE: rounding creates sharper hatching lines
        canvas-xy0 (mapv #(-> %1 (- %2) (/ 2) Math/round)
                         svg-bounds
                         canvas-wh)]
    {:canvas     canvas
     :scale      scale
     :canvas-wh  canvas-wh
     :canvas-xy0 canvas-xy0}))
