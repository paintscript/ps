(ns paintscript.app.s-app
  (:require [reagent.core :as r]))

(defn derive-scale [!s-app {:as canvas :keys [full-screen? scale]}]
  (or (when full-screen?
        @(r/cursor !s-app [:ui :full-scale]))
      scale))
