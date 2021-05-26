(ns paintscript.app.canvas-module
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [keybind.core :as key]
            [paintscript.els :as els]
            [paintscript.app.ctl :as ctl]
            [paintscript.canvas :as canvas]
            [paintscript.app.sidebar :refer [canvas-sidebar]]))

(defn canvas [cfg-init cmpt0]
  (r/with-let [ui-init      {:sel-rec       nil
                             :sel-set       nil
                             :snap          nil
                             :snap-to-grid? true
                             :insert-mode?  true}

               !ui          (r/atom   ui-init)
               !config      (r/atom   cfg-init)
               !cmpt        (r/atom   cmpt0)
               !s-log       (r/atom   nil)
               !hov-rec     (r/atom   nil)
               !tab         (r/atom   :tab/items)
               !shell       (r/atom   "")

               !sel-rec     (r/cursor !ui     [:sel-rec])
               !sel-set     (r/cursor !ui     [:sel-set])
               !scale       (r/cursor !config [:canvas :scale])


               dispatch!    (partial ctl/dispatch! !config !cmpt !s-log !ui)
               dnd-fns      (ctl/drag-and-drop-fns !cmpt !ui dispatch!)
               kb-fns       (ctl/keybind-fns       !cmpt !ui dispatch!)

               report-down! (fn [pth-rec i-main shift?]
                              (reset! !sel-rec
                                      (-> pth-rec
                                          (with-meta {:main?  (not i-main)
                                                      :shift? shift?}))))
               report-over! (fn [pth-rec val]
                              (swap! !hov-rec
                                     #(cond
                                        val           pth-rec
                                        (= pth-rec %) nil
                                        :else         %)))

               ; (reset! !s-log (s-log/init @!cmpt @!ui))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))

               canvas-paint' (with-meta #'canvas/canvas-paint
                               {:component-did-catch
                                (fn [e info]
                                  (println :paint-error e)
                                  (js/console.log e)
                                  (dispatch! [:undo]))})]

    (let [config  @!config
          cmpt    @!cmpt
          sel-rec @!sel-rec
          hov-rec @!hov-rec
          cmpt'   (-> (merge-with merge
                                  (-> config (dissoc :script))
                                  cmpt)
                      (update :script els/attach-ii-el-meta*))
          c-app   [hov-rec sel-rec dispatch! report-down! report-over! dnd-fns]]

      [:div.canvas
       [:div.sidebar.script-phantom]
       [canvas-sidebar
        !config !cmpt !ui !shell !s-log !tab
        config cmpt cmpt' sel-rec dispatch!]

       [canvas-paint' c-app config cmpt']])))
