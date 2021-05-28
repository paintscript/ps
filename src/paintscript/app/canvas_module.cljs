(ns paintscript.app.canvas-module
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [keybind.core :as key]
            [paintscript.els :as els]
            [paintscript.app.ctl :as ctl]
            [paintscript.util :as u]
            [paintscript.canvas :as canvas]
            [paintscript.app.sidebar :refer [canvas-sidebar]]
            [paintscript.nav :as nav]))

(defn canvas [cfg-init cmpt0]
  (r/with-let [ui-init      {:sel-rec       nil
                             :sel-set       nil
                             :snap          nil
                             :snap-to-grid? true
                             :insert-mode?  true}

               !ui          (r/atom   ui-init)
               !config      (r/atom   cfg-init)
               !cmpt-root   (r/atom   cmpt0)
               !s-log       (r/atom   nil)
               !hov-rec     (r/atom   nil)
               !tab         (r/atom   :tab/items)
               !shell       (r/atom   "")

               !sel-rec     (r/cursor !ui     [:sel-rec])
               !sel-set     (r/cursor !ui     [:sel-set])
               !scale       (r/cursor !config [:canvas :scale])


               dispatch!      (partial ctl/dispatch! !config !cmpt-root !s-log !ui)
               derive-dnd-fns (ctl/drag-and-drop-fns !cmpt-root !ui dispatch!)
               kb-fns         (ctl/keybind-fns       !cmpt-root !ui dispatch!)

               report-down! (fn [pth-rec i-main shift?]
                              (let [sel-rec (-> pth-rec
                                                (with-meta {:main?  (not i-main)
                                                            :shift? shift?}))]
                                ; NOTE: toggle doesn't work w/ select followed
                                ;; by select+drag
                                ; (swap! !sel-rec #(when (not= % sel-rec) sel-rec))
                                (reset! !sel-rec sel-rec)))
               report-over! (fn [pth-rec val]
                              (swap! !hov-rec
                                     #(cond
                                        val           pth-rec
                                        (= pth-rec %) nil
                                        :else         %)))

               ; (reset! !s-log (s-log/init @!cmpt-root @!ui))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))

               canvas-paint' (with-meta #'canvas/canvas-paint
                               {:component-did-catch
                                (fn [e info]
                                  (println :paint-error e)
                                  (js/console.log e)
                                  (dispatch! [:undo]))})]

    (let [config    @!config
          sel-rec   @!sel-rec
          cmpt-root @!cmpt-root
          hov-rec   @!hov-rec

          c-app   {:dispatch!      dispatch!
                   :report-down!   report-down!
                   :report-over!   report-over!
                   :derive-dnd-fns derive-dnd-fns}

          s-app   {:hov-rec hov-rec
                   :sel-rec sel-rec}

          cmpt-root* (u/deep-merge config
                                   (:config cmpt-root)
                                   cmpt-root)

          [cmpt-base
           cmpt-sel] (nav/get-cmpt-sel cmpt-root* sel-rec)

          cmpt-base* (-> cmpt-base
                         (nav/cmpt-merge-canvas cmpt-root*
                                                (nav/pth-rec :cmpt-pth (:cmpt-pth0 sel-rec))))

          cmpt-sel*  (-> cmpt-sel
                         ;; NOTE: merges upstream defs (needed to resolve refs
                         ;; during render)
                         (nav/cmpt-merge-defs cmpt-root* sel-rec)
                         (update :script els/attach-pth-rec-meta* sel-rec))]
      [:div.canvas
       [:div.sidebar.script-phantom]

       [canvas-sidebar
        !ui !shell !s-log !tab
        config cmpt-root cmpt-sel sel-rec dispatch!]

       [canvas-paint' c-app s-app cmpt-root* cmpt-base* cmpt-sel*]])))
