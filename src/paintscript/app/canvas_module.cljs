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
  (r/with-let [!s-app       (r/atom {:ui {:!full-svg     (atom nil)
                                          :full-svg-dims nil
                                          :window-dims   nil
                                          :tab           :tab/items
                                          :hov-rec       nil
                                          :sel-rec       nil
                                          :sel-set       nil
                                          :snap          nil
                                          :snap-to-grid? true
                                          :insert-mode?  true
                                          :shell         ""}
                                     :conf cfg-init
                                     :cmpt cmpt0
                                     :op-log nil})

               !ui          (r/cursor !s-app [:ui])
               !config      (r/cursor !s-app [:conf])
               !scale       (r/cursor !s-app [:conf :canvas :scale])
               !cmpt-root   (r/cursor !s-app [:cmpt])
               !s-log       (r/cursor !s-app [:op-log])

               !win-dims    (r/cursor !s-app [:ui :window-dims])
               !hov-rec     (r/cursor !s-app [:ui :hov-rec])
               !tab         (r/cursor !s-app [:ui :tab])
               !shell       (r/cursor !s-app [:ui :shell])
               !sel-rec     (r/cursor !s-app [:ui :sel-rec])
               !sel-set     (r/cursor !s-app [:ui :sel-set])

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
                                  (dispatch! [:undo]))})

               on-resize! (fn []
                            (when-let [^js full-svg @(get-in @!s-app [:ui :!full-svg])]
                              (let [rect (-> full-svg (.getBoundingClientRect))]
                                (swap! !s-app assoc-in [:ui :full-svg-dims]
                                       [(-> rect .-width)
                                        (-> rect .-height)]))))
               _ (swap! !s-app assoc-in [:ui :on-resize!] on-resize!)
               _ (.addEventListener js/window "resize" on-resize!)]

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
        dispatch! !ui !shell !s-log !tab !sel-rec
        config cmpt-root cmpt-sel]

       [canvas-paint' c-app !s-app cmpt-root* cmpt-base* cmpt-sel*]])
    (finally
      (.removeEventListener js/window "resize" on-resize!))))
