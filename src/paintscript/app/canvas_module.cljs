(ns paintscript.app.canvas-module
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [keybind.core :as key]
            [paintscript.els :as els]
            [paintscript.util :as u]
            [paintscript.canvas :as canvas]
            [paintscript.app.sidebar :refer [canvas-sidebar]]
            [paintscript.nav :as nav]

            [paintscript.app.ctl :as ctl]
            [paintscript.app.s-app :as s-app]))

(defn canvas [conf0 cmpt0]
  (r/with-let [!s-app       (r/atom {:ui {:!full-svg       (atom nil)
                                          :full-svg-bounds [800 800]
                                          :full-svg-xy0    [0 0]
                                          :full-svg-scale  nil
                                          :full-svg-params {:scale      1
                                                            :canvas-wh  [800 800]
                                                            :canvas-xy0 [0 0]}
                                          :tab           :tab/items
                                          :hov-rec       nil
                                          :sel-rec       nil
                                          :sel-set       nil
                                          :snap          nil
                                          :snap-to-grid? true
                                          :insert-mode?  true
                                          :shell         ""}
                                     :conf conf0
                                     :cmpt cmpt0
                                     :op-log nil})

               !ui          (r/cursor !s-app [:ui])
               !conf-ext    (r/cursor !s-app [:conf])
               !scale       (r/cursor !s-app [:conf :canvas :scale])
               !cmpt-root   (r/cursor !s-app [:cmpt])
               !s-log       (r/cursor !s-app [:op-log])

               !win-dims    (r/cursor !s-app [:ui :window-dims])
               !hov-rec     (r/cursor !s-app [:ui :hov-rec])
               !tab         (r/cursor !s-app [:ui :tab])
               !shell       (r/cursor !s-app [:ui :shell])
               !sel-rec     (r/cursor !s-app [:ui :sel-rec])
               !sel-set     (r/cursor !s-app [:ui :sel-set])

               dispatch!      (partial ctl/dispatch! !conf-ext !cmpt-root !s-log !ui)
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

               on-resize! (fn [arg]
                            ;; NOTE: invoked first by svg's :ref with canvas arg,
                            ;; and then later via resize w/o canvas arg
                            (when-let [^js full-svg @(get-in @!s-app [:ui :!full-svg])]
                              (let [rect       (-> full-svg (.getBoundingClientRect))
                                    svg-bounds [(-> rect .-width)
                                                (-> rect .-height)]]
                                (swap! !s-app update-in [:ui :full-svg-params]
                                       (fn [full-svg-params]
                                         (s-app/init-full-svg-params !s-app svg-bounds
                                                                     (or (when (map? arg)
                                                                           arg)
                                                                         (:canvas full-svg-params))))))))
               _ (swap! !s-app assoc-in [:ui :on-resize!] on-resize!)
               _ (.addEventListener js/window "resize" on-resize!)]

    (let [conf-ext   @!conf-ext
          sel-rec    @!sel-rec
          cmpt-root  @!cmpt-root
          hov-rec    @!hov-rec

          c-app      {:dispatch!      dispatch!
                      :report-down!   report-down!
                      :report-over!   report-over!
                      :derive-dnd-fns derive-dnd-fns}

          s-app      {:hov-rec hov-rec
                      :sel-rec sel-rec}

          cmpt-root* (u/deep-merge conf-ext
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

       [canvas-sidebar dispatch! !ui !shell !s-log !tab !sel-rec conf-ext cmpt-root]
       [canvas-paint'  c-app !s-app cmpt-root* cmpt-base* cmpt-sel*]])
    (finally
      (.removeEventListener js/window "resize" on-resize!))))
