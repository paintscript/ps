(ns paintscript.app.canvas-module
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [keybind.core :as key]
            [paintscript.util :as u]
            [paintscript.app.canvas :as canvas]
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
                                          :tab           :tab/elems
                                          :navr-hov       nil
                                          :navr-sel       nil
                                          :sel-set       nil
                                          :snap          nil
                                          :snap-to-grid? true
                                          :insert-mode?  true
                                          :shell         ""}
                                     :conf-ext  conf0
                                     :cmpt-root cmpt0
                                     :s-log     nil})

               !ui          (r/cursor !s-app [:ui])
               !conf-ext    (r/cursor !s-app [:conf-ext])
               !scale       (r/cursor !s-app [:conf-ext :canvas :scale])
               !cmpt-root   (r/cursor !s-app [:cmpt-root])
               !s-log       (r/cursor !s-app [:s-log])

               !win-dims    (r/cursor !s-app [:ui :window-dims])
               !navr-hov    (r/cursor !s-app [:ui :navr-hov])
               !tab         (r/cursor !s-app [:ui :tab])
               !shell       (r/cursor !s-app [:ui :shell])
               !navr-sel    (r/cursor !s-app [:ui :navr-sel])
               !sel-set     (r/cursor !s-app [:ui :sel-set])

               dispatch!      (partial ctl/dispatch! !s-app)
               derive-dnd-fns (ctl/drag-and-drop-fns !cmpt-root !ui dispatch!)
               kb-fns         (ctl/keybind-fns       !cmpt-root !ui dispatch!)

               report-down! (fn [navr i-main shift?]
                              (let [navr-sel0 @!navr-sel

                                    navr-sel  (-> navr
                                                  (with-meta {:main?  (not i-main)
                                                              :shift? shift?}))
                                    navr-sel' (-> navr-sel

                                                  ;; NOTE: locr doesn't track {cmpt0, ref}-pth
                                                  (cond-> (= :cmpt-pth navr-sel0)
                                                          (= :cmpt-pth navr-sel))
                                                  (assoc :cmpt-pth0 (:cmpt-pth0 navr-sel0))
                                                  (assoc :ref-pth   (:ref-pth navr-sel0)))]

                                ; NOTE: toggle doesn't work w/ select followed
                                ;; by select+drag
                                ; (swap! !navr-sel #(when (not= % navr-sel) navr-sel))
                                (dispatch! [:navr-sel navr-sel'])))

               report-over! (fn [navr val]
                              (swap! !navr-hov
                                     #(cond
                                        val        navr
                                        (= navr %) nil
                                        :else      %)))

               ; (reset! !s-log (s-log/init @!cmpt-root @!ui))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))

               canvas-paint' (with-meta #'canvas/canvas-paint
                               {:component-did-catch
                                (fn [e info]
                                  (println :paint-error e)
                                  (js/console.log e)
                                  (dispatch! [:op.s-log/undo]))})

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
          navr-sel   @!navr-sel
          cmpt-root  @!cmpt-root
          navr-hov   @!navr-hov

          c-app      {:dispatch!      dispatch!
                      :report-down!   report-down!
                      :report-over!   report-over!
                      :derive-dnd-fns derive-dnd-fns}

          s-app      {:navr-hov navr-hov
                      :navr-sel navr-sel}

          ;; TODO: write dedicated merge fn that only deep-merges where it makes sense
          ;; (e.g. it makes sense for :defs but not for :canvas where a new value
          ;; shouldn't inherit a parent's :background)
          cmpt-root* (u/deep-merge conf-ext
                                   (:config cmpt-root)
                                   cmpt-root)

          [cmpt-base
           cmpt-sel] (nav/get-cmpt-sel cmpt-root* navr-sel)

          cmpt-base* (-> cmpt-base
                         (nav/cmpt-merge-canvas cmpt-root*
                                                (nav/nav-rec :cmpt-pth (:cmpt-pth0 navr-sel))))

          cmpt-sel*  (-> cmpt-sel
                         ;; NOTE: merges upstream defs (needed to resolve refs
                         ;; during render)
                         (nav/cmpt-merge-defs cmpt-root* navr-sel))]
      [:div.canvas
       [:div.sidebar.script-phantom]
       [canvas-sidebar dispatch! !ui !shell !s-log !tab !navr-sel conf-ext cmpt-root]
       [canvas-paint'  c-app !s-app cmpt-root* cmpt-base* cmpt-sel*]])
    (finally
      (.removeEventListener js/window "resize" on-resize!))))
