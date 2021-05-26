
(ns paintscript.app.sidebar-items
  (:require [svg-hiccup-kit.core :as shk]
            [reagent.core :as r]
            [paintscript.nav :as nav]
            [paintscript.els :as els]
            [paintscript.paint :as paint]
            [paintscript.render :as render]
            [z-com.standard :as zc-std]
            [paintscript.util :as u]))

(defn- pth-rec-status [sel-rec pth-rec]
  (when (or (:cmpt-pth sel-rec)
            (:src-k    sel-rec))
    (let [sel? (reduce (fn [_ k]
                         (if (or (= (get sel-rec k)
                                    (get pth-rec k))
                                 (some #(nil? (get % k)) [sel-rec
                                                          pth-rec]))
                           true
                           (reduced false)))
                       false
                       [:cmpt-pth :src-k :x-el-k :p-el-i :xy-i])
          foc? (when sel?
                 (= sel-rec pth-rec))]
      [sel? foc?])))


(defn sidebar-items
  [dispatch! cmpt {:as sel-rec :keys [cmpt-pth]}]
  (let [sel-rec (or sel-rec
                    (nav/pth-rec))
        cmpt    (-> cmpt
                    (nav/get-cmpt-sel sel-rec))
        sel-rec-dispatcher (fn [sel-rec]
                             (fn [ev]
                               (.stopPropagation ev)
                               (dispatch! [:sel-rec sel-rec])))]
    [:ol.s-els
     (when cmpt-pth
       (let [cmpt-id-active (last cmpt-pth)]
         [:li.cmpt-pth
          [:ol.cmpt-pth
           (loop [cmpt-pth-acc nil
                  [cmpt-id
                   & cmpt-pth-tail] (cons :root cmpt-pth)
                  li-seq (list)]
             (if-not cmpt-id
               (reverse li-seq)
               (let [cmpt-pth-acc' (-> cmpt-pth-acc (cond-> (not= :root cmpt-id)
                                                            (conj cmpt-id)))
                     active?       (= cmpt-id-active
                                      cmpt-id)]
                 (recur cmpt-pth-acc'
                        cmpt-pth-tail
                        (-> li-seq
                            (conj
                             ^{:key cmpt-id}
                             [:li (if active?
                                    {:class "current"}
                                    {:on-click #(dispatch!
                                                 [:sel-rec
                                                  (nav/pth-rec :cmpt-pth cmpt-pth-acc')])})
                              [:span.cmpt-id cmpt-id]]))))))]]))
     (when-let [cmpts (get-in cmpt [:defs :components])]
       [:li.cmpt-sel
        [zc-std/select
         :model     (or (-> sel-rec :cmpt-pth last)
                        "root")
         :options   (cons "root"
                          (keys cmpts))
         :on-change #(dispatch! [:sel-rec (nav/pth-rec :cmpt-pth
                                                       (-> (:cmpt-pth sel-rec)
                                                           (u/conjv %)))])]])
     (for [[s-el-i
            [s-el-k
             s-el-opts
             & s-el-args]] (map-indexed vector (:script cmpt))]
       (let [pth-rec*  (nav/pth-rec :cmpt-pth (:cmpt-pth sel-rec)
                                    :src-k    :script
                                    :x-el-k   s-el-i)
             [sel?
              foc?]    (pth-rec-status sel-rec pth-rec*)]
         ^{:key s-el-i}
         [:li {:class (when foc? "active")
               :on-click (sel-rec-dispatcher pth-rec*)}
          [:span.s-el-k (name s-el-k)]
          [:span.s-el-opts (pr-str s-el-opts)]
          [:span.s-el-args
           (case s-el-k
             :path (pr-str s-el-args)
             :ref  (first s-el-args)
             nil)]
          (when (and sel?
                     (= :path s-el-k)
                     (first s-el-args))
            [:div
             [:ol.p-els
              (for [[p-el-i
                     p-el] (map-indexed vector s-el-args)]
                (let [{:as p-el-rec
                       :keys [p-el-k
                              i-arg0
                              args]} (els/el-vec-->el-rec p-el)

                      p-el-i'   (+ nav/p-el-i0
                                   p-el-i)
                      pth-rec*  (-> pth-rec* (assoc :p-el-i p-el-i'))

                      [sel?
                       foc?]    (pth-rec-status sel-rec pth-rec*)]
                  ^{:key p-el-i}
                  [:li {:class (when foc? "active")
                        :on-click (sel-rec-dispatcher pth-rec*)}
                   [:span.p-el-k (name p-el-k)]
                   [:ol.p-el-args
                    (for [[arg-i
                           arg] (map-indexed vector args)]
                      (let [xy-i      (+ i-arg0
                                         arg-i)
                            pth-rec*  (-> pth-rec* (assoc :xy-i xy-i))
                            [sel?
                             foc?]    (pth-rec-status sel-rec pth-rec*)]
                        ^{:key arg-i}
                        [:li {:class (when foc? "active")
                              :on-click (sel-rec-dispatcher pth-rec*)}
                         (pr-str arg)]))]]))]
             [:span.p-out
              (->> s-el-args
                   (render/path paint/svg-renderer cmpt s-el-opts )
                   (apply shk/d))]])]))]))
