
(ns paintscript.app.sidebar-items
  (:require [svg-hiccup-kit.core :as shk]
            [reagent.core :as r]
            [paintscript.nav :as nav]
            [paintscript.els :as els]
            [paintscript.paint :as paint]
            [paintscript.render :as render]))

(defn- pth-vec-status [sel-vec pth-vec]
  (when sel-vec
    (let [sel? (->> (map vector sel-vec pth-vec)
                    (every? (fn [[a b]] (or (= a b)
                                            (some nil? [a b])))))
          foc? (when sel?
                 (= sel-vec pth-vec))]
      [sel? foc?])))

(defn sidebar-items
  [dispatch! cmpt sel-rec]
  (let [sel-vec (some-> sel-rec nav/pth-rec->vec)
        sel-vec-dispatcher (fn [sel-vec]
                             (fn [ev]
                               (.stopPropagation ev)
                               (dispatch! [:sel-vec sel-vec])))]
    [:ol.s-els
     (for [[s-el-i
            [s-el-k
             s-el-opts
             & s-el-args]] (map-indexed vector (:script cmpt))]
       (let [pth-vec*  [:script s-el-i]
             [sel?
              foc?]    (pth-vec-status sel-vec pth-vec*)]
         ^{:key s-el-i}
         [:li {:class (when foc? "active")
               :on-click (sel-vec-dispatcher pth-vec*)}
          [:span.s-el-k (name s-el-k)]
          [:span.s-el-opts (pr-str s-el-opts)]
          (when (= :path s-el-k)
            [:span.s-el-args (pr-str s-el-args)])
          (when (and sel?
                     (= :path s-el-k))
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
                      pth-vec*  (-> pth-vec* (conj p-el-i'))

                      [sel?
                       foc?]    (pth-vec-status sel-vec pth-vec*)]
                  ^{:key p-el-i}
                  [:li {:class (when foc? "active")
                        :on-click (sel-vec-dispatcher pth-vec*)}
                   [:span.p-el-k (name p-el-k)]
                   [:ol.p-el-args
                    (for [[arg-i
                           arg] (map-indexed vector args)]
                      (let [xy-i      (+ i-arg0
                                         arg-i)
                            pth-vec*  (-> pth-vec* (conj xy-i))
                            [sel?
                             foc?]    (pth-vec-status sel-vec pth-vec*)]
                        ^{:key arg-i}
                        [:li {:class (when foc? "active")
                              :on-click (sel-vec-dispatcher pth-vec*)}
                         (pr-str arg)]))]]))]
             [:span.p-out
              (->> s-el-args
                   (render/path paint/svg-renderer cmpt s-el-opts )
                   (apply shk/d))]])]))]))
