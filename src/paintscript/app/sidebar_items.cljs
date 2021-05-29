(ns paintscript.app.sidebar-items
  (:require [clojure.string :as str]
            [svg-hiccup-kit.core :as shk]
            [z-com.standard :as zc-std]
            [reagent.core :as r]

            [paintscript.util :as u]
            [paintscript.nav :as nav]
            [paintscript.els :as els]
            [paintscript.paint :as paint]
            [paintscript.render :as render]))

(defn- pth-rec-status
  "notes:
   - a path counts as focused when the selection is idential to the path
   - a path counts as selected if the prefix up to the last non-nil elemen matches
     that of the selection (this includes more specific selections that have
     additional non-nil fields after this prefix)
   "
  [sel-rec pth-rec]
  (when (:src-k sel-rec)
    (cond
      (= sel-rec pth-rec) [true true]
      (not= (:cmpt-pth sel-rec)
            (:cmpt-pth pth-rec)) nil
      :else
      (let [sel? (reduce (fn [_ k]
                           (let [s (get sel-rec k)
                                 p (get pth-rec k)]
                             (cond
                               (and p
                                    (not= s p)) (reduced false)
                               :else            true)))
                         false
                         [:src-k :x-el-k :p-el-i :xy-i])]
        [sel? false]))))

(defn sidebar-source-item
  [dispatch! !sel-rec sel-rec-dispatcher
   cmpt-sel
   s-el-i
   [s-el-k
    s-el-opts
    & s-el-args
    :as s-el]]
  (let [sel-rec   @!sel-rec
        pth-rec*  (nav/pth-rec :cmpt-pth0 (:cmpt-pth0 sel-rec)
                               :ref-pth   (:ref-pth   sel-rec)
                               :cmpt-pth  (:cmpt-pth  sel-rec)
                               :src-k     :script
                               :x-el-k    s-el-i)
        [sel?
         foc?]    (pth-rec-status sel-rec pth-rec*)]

    [:li {:class (when foc? "active")
          :on-click (sel-rec-dispatcher pth-rec*)}
     [:span.s-el-k (name s-el-k)]
     (when-let [doc (:doc s-el-opts)]
       [:span.s-el-doc doc])
     [:span.s-el-opts (-> s-el-opts
                          (dissoc :doc)
                          pr-str)]
     [:span.s-el-args
      (case s-el-k
        :path [:span.data (pr-str s-el-args)]
        :ref  (let [cmpt-id (first s-el-args)]
                [:span.ref {:on-click
                            (fn [^js ev]
                              (.stopPropagation ev)
                              (dispatch! [:sel-ref s-el]))}
                 cmpt-id])
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

                 p-el-i'        (+ nav/p-el-i0
                                   p-el-i)
                 pth-rec*       (-> pth-rec* (assoc :p-el-i p-el-i'))
                 [sel?
                  foc?]         (pth-rec-status sel-rec pth-rec*)]
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
        (when sel?
          [:span.p-out {:on-click (fn [^js ev] (.stopPropagation ev))}
           (->> s-el-args
                (render/path paint/svg-renderer cmpt-sel s-el-opts )
                (apply shk/d))])])]))

(defn cmpt-pth-view [dispatch! cmpt-pth]
  (let [cmpt-id-active (last cmpt-pth)]
    [:li.cmpt-pth
     [:ol.cmpt-pth
      (loop [cmpt-pth-acc      nil
             [cmpt-id
              & cmpt-pth-tail] (cons :root cmpt-pth)
             li-seq            (list)]
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


(defn sidebar-items
  [dispatch! !sel-rec cmpt-root cmpt-sel]
  (let [{:as sel-rec
         :keys [ref-pth
                cmpt-pth]} @!sel-rec

        sel-rec-dispatcher (fn [sel-rec*]
                             (fn [^js ev]
                               (.stopPropagation ev)
                               (dispatch! [:sel-rec (when (not= @!sel-rec
                                                                sel-rec*)
                                                      sel-rec*)])))]
    [:div.sidebar-items
     [:ol.s-els

      ;; --- cmpt-pth

      (when cmpt-pth
        [cmpt-pth-view dispatch! cmpt-pth])

      ;; --- ref-pth

      (when ref-pth
        [:li.cmpt-pth
         [zc-std/checkbox
          :model     true
          :on-change #(dispatch! [:disable-ref-pth])
          :label     (str "in context (" (->> ref-pth
                                              (map #(str "$" (:cmpt-id %)))
                                              (cons "root")
                                              (str/join " → "))
                          ")")]])

      ;; --- cmpt-selector

      (when-let [cmpts (get-in cmpt-sel [:defs :components])]
        [:li.cmpt-sel
         [zc-std/select
          :model     (or (-> @!sel-rec :cmpt-pth last)
                         "root")
          :options   (cons "root"
                           (keys cmpts))
          :on-change #(dispatch! [:sel-cmpt-id %])]])

      ;; --- script items

      (for [[s-el-i
             s-el] (map-indexed vector (:script cmpt-sel))]
        ^{:key s-el-i}
        [sidebar-source-item dispatch! !sel-rec sel-rec-dispatcher cmpt-sel s-el-i s-el])]]))
