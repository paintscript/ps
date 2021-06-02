(ns paintscript.app.sidebar-items
  (:require [clojure.string :as str]
            [svg-hiccup-kit.core :as shk]
            [z-com.standard :as zc-std]
            [reagent.core :as r]

            [paintscript.util :as u]
            [paintscript.nav :as nav]
            [paintscript.paint :as paint]
            [paintscript.render :as render]))

(defn- nav-rec-status
  "notes:
   - a path counts as focused when the selection is idential to the path
   - a path counts as selected if the prefix up to the last non-nil elemen matches
     that of the selection (this includes more specific selections that have
     additional non-nil fields after this prefix)
   "
  [navr-sel navr-ctx]
  (when (:src-k navr-sel)
    (cond
      (= navr-sel navr-ctx) [true true]
      (not= (:cmpt-pth navr-sel)
            (:cmpt-pth navr-ctx)) nil
      :else
      (let [sel? (reduce (fn [_ k]
                           (let [s (get navr-sel k)
                                 p (get navr-ctx k)]
                             (cond
                               (and p
                                    (not= s p)) (reduced false)
                               :else            true)))
                         false
                         [:src-k :x-el-k :p-el-i :xy-i])]
        [sel? false]))))

(defn sidebar-source-item
  [dispatch! !navr-sel navr-sel-dispatcher
   cmpt-sel
   s-el-i
   {:as s-el
    s-el-k :el-k
    s-el-opts :el-opts
    s-el-args :el-argv}]
  (let [navr-sel  @!navr-sel
        navr-ctx* (nav/nav-rec :cmpt-pth0 (:cmpt-pth0 navr-sel)
                               :ref-pth   (:ref-pth   navr-sel)
                               :cmpt-pth  (:cmpt-pth  navr-sel)
                               :src-k     :script
                               :x-el-k    s-el-i)
        [sel?
         foc?]    (nav-rec-status navr-sel navr-ctx*)]

    [:li {:class (when foc? "active")
          :on-click (navr-sel-dispatcher navr-ctx*)}
     [:span.s-el-k (name s-el-k)]
     (when-let [doc (:doc s-el-opts)]
       [:span.s-el-doc doc])
     [:span.s-el-opts (-> s-el-opts
                          (dissoc :doc)
                          pr-str)]
     [:span.s-el-args
      (case s-el-k
        ; :path [:span.data (pr-str s-el-args)]
        :ref  (let [cmpt-id (first s-el-args)]
                [:span.ref {:on-click
                            (fn [^js ev]
                              (.stopPropagation ev)
                              (dispatch! [:nav-into-ref s-el]))}
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
                  :keys [el-k
                         ; i-arg0
                         el-argv]} (-> p-el
                                       ; els/el-vec-->el-rec
                                       )

                 p-el-i'        p-el-i
                 navr-ctx*      (-> navr-ctx* (assoc :p-el-i p-el-i'))
                 [sel?
                  foc?]         (nav-rec-status navr-sel
                                                navr-ctx*)]
             ^{:key p-el-i}
             [:li {:class (when foc? "active")
                   :on-click (navr-sel-dispatcher navr-ctx*)}
              [:span.p-el-k (name el-k)]
              [:ol.p-el-args
               (for [[arg-i
                      arg] (map-indexed vector el-argv)]
                 (let [xy-i      (+ ;i-arg0
                                    arg-i)
                       navr-ctx*  (-> navr-ctx* (assoc :xy-i xy-i))
                       [sel?
                        foc?]    (nav-rec-status navr-sel navr-ctx*)]
                   ^{:key arg-i}
                   [:li {:class (when foc? "active")
                         :on-click (navr-sel-dispatcher navr-ctx*)}
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
                                            [:navr-sel
                                             (nav/nav-rec :cmpt-pth cmpt-pth-acc')])})
                         [:span.cmpt-id cmpt-id]]))))))]]))


(defn sidebar-items
  [dispatch! !navr-sel cmpt-root cmpt-sel]
  (let [{:as navr-sel
         :keys [ref-pth
                cmpt-pth]} @!navr-sel

        navr-sel-dispatcher (fn [navr-sel*]
                              (fn [^js ev]
                                (.stopPropagation ev)
                                (dispatch! [:navr-sel (when (not= @!navr-sel
                                                                  navr-sel*)
                                                        navr-sel*)])))]
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
          :model     (or (-> @!navr-sel :cmpt-pth last)
                         "root")
          :options   (cons "root"
                           (keys cmpts))
          :on-change #(dispatch! [:sel-cmpt-id %])]])

      ;; --- script items

      (for [[s-el-i
             s-el] (map-indexed vector (:script cmpt-sel))]
        ^{:key s-el-i}
        [sidebar-source-item dispatch! !navr-sel navr-sel-dispatcher cmpt-sel s-el-i s-el])]]))
