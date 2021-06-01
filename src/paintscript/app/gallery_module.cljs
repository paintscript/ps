(ns paintscript.app.gallery-module
  (:require [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [z-com.core :as zc]
            [keybind.core :as key]
            [reagent.core :as r]

            [paintscript.util :as u]
            [paintscript.data :as data]
            [paintscript.canvas :as canvas]
            ; [paintscript.app.canvas-module :as canvas-module]
            ))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn- gallery-sidebar
  [dispatch! !c-gallery !c-gallery-committed !root-def !root-def-committed !tab !shell]
  (let [cg    @!c-gallery
        gg    @!root-def
        tab   @!tab
        diff? (or (not= gg @!root-def-committed)
                  (not= cg @!c-gallery-committed))]
    [:div.sidebar.script
     [:div.controls
      {:class (when diff? "unsaved-changes")}
      (for [tab-k [:script :config]]
        ^{:key tab-k}
        [zc/button
         :label    (name tab-k)
         :active?  (= tab-k tab)
         :on-click #(reset! !tab tab-k)])
      [zc/button
       :class "save"
       :label "save"
       :disabled? (not diff?)
       :on-click #(dispatch! [:save-edn])]]

     (case tab
       :script [:div.sidebar-config
                [:textarea
                 {:value     (str/trim (pprint' gg))
                  :on-focus  #(key/disable!)
                  :on-blur   #(key/enable!)
                  :on-change #(reset! !root-def (-> % .-target .-value read-string))}]]
       :config [:div.sidebar-config
                [:textarea
                 {:value     (str/trim (pprint' cg))
                  :on-focus  #(key/disable!)
                  :on-blur   #(key/enable!)
                  :on-change #(reset! !c-gallery (-> % .-target .-value read-string))}]])

     [:div.shell
      [:textarea
       {:value       @!shell
        :placeholder "enter command..."
        :on-focus    #(key/disable!)
        :on-blur     #(key/enable!)
        :on-change   #(reset! !shell (-> % .-target .-value))
        :on-key-down (fn [e]
                       (let [k (max (.-keyCode e)
                                    (.-which e))
                             cmd @!shell]
                         (when (= 13 k)
                           (reset! !shell "")
                           (dispatch! [:cmd (str/trim cmd)])
                           (-> e (.preventDefault)))))}]]]))

(defn galleries
  [app-dispatch! !c-gallery !c-gallery-committed !root-def !root-def-committed]
  (r/with-let [!shell (r/atom "")
               !tab   (r/atom :script)]

    (let [c-base              @!c-gallery
          {:as root-def
           :keys [galleries]} @!root-def]

      [:div.galleries
       [:div.sidebar.script-phantom]
       [gallery-sidebar app-dispatch!
        !c-gallery !c-gallery-committed
        !root-def  !root-def-committed
        !tab       !shell]
       [:div.gallery-main

        ;; --- gallery list
        (for [[g-id {:as gallery :keys [title paintings]}] galleries]
          ^{:key g-id}
          [:div.gallery
           [:h1 (or title g-id)]
           [:div.paintings

            ;; --- painting list
            (for [[painting-id
                   {:as painting :keys [component]}] (->> paintings
                                                          sort)
                  :let [c  (u/deep-merge (-> root-def :config)
                                         (-> gallery  :config)
                                         (-> painting :config))
                        c' (-> (or c
                                   c-base)
                               (assoc-in [:canvas :coords?] false))]]
              ^{:key painting-id}
              [:div.gallery-item
               {:on-click #(app-dispatch! [:set-canvas [c component]])}
               [:div.gallery-item-title (or (:title painting)
                                            painting-id)]
               [canvas/canvas-paint (-> (u/deep-merge c' component)
                                        data/parse-cmpt)]
               ])]])]])))
