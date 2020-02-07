(ns paintscript.gallery
  (:require [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [z-com.core :as zc]
            [keybind.core :as key]
            [reagent.core :as r]

            [paintscript.canvas :as canvas]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn- gallery-sidebar
  [dispatch! !c-gallery !c-gallery-committed !galleries !galleries-committed !tab !shell]
  (let [cg  @!c-gallery
        gg  @!galleries
        tab @!tab
        state-changes? (or (not= gg @!galleries-committed)
                           (not= cg @!c-gallery-committed))]
    [:div.sidebar.script
     [:div.controls
      {:class (when state-changes? "unsaved-changes")}
      (for [tab-k [:script :config]]
        ^{:key tab-k}
        [zc/button
         :label    (name tab-k)
         :active?  (= tab-k tab)
         :on-click #(reset! !tab tab-k)])
      [zc/button
       :class "save"
       :label "save"
       :disabled? (not state-changes?)
       :on-click #(dispatch! [:save-edn])]]

     (case tab
       :script [:textarea
                {:value     (str/trim (pprint' gg))
                 :on-focus  #(key/disable!)
                 :on-blur   #(key/enable!)
                 :on-change #(reset! !galleries (-> % .-target .-value read-string))}]
       :config [:textarea
                {:value     (str/trim (pprint' cg))
                 :on-focus  #(key/disable!)
                 :on-blur   #(key/enable!)
                 :on-change #(reset! !c-gallery (-> % .-target .-value read-string))}])

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
  [app-dispatch! !c-gallery !c-gallery-committed !galleries !galleries-committed]
  (r/with-let [!shell (r/atom "")
               !tab   (r/atom :script)]
    (let [c-base @!c-gallery
          gg     @!galleries]
      [:div.galleries
       [:div.sidebar.script-phantom]
       [gallery-sidebar app-dispatch!
        !c-gallery !c-gallery-committed
        !galleries !galleries-committed
        !tab !shell]
       [:div.gallery-main
        (for [[g-id {:as g :keys [title paintings]}] gg]
          ^{:key (hash g)}
          [:div.gallery
           [:h1 (or title g-id)]
           [:div.paintings
            (for [[item-id {:as item :keys [params]}] paintings
                  :let [c  (or (-> item :config)
                               (-> g    :config))
                        c' (-> (or c c-base)
                               (assoc-in [:canvas :coords?] false))]]
              ^{:key (hash item)}
              [:div.gallery-item
               {:on-click #(app-dispatch! [:set-canvas [c params]])}
               [canvas/canvas-paint c' (merge c' params)]])]])]])))
