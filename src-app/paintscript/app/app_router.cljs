 (ns paintscript.app.app-router
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [cljs.reader :refer [read-string]]
            [urlkit.core :as uk]
            [urlkit.sync :as sync]

            [paintscript.data :as data]
            [paintscript.app.canvas-module :refer [canvas]]
            [paintscript.app.gallery-module :refer [galleries]]
            [paintscript.app.ctl :as ctrl]
            [paintscript.app.cmpt0 :refer [cmpt0]]))

(defn read-edn! [data-k]
  (some-> js/window.localStorage
          (.getItem (name data-k))
          read-string))

(defn write-edn! [data-k edn]
  (-> js/window.localStorage
      (.setItem (name data-k) (pr-str edn))))

(def init-clear
  {:defs {}
   :script []})

(def conf0
  {:canvas       {:dims [100 100] :full-screen? true :scale 4 :hatching true}
   :attr-classes {"outline" {:stroke "black" :fill "none" :stroke-width 1}
                  "solid"   {:stroke "none"  :fill "black"}}})

(def conf0-gallery
  (-> conf0
      (assoc-in [:canvas :coords?] false)))

(def !app-state (r/atom {:page :canvas}))

(def c-url-sync
  {:state-atom   !app-state
   :url-to-state (fn [url]
                   (let [{:keys [fragment]} (uk/parse-url url)]
                     {:page (keyword fragment)}))
   :state-to-url (fn [{:as state :keys [page]}]
                   [:hash (name page)])})

(defn- app []
  (r/with-let [!config-canvas       (r/atom conf0)

               !c-gallery           (r/atom (or (read-edn! :gallery-config)
                                                conf0-gallery))
               !c-gallery-committed (r/atom @!c-gallery)

               !galleries           (r/atom (read-edn! :galleries))
               !galleries-committed (r/atom @!galleries)

               !cmpt0        (r/atom (-> cmpt0
                                         data/parse-cmpt))
               app-dispatch! (fn [[op-k arg :as op]]
                               (case op-k
                                 :op/set-canvas
                                 (let [[config cmpt0] arg]
                                   (reset! !config-canvas (or config
                                                              conf0))
                                   (reset! !cmpt0         (-> cmpt0
                                                              data/parse-cmpt))
                                   (swap!  !app-state assoc :page :canvas))

                                 :op/save-edn
                                 (let [gg @!galleries
                                       gc @!c-gallery]
                                   (write-edn! :galleries gg)
                                   (write-edn! :gallery-config gc)
                                   (reset! !galleries-committed gg)
                                   (reset! !c-gallery-committed gc))))]
    (let [{:keys [page]} @!app-state]
      [:div.app
       (case page
         :gallery
         [galleries app-dispatch!
          !c-gallery !c-gallery-committed
          !galleries !galleries-committed]
         (let [conf0 @!config-canvas
               cmpt0 @!cmpt0]
           [canvas conf0 cmpt0]))])))

(defn- mount-root! []
  (rd/render [#'app]
            (.getElementById js/document "app")))

(defn init! []
  (sync/init! c-url-sync)
  (mount-root!))

(defn ^:dev/after-load  _after-load [] (init!))
