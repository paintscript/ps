 (ns paintscript.app.app-router
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [cljs.reader :refer [read-string]]
            [urlkit.core :as uk]
            [urlkit.sync :as sync]

            [paintscript.app.canvas :refer [canvas]]
            [paintscript.app.gallery :refer [galleries]]
            [paintscript.app.ctl :as ctrl]))

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

(def init-config
  {:canvas
   {:dims [100 100] :scale 4}

   :styles
   {"outline" {:stroke "black" :fill "none" :stroke-width 1}
    "solid"   {:stroke "none"  :fill "black"}}})

(def init-config-gallery
  (-> init-config
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
  (r/with-let [!config-canvas       (r/atom init-config)

               !c-gallery           (r/atom (or (read-edn! :gallery-config)
                                                init-config-gallery))
               !c-gallery-committed (r/atom @!c-gallery)

               !galleries           (r/atom (read-edn! :galleries))
               !galleries-committed (r/atom @!galleries)

               !cmpt0        (r/atom ctrl/cmpt0)
               app-dispatch! (fn [[op-k arg :as op]]
                               (case op-k
                                 :set-canvas
                                 (let [[config cmpt0] arg]
                                   (reset! !config-canvas (or config
                                                              init-config))
                                   (reset! !cmpt0 cmpt0)
                                   (swap!  !app-state assoc :page :canvas))

                                 :save-edn
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
         (let [init-config @!config-canvas
               init-cmpt0  @!cmpt0]
           [canvas init-config init-cmpt0]))])))

(defn- mount-root! []
  (rd/render [#'app]
            (.getElementById js/document "app")))

(defn init! []
  (sync/init! c-url-sync)
  (mount-root!))

(defn ^:dev/after-load  _after-load [] (init!))
