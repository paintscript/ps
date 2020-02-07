 (ns paintscript.app
  (:require [reagent.core :as r]
            [cljs.reader :refer [read-string]]
            [paintscript.canvas :refer [canvas]]
            [paintscript.gallery :refer [galleries]]
            [paintscript.ctrl :as ctrl]

            [urlkit.core :as uk]
            [urlkit.sync :as sync]))

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
   {"outline" {:stroke "black" :fill "none"}
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

               !params       (r/atom ctrl/params-init)
               app-dispatch! (fn [[op-k arg :as op]]
                               (case op-k
                                 :set-canvas
                                 (let [[config params] arg]
                                   (reset! !config-canvas (or config
                                                              init-config))
                                   (reset! !params params)
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
               init-params @!params]
           [canvas init-config init-params]))])))

(defn- mount-root! []
  (r/render [#'app]
            (.getElementById js/document "app")))

(defonce _init
  (do
    (sync/init! c-url-sync)
    (mount-root!)))

(defn on-js-reload [] (mount-root!))
