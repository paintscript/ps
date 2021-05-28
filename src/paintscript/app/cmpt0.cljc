(ns paintscript.app.cmpt0)

(def cmpt0
  {:config {:coords? false}
   :canvas {:dims [100 100]
            :hatching false
            :instances [{:canvas {:scale 2}}
                        {:canvas {:scale 4}}]}
   :attr-classes
   {"blue" {:fill "blue"}}
   :defs
   {:components
    {"reps"  {:canvas {:dims [100 100]},
              :script
              [[:path
                {:doc "blue circles"
                 :attr-class "blue", :repeat {:translate [30 0], :times 2}}
                [:circle {:center [20 50], :r 5}]]
               [:path
                {:doc "black trianagles"
                 :attr-class "solid",
                 :repeat {:rotate {:degree 60, :center [50 50]}, :times 6}}
                [:M [50 8]]
                [:L [39 15] [61 15]]]]}

     "group" {:canvas {:dims [30 30]
                       :zero :center}
              :attrs {:style {:stroke-width 2
                              :vector-effect "non-scaling-stroke"}}
              :script
              [[:circle {:r 15 :cx 0   :cy  0 :fill "none" :stroke "yellow"}]
               [:circle {:r 2  :cx -10.5 :cy  10.5 :fill "red" :stroke "none"}]
               [:circle {:r 2  :cx 0   :cy -15 :fill "red" :stroke "none"}]
               [:circle {:r 2  :cx 10.5  :cy  10.5 :fill "red" :stroke "none"}]]}}},
   :script
   [[:ref {} "reps"]

    [:path
     {:attr-class "outline",
      :repeat
      {:rotate {:degree 60, :center [50 50]}, :times 5, :mode :fuse}}
     [:M [46 29]]
     [:L [90 19]]
     [:L [23 34]]]

    [:path
     {:attrs {:style {:stroke "red"}}}
     [:M [10 10]]
     [:C [10 10] [90 10] [90 10]]]

    [:ref {:repeat {:tfs+ [{:tl [50 50] :sc 1.2}
                           {:tl [10 10] :sc 1.2 :rt 15}]}}
     "group"]]})
