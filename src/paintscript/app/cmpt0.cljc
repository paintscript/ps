(ns paintscript.app.cmpt0)

(def cmpt0
  {:config {:coords? false}
   :canvas {:dims [100 100]
            :hatching true
            ; :full-screen? true
            :instances [{:canvas {:scale 2}}
                        {:canvas {:scale 4}}]
            }
   :attr-classes
   {"blue" {:fill "blue"}
    "outline" {:stroke "blue"}}
   :defs
   {"red-curve"
    [[:M [10 10]]
     [:C [125 33] [-8 61] [90 10]]]
    :components
    {"nothing" {:script
                [[:path {:attr-class "outline"} [:M [50 50]] [:L [60 60]]]]}

     "reps"  {:canvas {:dims [90 90]}
              :script
              [[:path
                {:doc "repeat tl (blue circles)"
                 :attr-class "blue", :repeat {:translate [30 0], :times 2}}
                [:circle {:center [20 50], :r 5}]]

               [:path
                {:doc "repeat rt (black trianagles)"
                 :attr-class "solid",
                 :repeat {:rotate {:degree 60, :center [50 50]}, :times 6}}
                [:M [50 8]]
                [:L [39 15] [61 15]]]

               [:path
                {:doc "repeat rt w/ fusion"
                 :attr-class "outline"
                 :repeat
                 {:rotate {:degree 60, :center [50 50]}
                  :times 5
                  :mode :fuse}}
                [:M [46 29]]
                [:L [90 19]]
                [:L [23 34]]]]}

     "circs" {:canvas {:dims [50 50]
                       :zero :center}
              :attrs {:style {:stroke-width 2
                              :vector-effect "non-scaling-stroke"}}
              :script
              [[:circle {:r 15 :cx 0   :cy  0 :fill "none" :stroke "yellow"}]
               [:circle {:r 2  :cx -10.5 :cy  10.5 :fill "red" :stroke "none"}]
               [:circle {:r 2  :cx 0   :cy -15 :fill "red" :stroke "none"}]
               [:circle {:r 2  :cx 10.5  :cy  10.5 :fill "red" :stroke "none"}]]}}},
   :script
   [[:circle {:cx 50 :cy 50 :r 10 :fill "red"}]
    [:ref {} "reps"]

    [:path
     {:doc "red curve 1"
      :attrs {:style {:stroke "red" :fill "none"}}
      :mirror {:mode :separate
               :axis 0}}
     [:ref {} "red-curve"]]

    [:path
     {:doc "red curve 2",
      :attrs {:style {:stroke "red", :fill "none"}},
      :mirror {:mode :separate, :axis 1}}
     [:M [5 39]]
     [:C [17 29] [17 68] [44 57]]]

    [:ref {:doc "cascading tfs"
           :repeat {:tfs+ [{:tl [50 50] :sc 1.2}
                           {:tl [10 10] :sc 1.2 :rt 15}]}}
     "circs"]]})
