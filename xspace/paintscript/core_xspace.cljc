 (ns paintscript.core-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.els :as els]
            [paintscript.core :as core]))

(def core-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op params opts-base opts arcs xys center fract width path =>]}
            (merge (-> ctx :args) args)]
        (let [opts' (merge opts-base opts)]
          (is (= =>
                 (case op
                   :path           (els/path opts' path)
                   :arcs           (#'els/arcs arcs opts)
                   :mirror-xys     (#'els/mirror-xys width xys)
                   :normalize-els  (#'els/normalize-els path)
                   :reverse-el-xys (#'els/reverse-el-xys path)
                   :scale-els      (els/scale-els path center fract)
                   :paint          (core/paint params)))))))}})

(def core-xspace
  [(xx "util"

       (xx {:= {:op :arcs}}

           (x-> :arcs '([0 0] [10 10] [20 0])
                :=>   '(["M" [0 0]]
                        ["A" [10  10] [0 "0,1"] [10 10]]
                        ["A" [10 -10] [0 "0,1"] [20  0]])))

       (xx {:= {:op :normalize-els}}

           (x-> "C1"
                :path '([:C1 [10 10] [20 20]])
                :=>   '([:C  [10 10] [20 20] [20 20]]))

           (x-> "S"
                :path '([:C [0 0] [10 15] [20 25]]
                        [:S [25 25] [35 35]])
                :=>   '([:C [0 0] [10 15] [20 25]]
                        [:C [30 35] [25 25] [35 35]])))

       (xx {:= {:op :reverse-el-xys}}

           (x-> :path '([:M 1] [:L 2] [:C 3 4 5] [:C 6 7 8])
                :=>   '([:C 7 6 5] [:C 4 3 2] [:L 1]))

           (x-> :path '([:M 1] [:L 2] )
                :=>   '([:L 1]))

           (x-> :path '([:M 1] [:C 2 3 4] )
                :=>   '([:C 3 2 1]))))

   (xx {:= {:op :path}}

       (x:= :view-opts
            {:debug? false :coord-size 10
             :attrs {:stroke "red" :stroke-width 2 :fill "none"}})

       (xx "lines"

           (x-> :path [[:M [0 0]] [:L [30 30]]]
                :=>   (list "M" 0 0 "L" 30 30))

           (x-> :path [[:M [0 0]] [:L [30 30]] [:z]]
                :=>   (list "M" 0 0 "L" 30 30 "z"))

           (xx {:= {:opts {:mirror :separate}}}

               (x-> :path [[:M [0 0]] [:L [30 30]]]
                    :=>   '("M" 0 0 "L" 30 30 "M" 100 0 "L" 70 30))

               (x-> :path [[:M [0  0]] [:L [30 30]] [:z]
                           [:M [0 50]] [:L [30 80]] [:z]]
                    :=>   '("M" 0  0 "L" 30 30 "M" 100  0 "L" 70 30 "z"
                            "M" 0 50 "L" 30 80 "M" 100 50 "L" 70 80 "z")))

           (xx {:= {:opts {:mirror :merged}}}

               (x-> :path [[:M [0 0]] [:L [30 30]]]
                    :=>   '("M" 0 0 "L" 30 30 "L" 100 0))

               (x-> :path [[:M [0 0]] [:L [50 50]]]
                    :=>   '("M" 0 0 "L" 50 50 "L" 100 0))))

       (xx "arcs"

           (xx {:= {:path [[:arc [0 0] [30 30]]]}}

               (x-> :=>   '("M" 0 0 "A" 30 30 0 "0,1" 30 30))

               (x-> :opts {:mirror :separate}
                    :=>   '("M" 0   0 "A"  30 30 0 "0,1" 30 30
                            "M" 100 0 "A" -30 30 0 "0,0" 70 30)))

           (xx {:= {:path [[:arc [10 10] [50 30]]]}}

               (x-> :=>   '("M" 10 10 "A" 40 20 0 "0,1" 50 30))

               (x-> :opts {:mirror :separate}
                    :=>   '("M" 10 10 "A"  40 20 0 "0,1" 50 30
                            "M" 90 10 "A" -40 20 0 "0,0" 50 30)))

           (xx {:= {:path [[:M [10 10]]
                           [:A [40 20] [0 0 1] [50 30]]]}}

               (x-> :=>   '("M" 10 10 "A"  40 20 0 0 1 50 30))

               (x-> :opts {:mirror :separate}
                    :=>   '("M" 10 10 "A" 40 20 0 0 1 50 30
                            "M" 90 10 "A" 40 20 0 0 0 50 30))

               (x-> :opts {:mirror :merged}
                    :view-opts
                    {:debug? false :coord-size 10
                     :attrs {:stroke "red" :stroke-width 2 :fill "none"}}
                    :=>   ["M" 10 10
                           "A" 40 20 0 0 1 50 30
                           "A" 40 20 0 0 1 90 10])))

       (xx "scale"

           (x:= :view-opts {:debug? false
                            :attrs {:stroke "red" :stroke-width 2 :fill "none"}})

           (xx {:= {:path [[:M [ 0  0]]
                           [:L
                            [15  0] [30  0]
                            [30 15] [30 30] [15 30]
                            [ 0 30] [ 0 15] [ 0 0]]]}}

               (x-> :=>   '("M" 0 0 "L" 15 0 30 0 30 15 30 30 15 30 0 30 0 15 0 0))

               (x-> :opts {:scale [[15 15] (/ 1 2)]
                           :mirror :separate}
                    :=>   '("M" 6.792893218813453 6.792893218813452 "L" 15.0 6.5 23.207106781186546 6.792893218813452 23.5 15.0 23.207106781186546 23.207106781186546 15.0 23.5 6.792893218813453 23.207106781186546 6.5 15.0 6.792893218813453 6.792893218813452 "M" 93.20710678118655 6.792893218813452 "L" 85.0 6.5 76.79289321881345 6.792893218813452 76.5 15.0 76.79289321881345 23.207106781186546 85.0 23.5 93.20710678118655 23.207106781186546 93.5 15.0 93.20710678118655 6.792893218813452)))

           (xx {:= {:path [[:M [10 10]]
                           [:C [5 30] [40 20] [40 40]]
                           [:C [40 50] [20 40] [20 60]]]}}

               (x-> :=>   '("M" 10 10 "C" 5 30 40 20 40 40 "C" 40 50 20 40 20 60))
               (x-> :opts {:scale [[30 40] (/ -3 2)]
                           :mirror :separate}

                    :=>   '("M" 59.44529980377477 84.16794970566215 "C" 66.57152330911474 54.6286093236459 15.44721359549995 69.10557280900008 16.0 40.0 "C" 15.707106781186546 25.70710678118655 44.0 40.0 44.552786404500054 10.894427190999913 "M" 40.55470019622523 84.16794970566215 "C" 33.42847669088526 54.6286093236459 84.55278640450005 69.10557280900008 84.0 40.0 "C" 84.29289321881345 25.70710678118655 56.0 40.0 55.447213595499946 10.894427190999913)))

           (xx {:= {:path [[:M [10 10]]
                           [:A [10 5] [0 0 1] [30 30]]]}}

               (x-> :=>   '("M" 10 10 "A" 10 5 0 0 1 30 30))
               (x-> :opts {:scale [[15 15] (/ 1 2)]
                           :mirror :separate}

                    :=>   '("M" 11.792893218813452 11.792893218813452 "A" 10 5 0 0 1 23.207106781186546 23.207106781186546 "M" 88.20710678118655 11.792893218813452 "A" 10 5 0 0 0 76.79289321881345 23.207106781186546))))

       (xx {:= {:op :paint}}

           (x-> :params {:script [[:path {} [:M [0 0]]]]}
                :=>     '[:g ([:g [:path {:d "M 0 0"}]])])

           (x-> :params {:script [[:circle {:cx 10 :cy 10 :r 10}]]}
                :=>     '[:g ([:circle {:cx 10 :cy 10 :r 10}])])))])

(deftest core-test
  (x/traverse-xspace core-xspace-cfg
                     core-xspace))
