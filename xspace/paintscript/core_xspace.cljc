(ns paintscript.core-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [paintscript.core :as core]
            [xspace.core :as x :refer [x-> xx x:=]]))

(def core-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op opts-base opts arcs pnts width path =>]}
            (merge (-> ctx :args) args)]
        (let [opts' (merge opts-base opts)]
          (is (= =>
                 (case op
                   :path                 (core/path opts' path)
                   :arcs                 (#'core/arcs arcs opts)
                   :mirror-pnts          (#'core/mirror-pnts width pnts)
                   :normalize-curves     (#'core/normalize-curves path)
                   :reverse-pth-vec-pnts (#'core/reverse-pth-vec-pnts path)))))))}})

(def core-xspace
  [(xx "util"

       (xx {:= {:op :arcs}}

           (x-> :arcs '([0 0] [10 10] [20 0])
                :=>   '(["M" [0 0]]
                        ["A" [10  10] [0 "0,1"] [10 10]]
                        ["A" [10 -10] [0 "0,1"] [20  0]])))

       (xx {:= {:op :normalize-curves}}

           (x-> "C1"
                :path '([:curve-C1 [10 10] [20 20]])
                :=>   '([:curve-C  [10 10] [20 20] [20 20]]))

           (x-> "S"
                :path '([:curve-C [0 0] [10 15] [20 25]]
                        [:curve-S [25 25] [35 35]])
                :=>   '([:curve-C [0 0] [10 15] [20 25]]
                        [:curve-C [30 35] [25 25] [35 35]])))

       (xx {:= {:op :reverse-pth-vec-pnts}}

           (x-> :path '([:line 1 2] [:curve-C 3 4 5] [:curve-C 6 7 8])
                :=>   '([:curve-C 7 6 5] [:curve-C 4 3 2] [:line* 1]))))

   (xx {:= {:op :path}}

       (x:= :view-opts
            {:debug? true :coord-size 10
             :attrs {:stroke "red" :stroke-width 2 :fill "none"}})

       (xx "lines"

           (x-> :path [[:line [0 0] [30 30]]]
                :=>   (list "M" 0 0 30 30))

           (xx {:= {:opts {:mirror :separate}}}

               (x-> :path [[:line [0 0] [30 30]]]
                    :=>   '("M" 0 0 30 30 "M" 100 0 70 30)))

           (xx {:= {:opts {:mirror :merged}}}

               (x-> :path [[:line [0 0] [30 30]]]
                    :=>   '("M" 0 0 30 30 "L" 70 30 100 0))

               (x-> :path [[:line [0 0] [50 50]]]
                    :=>   '("M" 0 0 50 50 "L" 50 50 100 0))))

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

           (xx {:= {:path [[:line [10 10]]
                           [:curve-A [40 20] [0 0 1] [50 30]]]}}

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
                           "A" 40 20 0 0 1 90 10]))))])

(deftest core-test
  (x/traverse-xspace core-xspace-cfg
                     core-xspace))
