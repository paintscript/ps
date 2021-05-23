 (ns paintscript.util-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.util :as util]))

(def util-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [=> op ctr pnt alpha rad dist opts arcs]}
            (merge (-> ctx :args) args)]
        (is (= =>
               (case op
                 :util/angle-between   (#'util/angle-between   ctr pnt)
                 :util/angle-delta     (#'util/angle-delta     ctr pnt)
                 :util/rad-between     (#'util/rad-between     ctr pnt)
                 :util/point-at-angle  (#'util/point-at-angle  ctr rad alpha)
                 :util/tl-point-around (#'util/tl-point-around ctr pnt alpha)
                 :util/tl-point-along  (#'util/tl-point-along  pnt alpha dist))))))}})

(def util-xspace
  [(xx {:= {:op  :util/angle-delta
            :ctr [50 50]}}

       (x-> :pnt [60 40] :=> -45.0) ;; ↘
       (x-> :pnt [40 40] :=>  45.0) ;; ↗
       (x-> :pnt [40 60] :=> -45.0) ;; ↘
       (x-> :pnt [60 60] :=>  45.0) ;; ↗

       (x-> :pnt [70 50] :=>   0.0) ;; →
       (x-> :pnt [50 30] :=> -90)   ;; ↓
       (x-> :pnt [30 50] :=>   0.0) ;; →
       (x-> :pnt [50 70] :=>  90))  ;; ↑


   (xx {:= {:op  :util/angle-between
            :ctr [50 50]}}

       (x-> :pnt [60 40] :=>  45.0)  ;; ↗
       (x-> :pnt [40 40] :=> 135.0)  ;; ↖
       (x-> :pnt [40 60] :=> 225.0)  ;; ↙
       (x-> :pnt [60 60] :=> -45.0)  ;; ↘

       (x-> :pnt [70 50] :=>     0)  ;; →
       (x-> :pnt [50 30] :=>    90)  ;; ↑
       (x-> :pnt [30 50] :=>   180)  ;; ←
       (x-> :pnt [50 70] :=>   270)) ;; ↓

   (xx {:= {:op :util/tl-point-along
            :pnt [50 50]
            :dist 20}}

       (x-> :alpha 0   :=> [70.0 50.0])  ;; →
       (x-> :alpha 90  :=> [50.0 70.0])  ;; ↓
       (x-> :alpha 180 :=> [30.0 50.0])  ;; ←
       (x-> :alpha -90 :=> [50.0 30.0])) ;; ↑

   (let [p Math/PI]
     (xx {:= {:op  :util/rad-between
              :ctr [50 50]}}

         (x-> :pnt [60 40] :=>  (* p (/ 1 4)))
         (x-> :pnt [40 40] :=>  (* p (/ 3 4)))
         (x-> :pnt [40 60] :=>  (* p (/ 5 4)))
         (x-> :pnt [60 60] :=>  (- (* p (/ 1 4))))

         (x-> :pnt [70 50] :=> 0.0)
         (x-> :pnt [50 30] :=> (* p (/ 1 2)))
         (x-> :pnt [30 50] :=>    p)
         (x-> :pnt [50 70] :=> (* p (/ 6 4)))))

   (xx {:= {:op :util/point-at-angle
            :ctr [50 50]
            :rad 20}}

       (x-> :alpha 0   :=> [70.0 50.0])
       (x-> :alpha 90  :=> [50.0 30.0])
       (x-> :alpha 180 :=> [30.0 50.0])
       (x-> :alpha -90 :=> [50.0 70.0]))

   (xx {:= {:op :util/tl-point-around
            :ctr [50 50]
            :pnt [70 50]}}

       (x-> :alpha  90 :=> [50.0 30.0])
       (x-> :alpha 180 :=> [30.0 50.0])
       (x-> :alpha -90 :=> [50.0 70.0]))])

(deftest util-test
  (x/traverse-xspace util-xspace-cfg
                     util-xspace))
