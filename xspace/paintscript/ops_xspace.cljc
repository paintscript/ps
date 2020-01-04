(ns paintscript.ops-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [paintscript.ops :as ops]
            [xspace.core :as x :refer [x-> xx x:=]]))

(def ops-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op params script els pnt ii tl =>]}
            (merge (-> ctx :args) args)]
        (is (= =>
               (case op
                 :append-pnt (if pnt
                               (apply ops/append-pnt els (concat ii [pnt]))
                               (apply ops/append-pnt els ii))
                 :del-pnt    (apply ops/del-pnt    els ii)
                 :append-el  (apply ops/append-el  els ii)
                 :del-el     (apply ops/del-el     els ii)
                 :append-pth (apply ops/append-pth script ii)
                 :del-pth    (apply ops/del-pth    script ii)
                 :tl-pth     (ops/tl-pth params ii tl))))))}})

(def ops-xspace
  [;; pnt
   (xx {:= {:op :append-pnt}}

       (xx {:title "infer pnt (|2|)"
            := {:els [[:L [0 0] [5 5]]]
                :=>  [[:L [0 0] [5 5] [10 10]]]}}

           (x-> "provide pnt"   :ii [0 1] :pnt [10 10])
           (x-> "infer pnt"     :ii [0 1])
           (x-> "infer pnt & i" :ii [0]))

       (x-> "infer pnt (|1| w/ preceding el)"
            :els [[:M [0 0]] [:L [5 5]]]
            :ii  [1 0]
            :=>  [[:M [0 0]] [:L [5 5] [10 10]]])

       (x-> "infer pnt (|1| w/o preceding el)"
            :els [[:L [5 5]]]
            :ii  [0 0]
            :=>  [[:L [5 5] [10 10]]]))

   (xx {:= {:op :del-pnt}}

       (x-> :els [[:L [0 0] [5 5] [10 10]]]
            :ii  [0 1]
            :=>  [[:L [0 0] [10 10]]]))

   ;; el
   (xx {:= {:op :append-el}}

       (x-> :els [[:L [0 0] [5 5]]]
            :ii  [0]
            :=>  [[:L [0 0] [5 5]] [:L [15 15]]]))

   (xx {:= {:op :del-el}}

       (x-> :els [[:L [0 0] [5 5]] [:L [15 15]]]
            :ii  [0]
            :=>  [[:L [15 15]]]))

   ;; pth
   (xx {:= {:op :append-pth}}

       (x-> :script [[:path {} [:M [5 5]]]]
            :ii     [0]
            :=>     [[:path {} [:M [5 5]]]
                     [:path {} [:M [10 10]]]]))

   (xx {:= {:op :del-pth}}

       (x-> :script [[:path {} [:M [5 5]]]
                     [:path {} [:M [10 10]]]]
            :ii     [1]
            :=>     [[:path {} [:M [5 5]]]]))

   (xx {:= {:op :tl-pth}}

       (x-> :params {:script
                     [[:path {} [:M [5 5]]]
                      [:path {} [:M [10 10]]]]}
            :ii     [:script 0]
            :tl     [1 2]
            :=>     {:script
                     [[:path {} [:M [6 7]]]
                      [:path {} [:M [10 10]]]]}))])

(deftest ops-test
  (x/traverse-xspace ops-xspace-cfg
                     ops-xspace))
