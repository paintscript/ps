(ns paintscript.ops-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [paintscript.ops :as ops]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.nav :as nav]))

(def ops-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op cmpt script els pnt ii i to tl =>]}
            (merge (-> ctx :args) args)]
        (let []
          (is (= =>
                 (case op
                   :ops/append-pnt   (if pnt
                                       (apply ops/append-pnt els (concat ii [pnt]))
                                       (apply ops/append-pnt els ii))
                   :ops/del-pnt      (apply ops/del-pnt    els ii)
                   :ops/append-p-el    (apply ops/append-p-el  els ii)
                   :ops/del-el       (apply ops/del-el     els ii)
                   :ops/append-pth   (apply ops/append-pth script ii)
                   :ops/del-pth      (apply ops/del-pth    script ii)
                   :ops/tl-pth       (ops/translate cmpt (nav/pth-vec->rec ii) tl)
                   :ops/rel->abs     (ops/absolute cmpt)
                   :ops/transform-el (ops/transform-el els i to)))))))}})

(def ops-xspace
  [;; pnt
   (xx {:= {:op :ops/append-pnt}}

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

   (xx {:= {:op :ops/del-pnt}}

       (x-> :els [[:L [0 0] [5 5] [10 10]]]
            :ii  [0 1]
            :=>  [[:L [0 0] [10 10]]]))

   ;; el
   (xx {:= {:op :ops/append-p-el}}

       (x-> :els [[:L [0 0] [5 5]]]
            :ii  [0]
            :=>  [[:L [0 0] [5 5]] [:L [15 15]]]))

   (xx {:= {:op :ops/del-el}}

       (x-> :els [[:L [0 0] [5 5]] [:L [15 15]]]
            :ii  [0]
            :=>  [[:L [15 15]]]))

   (xx {:= {:op :ops/transform-el}}

       (x-> :els [[:M [0 0]] [:L [5 5]]]
            :i   1
            :to  :C
            :=>  [[:M [0 0]] [:C [0 0] [5 5] [5 5]]])

       (x-> :els [[:M [0 0]] [:C [0 0] [5 5] [5 5]]]
            :i   1
            :to  :L
            :=>  [[:M [0 0]] [:L [5 5]]]))

   ;; pth
   (xx {:= {:op :ops/append-pth}}

       (x-> :script [[:path {} [:M [5 5]]]]
            :ii     [0]
            :=>     [[:path {} [:M [5 5]]]
                     [:path {} [:M [10 10]]]]))

   (xx {:= {:op :ops/del-pth}}

       (x-> :script [[:path {} [:M [5 5]]]
                     [:path {} [:M [10 10]]]]
            :ii     [1]
            :=>     [[:path {} [:M [5 5]]]]))

   (xx {:= {:op :ops/tl-pth}}

       (x-> :cmpt {:script
                   [[:path {} [:M [5 5]]]
                    [:path {} [:M [10 10]]]]}
            :ii   [:script 0]
            :tl   [1 2]
            :=>   {:script
                   [[:path {} [:M [6 7]]]
                    [:path {} [:M [10 10]]]]}))

   (xx {:= {:op :ops/rel->abs}}

       (x-> "l"
            :cmpt {:script
                   [[:path {} [:m [5 5]] [:l [5 5]]]
                    [:path {} [:m [10 10]]]]}
            :=>   {:defs {}
                   :script
                   [[:path {} [:M [5 5]] [:L [10 10]]]
                    [:path {} [:M [10 10]]]]})

       (x-> "a"
            :cmpt {:script
                   [[:path {} [:M [5 5]] [:a [5 5] [0 0 1] [7 7]]]]}
            :=>   {:defs {}
                   :script
                   [[:path {} [:M [5 5]] [:A [5 5] [0 0 1] [12 12]]]]}))])

(deftest ops-test
  (x/traverse-xspace ops-xspace-cfg
                     ops-xspace))
