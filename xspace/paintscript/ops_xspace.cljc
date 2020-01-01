(ns paintscript.ops-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [paintscript.ops :as ops]
            [xspace.core :as x :refer [x-> xx x:=]]))

(def ops-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op script pth-vecs pnt ii =>]}
            (merge (-> ctx :args) args)]
        (is (= =>
               (case op
                 :append-pnt (if pnt
                               (apply ops/append-pnt pth-vecs (concat ii [pnt]))
                               (apply ops/append-pnt pth-vecs ii))
                 :del-pnt (apply ops/del-pnt pth-vecs ii)
                 :append-pth-vec (apply ops/append-pth-vec pth-vecs ii)
                 :del-pth-vec (apply ops/del-pth-vec pth-vecs ii)
                 :append-pth (apply ops/append-pth script ii)
                 :del-pth (apply ops/del-pth script ii))

               ))))}})

(def ops-xspace
  [;; pnt
   (xx {:= {:op :append-pnt}}

       (xx {:title "infer pnt (|2|)"
            := {:pth-vecs [[:L [0 0] [5 5]]]
                :=>       [[:L [0 0] [5 5] [10 10]]]}}

           (x-> "provide pnt"   :ii [0 1] :pnt [10 10])
           (x-> "infer pnt"     :ii [0 1])
           (x-> "infer pnt & i" :ii [0]))

       (x-> "infer pnt (|1| w/ preceding pth-vec)"
            :pth-vecs [[:M [0 0]] [:L [5 5]]]
            :ii       [1 0]
            :=>       [[:M [0 0]] [:L [5 5] [10 10]]])

       (x-> "infer pnt (|1| w/o preceding pth-vec)"
            :pth-vecs [[:L [5 5]]]
            :ii       [0 0]
            :=>       [[:L [5 5] [10 10]]]))

   (xx {:= {:op :del-pnt}}

       (x-> :pth-vecs [[:L [0 0] [5 5] [10 10]]]
            :ii       [0 1]
            :=>       [[:L [0 0] [10 10]]]))

   ;; pth-vec
   (xx {:= {:op :append-pth-vec}}

       (x-> :pth-vecs [[:L [0 0] [5 5]]]
            :ii       [0]
            :=>       [[:L [0 0] [5 5]] [:L [15 15]]]))

   (xx {:= {:op :del-pth-vec}}

       (x-> :pth-vecs [[:L [0 0] [5 5]] [:L [15 15]]]
            :ii       [0]
            :=>       [[:L [15 15]]]))

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
            :=>     [[:path {} [:M [5 5]]]]))])

(deftest ops-test
  (x/traverse-xspace ops-xspace-cfg
                     ops-xspace))
