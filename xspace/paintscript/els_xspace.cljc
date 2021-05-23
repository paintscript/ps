 (ns paintscript.els-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.els :as els]
            [paintscript.render-svg :as render-svg]))

(def els-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [=> op arcs opts path]}
            (merge (-> ctx :args) args)]
        (is (= =>
               (case op
                 :els/normalize-els  (#'els/normalize-els path)
                 :els/reverse-el-xys (#'els/reverse-el-xys path)
                 ; :mirror-xys     (#'els/mirror-xys width xys)
                 ; :scale-els      (#'els/scale-els path center factor)
                 )))))}})

(def els-xspace
  [(xx {:= {:op :els/normalize-els}}

       (x-> "C1"
            :path '([:C1 [10 10] [20 20]])
            :=>   '([:C  [10 10] [20 20] [20 20]]))

       (x-> "S"
            :path '([:C [0 0] [10 15] [20 25]]
                    [:S [25 25] [35 35]])
            :=>   '([:C [0 0] [10 15] [20 25]]
                    [:C [30 35] [25 25] [35 35]])))

   (xx {:= {:op :els/reverse-el-xys}}

       (x-> :path '([:M 1] [:L 2] [:C 3 4 5] [:C 6 7 8])
            :=>   '([:C 7 6 5] [:C 4 3 2] [:L 1]))

       (x-> :path '([:M 1] [:L 2] )
            :=>   '([:L 1]))

       (x-> :path '([:M 1] [:C 2 3 4] )
            :=>   '([:C 3 2 1])))])

(deftest els-test
  (x/traverse-xspace els-xspace-cfg
                     els-xspace))
