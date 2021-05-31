 (ns paintscript.els-xspace
  (:require [clojure.test :refer [testing is]]
            [xspace.core :refer [x-> xx x:=]]
            [paintscript.data :as data]
            [paintscript.data-ops :as data-ops]))

(def els-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [=> op arcs opts path drop?] :or {drop? true}}
            (merge (-> ctx :args) args)]
        (let [path' (->> path
                         (mapv data/elv->r))]
          (is (= =>
                 (->> (case op
                        :els/normalize-p-els  (->> path'
                                                   (#'data-ops/normalize-pcmd-seq))

                        :els/reverse-p-el-xys (->> path'
                                                   (#'data-ops/reverse-p-el-xys
                                                     {:drop-last? drop?})))
                      (mapv data/elr->v)))))))}})

(def els-xspace
  [(xx {:= {:op :els/normalize-p-els}}

       (x-> "C1"
            :path '([:C1 [10 10] [20 20]])
            :=>   '([:C  [10 10] [20 20] [20 20]]))

       (x-> "S"
            :path '([:C [0 0] [10 15] [20 25]]
                    [:S [25 25] [35 35]])
            :=>   '([:C [0 0] [10 15] [20 25]]
                    [:C [30 35] [25 25] [35 35]])))

   (xx {:= {:op :els/reverse-p-el-xys}}

       (xx {:= {:path '([:M 1] [:L 2])}}

           (x-> :drop? true  :=> '([:L 1]))
           (x-> :drop? false :=> '([:M 2] [:L 1])))

       (xx {:= {:path '([:M 1] [:C 2 3 4])}}

           (x-> :drop? true  :=> '([:C 3 2 1]))
           (x-> :drop? false :=> '([:M 4] [:C 3 2 1])))

       (xx {:= {:path '([:M 1] [:L 2] [:C 3 4 5] [:C 6 7 8])}}

           (x-> :drop? true  :=> '([:C 7 6 5] [:C 4 3 2] [:L 1]))
           (x-> :drop? false :=> '([:M 8] [:C 7 6 5] [:C 4 3 2] [:L 1]))))])
