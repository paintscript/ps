(ns paintscript.ops-xspace
  (:require [clojure.test :refer [testing is]]
            [xspace.core :refer [x-> xx x:=]]
            [paintscript.nav :as nav]
            [paintscript.data :as data]

            [paintscript.ops.ops-path-tf :as ops-path-tf]
            [paintscript.ops.ops-cmpt :as ops-cmpt]))

(def ops-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op cmpt script path drop? els pnt ii i to tl =>] :or {drop? true}}
            (merge (-> ctx :args) args)

            cmpt   (some->> cmpt   data/parse-cmpt)
            script (some->> script (mapv #(#'data/elvv->rr data/locr-root %)))
            els    (some->> els    (mapv data/elv->r))
            path   (some->> path   (mapv data/elv->r))]
        (is (= =>
               (case op
                 ;; --- ops-path-tf
                 :ops/normalize-p-els  (->> path
                                            (#'ops-path-tf/normalize-pcmds)
                                            (mapv data/elr->v))

                 :ops/reverse-p-el-xys (->> path
                                            (#'ops-path-tf/reverse-p-el-xys
                                              {:drop-last? drop?})
                                            (mapv data/elr->v))

                 :ops/append-pnt   (->> (if pnt
                                          (apply ops-path-tf/append-pnt els (concat ii [pnt]))
                                          (apply ops-path-tf/append-pnt els ii))
                                        (mapv data/elr->v))
                 :ops/del-pnt      (->> (apply ops-path-tf/del-pnt     els ii)
                                        (mapv data/elr->v))
                 :ops/append-p-el  (->> (ops-path-tf/append-p-el els ii)
                                        (mapv data/elr->v))
                 :ops/del-el       (->> (apply ops-path-tf/del-el      els ii)
                                        (mapv data/elr->v))

                 :ops/change-pcmd-k (->> (ops-path-tf/change-pcmd-k els i to)
                                        (mapv data/elr->v))

                 ;; --- ops-cmpt
                 :ops/append-pth   (->> (apply ops-cmpt/append-pth script ii)
                                        (mapv #'data/elrr->vv))
                 :ops/del-pth      (->> (apply ops-cmpt/del-pth    script ii)
                                        (mapv #'data/elrr->vv))

                 :ops/tl-pth       (-> (ops-cmpt/translate cmpt (nav/nav-vec->rec ii) tl)
                                       data/serialize-cmpt)
                 :ops/rel->abs     (-> (ops-cmpt/absolute cmpt)
                                       data/serialize-cmpt))))))}})

(def ops-xspace
  [(xx {:= {:op :ops/normalize-p-els}}

       (x-> "C1"
            :path '([:C1 [10 10] [20 20]])
            :=>   '([:C  [10 10] [20 20] [20 20]]))

       (x-> "S"
            :path '([:C [0 0] [10 15] [20 25]]
                    [:S [25 25] [35 35]])
            :=>   '([:C [0 0] [10 15] [20 25]]
                    [:C [30 35] [25 25] [35 35]])))

   (xx {:= {:op :ops/reverse-p-el-xys}}

       (xx {:= {:path '([:M 1] [:L 2])}}

           (x-> :drop? true  :=> '([:L 1]))
           (x-> :drop? false :=> '([:M 2] [:L 1])))

       (xx {:= {:path '([:M 1] [:C 2 3 4])}}

           (x-> :drop? true  :=> '([:C 3 2 1]))
           (x-> :drop? false :=> '([:M 4] [:C 3 2 1])))

       (xx {:= {:path '([:M 1] [:L 2] [:C 3 4 5] [:C 6 7 8])}}

           (x-> :drop? true  :=> '([:C 7 6 5] [:C 4 3 2] [:L 1]))
           (x-> :drop? false :=> '([:M 8] [:C 7 6 5] [:C 4 3 2] [:L 1]))))


   ;; pnt
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
            :ii  0
            :=>  [[:L [0 0] [5 5]] [:L [15 15]]]))

   (xx {:= {:op :ops/del-el}}

       (x-> :els [[:L [0 0] [5 5]] [:L [15 15]]]
            :ii  [0]
            :=>  [[:L [15 15]]]))

   (xx {:= {:op :ops/change-pcmd-k}}

       (x-> :els [[:M [0 0]] [:L [5 5]]]
            :i   1
            :to  :C
            :=>  [[:M [0 0]] [:C [0 0] [5 5] [5 5]]])

       (x-> :els [[:M [0 0]] [:C [0 0] [5 5] [5 5]]]
            :i   1
            :to  :L
            :=>  [[:M [0 0]] [:L [5 5]]]))

   ; ;; pth
   (xx {:= {:op :ops/append-pth}}

       (x-> :script [[:path [:M [5 5]]]]
            :ii     [0]
            :=>     [[:path [:M [5 5]]]
                     [:path [:M [10 10]]]]))

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
