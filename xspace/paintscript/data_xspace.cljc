 (ns paintscript.data-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.data :as d]))

(def data-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [=> op cmpt]}
            (merge (-> ctx :args) args)]
        (is (= =>
               (case op
                 :data/parse-cmpt (d/parse-cmpt cmpt)
                 )))))}})

(defn l+
  [locr & args]
  (apply update locr :el-pthv conj args))

(defn ll
  ([cmpt-pthv el-pthv]
   (-> d/locr-root
       (assoc :cmpt-pthv cmpt-pthv
              :el-pthv  el-pthv)))
  ([el-pthv]
   (-> d/locr-root
       (assoc :el-pthv el-pthv))))

(def data-xspace
  [(xx {:= {:op :data/parse-cmpt}}

       (x-> :cmpt {:defs   {:components
                            {"c1" {:script [[:path {} [:A [5 10] [0 0 1] [20 20]]]]}}}
                   :script [[:path {} [:M [10 10]] [:Q [5 5] [15 15]]]]}
            :=>   {:defs
                   {:components
                    {"c1" {:script
                           [(d/elem :locr    (ll ["c1"] [:script 0])
                                    :el-src  [:path {} [:A [5 10] [0 0 1] [20 20]]]
                                    :el-k    :path
                                    :el-opts {}
                                    :el-argv
                                    [(d/elem :locr    (ll ["c1"] [:script  0
                                                                  :el-argv 0])
                                             :el-src  [:A [5 10] [0 0 1] [20 20]]
                                             :el-k    :A
                                             :el-argv
                                             [[5 10]
                                              [0 0 1]
                                              (d/pnt :locr  (ll ["c1"] [:script  0
                                                                        :el-argv 0
                                                                        :el-argv 2])
                                                     :xy    [20 20])])])]}}}
                   :script
                   [(d/elem :locr    (ll [:script 0])
                            :el-src  [:path {} [:M [10 10]] [:Q [5 5] [15 15]]]
                            :el-k    :path
                            :el-opts {}
                            :el-argv
                            [(d/elem :locr    (ll [:script  0
                                                   :el-argv 0])
                                     :el-src  [:M [10 10]]
                                     :el-k    :M
                                     :el-argv
                                     [(d/pnt :locr  (ll [:script  0
                                                         :el-argv 0
                                                         :el-argv 0])
                                             :pnt-k :pnt/main
                                             :xy    [10 10])])

                             (d/elem :locr    (ll [:script  0
                                                   :el-argv 1])
                                     :el-src  [:Q [5 5] [15 15]]
                                     :el-k    :Q
                                     :el-argv
                                     [(d/pnt :locr  (ll [:script  0
                                                         :el-argv 1
                                                         :el-argv 0])
                                             :pnt-k :pnt/cp
                                             :xy    [5 5]
                                             :pnt-i-main 1)
                                      (d/pnt :locr  (ll [:script  0
                                                         :el-argv 1
                                                         :el-argv 1])
                                             :pnt-k :pnt/main
                                             :xy    [15 15])])])]}))])

(deftest data-test
  (x/traverse-xspace data-xspace-cfg
                     data-xspace))
