 (ns paintscript.data-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.data :as d]))

(def data-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [=> op cmpt elv]}
            (merge (-> ctx :args) args)]
        (case op
          :data/parse-cmpt (is (= => (d/parse-cmpt cmpt)))
          :data/vec<->rec  (let [elr  (d/elv->r elv)
                                 elv' (d/elr->v elr)]
                             (is (= =>  elr))
                             (is (= elv elv'))))))}})

(defn ll
  ([cmpt-pthv
    el-pthv] (-> d/locr-root (assoc :cmpt-pthv cmpt-pthv
                                    :el-pthv   el-pthv)))
  ([el-pthv] (-> d/locr-root (assoc :el-pthv   el-pthv))))

(def data-xspace
  [(xx {:= {:op :data/vec<->rec}}

       (x-> "path command"
            :elv [:M [10 10]]
            :=>  (d/elem :el-k    :M
                         :el-src  [:M [10 10]]
                         :el-argv [[10 10]]))

       (x-> "path command w/ opts"
            :elv [:M {:k 42} [10 10]]
            :=>  (d/elem :el-k    :M
                         :el-opts {:k 42}
                         :el-src  [:M {:k 42} [10 10]]
                         :el-argv [[10 10]])))

   (xx {:= {:op :data/parse-cmpt}}

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
                                             :el-argv [[5 10] [0 0 1] [20 20]])])]}}}
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
                                     :el-argv [[10 10]])

                             (d/elem :locr    (ll [:script  0
                                                   :el-argv 1])
                                     :el-src  [:Q [5 5] [15 15]]
                                     :el-k    :Q
                                     :el-argv [[5 5] [15 15]])])]}))])
