 (ns paintscript.nav-xspace
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x :refer [x-> xx x:=]]
            [paintscript.nav :as nav]))

(def nav-xspace-cfg
  {:fns
   {:xx (fn [_ctx {:keys [title]} f] (testing title (f)))
    :x->
    (fn [ctx c args]
      (let [{:keys [op => cmpt ref-pth]}
            (merge (-> ctx :args) args)]
        (is (= =>
               (case op
                 :nav/ref-pth->cmpt-pth  (#'nav/ref-pth->cmpt-pth cmpt ref-pth))))))}})

(def nav-xspace
  [(xx {:= {:op :nav/ref-pth->cmpt-pth}}

       (xx {:= {:cmpt
                {:script [[:path {} [:M [1 1]]]
                          [:ref {:translate [0 0]} "A"]]
                 :defs   {:components
                          {"A" {:script [[:ref {} "B"]
                                         [:ref {} "C"]]
                                :defs   {:components
                                         {"C" {:script [[:path {} [:M [10 10]]]]}}}}
                           "B" {:script [[:path {} [:M [10 10]]]]}}}}}}

           (x-> "ref-pth ~= cmpt-pth"

                :ref-pth [{:cmpt-id "A"} {:cmpt-id "C"}]
                :=>      ["A" "C"])

           (x-> "ref-pth != cmpt-pth"

                :ref-pth [{:cmpt-id "A"} {:cmpt-id "B"}]
                :=>      ["B"])))])

(deftest nav-test
  (x/traverse-xspace nav-xspace-cfg
                     nav-xspace))
