 (ns paintscript.nav-xspace
  (:require [clojure.test :refer [testing is]]
            [xspace.core :refer [x-> xx x:=]]
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

       (xx {:= {:cmpt {:defs
                       {:components
                        {"A" {}
                         "B" {:defs {:components
                                     {"D" {}}}}
                         "C" {}}}}}}

           (x-> "ref-pth ~= cmpt-pth"

                :ref-pth [{:cmpt-id "B"} {:cmpt-id "D"}]
                :=>      ["B" "D"])

           (x-> "ref-pth != cmpt-pth"

                :ref-pth [{:cmpt-id "A"} {:cmpt-id "B"} {:cmpt-id "C"}]
                :=>      ["C"])))])
