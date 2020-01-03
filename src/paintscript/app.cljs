 (ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:defs   {"head" [[:M [50 38.5]]
                            [:L [32 38.5]]
                            [:c [0 0] [-0.5 -3.5] [2.5 -6.5]]
                            [:s [9 -4] [9 -4]]
                            [:s [-4.5 -4] [-4.5 -9.5]]
                            [:c [0 -5.5] [4.5 -10.5] [11 -11]]]
                    "body" [[:M [50 92]]
                            [:L [20 92]]
                            [:c [0 0] [-0.5 -7] [3 -12]]
                            [:s [11 -6] [11 -6]]
                            [:s [5.5 -9] [8 -17]]
                            [:c [2.5 -7.5] [3 -18.5] [3 -18.5]]
                            [:s [5 0] [5 0]]]}

           :script [; [:path
                    ;  {:mirror :separate}
                    ;  [:M [76 11]]
                    ;  [:C [8 34] [79 61] [79 49]]
                    ;  [:S [10 54] [74 89]]]

                    [:path
                     {:variant-k :outline :mirror :merged :class-k :outline}
                     [:ref "head"]
                     [:ref "body"]]

                    [:path
                     {:variant-k :solid :mirror :merged :class-k :solid}
                     [:ref "head"]
                     [:ref "body"]]]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
