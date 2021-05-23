(ns paintscript.app.s-log)

(defn init [cmpt0 ui-init]
  {:i-active 0
   :s-items  (list
              {:n    1
               :op   [:init]
               :cmpt cmpt0
               :ui   ui-init})})

(defn undo? [!s-log]
  (-> @!s-log :s-items first :n (not= 1)))

(defn op [!s-log !cmpt !ui op-k & [op-arg]]
  (let [{:keys [i-active
                s-items]} @!s-log

        [i-active'
         [s-active'
          s-items']]
        (case op-k
          :undo     (let [i (inc i-active)]
                      [i [(nth s-items i) s-items]])
          :activate [op-arg   [(nth s-items op-arg) s-items]]
          :preview  [i-active [(nth s-items op-arg) s-items]]

          (:clear
           :clear<
           :clear>) (let [s (-> (nth s-items i-active)
                                (assoc :n 1))
                          [i
                           ss] (case op-k
                                 :clear  [0 (list s)]
                                 :clear< [i-active
                                          (let [n-keep (inc i-active)
                                                n-drop (-> s-items first :n (- n-keep))]
                                            (->> s-items
                                                 (take n-keep)
                                                 (map #(-> % (update :n - n-drop)))))]
                                 :clear> [0 (->> s-items
                                                 (drop i-active))])]
                      [i [s ss]]))]

    (reset!  !s-log  {:i-active i-active'
                      :s-items  s-items'})
    (reset! !cmpt (:cmpt s-active'))
    (reset! !ui     (:ui     s-active'))))

(defn items [!s-log]
  (let [{:keys [i-active s-items]} @!s-log]
    [i-active
     (map-indexed vector s-items)]))

(defn add [!s-log s-item]
  (let [{:as s-log
         [{:as s-curr :keys [n]} :as s-items] :s-items} @!s-log]
    (reset! !s-log
            {:i-active 0
             :s-items
             (if (and (= :set-sel-d
                         (first (:op s-curr))
                         (first (:op s-item)))
                      (= (-> s-curr :ui :sel)
                         (-> s-item :ui :sel)))
               (-> s-items
                   rest
                   (conj (-> s-item (assoc :n n))))
               (-> s-items
                   (conj (-> s-item (assoc :n (-> n inc))))))})))
