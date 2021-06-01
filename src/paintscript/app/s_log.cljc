(ns paintscript.app.s-log)

(def s-log-ops
  #{:op.s-log/undo
    :op.s-log/activate
    :op.s-log/preview
    :op.s-log/clear
    :op.s-log/clear<
    :op.s-log/clear>})

(defn init [cmpt0 ui-init]
  {:i-active 0
   :s-items  (list
              {:n    1
               :op   [:init]
               :cmpt cmpt0
               :ui   ui-init})})

(defn undo? [!s-log]
  (-> @!s-log :s-items first :n (not= 1)))

(defn handle-op
  [s-log cmpt ui [op-k & [op-arg] :as op]]
  (let [{:keys [i-active
                s-items]} s-log

        [i-active'
         [s-active'
          s-items']]
        (case op-k
          :op.s-log/undo     (let [i (inc i-active)]
                               [i [(nth s-items i) s-items]])
          :op.s-log/activate [op-arg   [(nth s-items op-arg) s-items]]
          :op.s-log/preview  [i-active [(nth s-items op-arg) s-items]]

          (:op.s-log/clear
           :op.s-log/clear<
           :op.s-log/clear>)
          (let [s (-> (nth s-items i-active)
                      (assoc :n 1))
                [i
                 ss] (case op-k
                       :op.s-log/clear  [0 (list s)]
                       :op.s-log/clear< [i-active
                                         (let [n-keep (inc i-active)
                                               n-drop (-> s-items first :n (- n-keep))]
                                           (->> s-items
                                                (take n-keep)
                                                (map #(-> % (update :n - n-drop)))))]
                       :op.s-log/clear> [0 (->> s-items
                                                (drop i-active))])]
            [i [s ss]]))]

    {:s-log {:i-active  i-active'
             :s-items   s-items'}
     :cmpt  (get-in s-active' [:s-app :cmpt-root])
     :ui    (-> (get-in s-active' [:s-app :ui])
                (assoc :tab :tab/log))}))

(defn items [!s-log]
  (let [{:keys [i-active s-items]} @!s-log]
    [i-active
     (map-indexed vector s-items)]))

(defn add [{:as s-log
            [{:as s-curr :keys [n]} :as s-items] :s-items} s-item]
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
         (conj (-> s-item (assoc :n (-> n inc))))))})
