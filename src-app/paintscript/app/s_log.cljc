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

(defn- clear-s-items [{:as s-log
                       :keys [i-active
                              s-items]} op-k]
  (let [s (-> (nth s-items i-active)
              (assoc :n 1))
        [i
         ss] (case op-k
               ;; --- clear all
               :op.s-log/clear  [0 (list s)]

               ;; --- clear before active (earlier items leading up to active)
               :op.s-log/clear< [i-active
                                 (let [n-keep (inc i-active)
                                       n-drop (-> s-items first :n (- n-keep))]
                                   (->> s-items
                                        (take n-keep)
                                        (map #(-> % (update :n - n-drop)))))]

               ;; --- clear after active (later items that have been undone)
               :op.s-log/clear> [0 (->> s-items
                                        (drop i-active))])]
    (-> s-log
        (assoc :i-active i
               :s-items  ss))))

(defn handle-op
  [s-log cmpt ui
   [op-k & [op-arg] :as op]]
  (let [{:keys
         [i-active
          s-items]} s-log

        s-log' (case op-k
                 :op.s-log/undo     (-> s-log (assoc :i-active (-> i-active inc)
                                                     :s-items  s-items))
                 :op.s-log/activate (-> s-log (assoc :i-active op-arg
                                                     :s-items  s-items))
                 :op.s-log/preview  (-> s-log (assoc :i-active i-active
                                                     :s-items  s-items))

                 (:op.s-log/clear
                  :op.s-log/clear<
                  :op.s-log/clear>) (clear-s-items s-log op-k))

        s-active'  (nth (:s-items  s-log')
                        (:i-active s-log'))]

    {:s-log s-log'
     :cmpt  (-> (get-in s-active' [:s-app :cmpt-root]))
     :ui    (-> (get-in s-active' [:s-app :ui])
                (assoc :tab (:tab ui)))}))

(defn items [!s-log]
  (let [{:keys [i-active
                s-items]} @!s-log]
    [i-active
     (map-indexed vector s-items)]))

(defn- update-head?
  "when the op is equivalent to the last one replace it insead of adding an
   additional one (especially :set-sel-d ops are dispatched en-mass when moving
   a point)"
  [s-curr s-item]
  (and (= :set-sel-d
          (first (:op s-curr))
          (first (:op s-item)))
       (= (-> s-curr :navr-sel)
          (-> s-item :navr-sel))))

(defn add [{:as s-log :keys [i-active]
            [{:as s-curr :keys [n]} :as s-items] :s-items}
           s-item]
  (-> s-log
      (cond-> (and i-active
                   (not= 0 i-active))
              (clear-s-items :op.s-log/clear>))
      (update :s-items #(if (update-head? s-curr s-item)
                          (-> %
                              rest
                              (conj (-> s-item (assoc :n n))))
                          (-> %
                              (conj (-> s-item (assoc :n (-> n inc)))))))))
