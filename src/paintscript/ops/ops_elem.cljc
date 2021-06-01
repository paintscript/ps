(ns paintscript.ops.ops-elem
  "generic ops on data/Elem records(-colls)"
  (:require [paintscript.nav :as nav]))

;; --- traverse (HOF)

(defn map-el-args
  [el f & args]
  (-> el (update :el-argv #(mapv f %))))

(defn update-el-argv
  [el f & args]
  (-> el
      (update :el-argv #(apply f % args))))

;; --- refs

(defn ref?   [elr] (-> elr :el-k (= :ref)))
(defn ref-id [elr] (get-in elr [:el-argv 0]))

(defn resolve-els-ref  [cmpt-defs ref] (get    cmpt-defs (-> ref ref-id)))
(defn resolve-d-ref    [cmpt-defs ref] (get-in cmpt-defs [:d (-> ref ref-id)]))
(defn resolve-cmpt-ref [cmpt-defs ref] (get-in cmpt-defs [:components (-> ref ref-id)]))

(defn resolve-els-refs
  [cmpt-defs els]
  (->> els
       (into []
             (comp (map-indexed (fn [i elr]
                                  (if (ref? elr)
                                    (resolve-els-ref cmpt-defs elr)
                                    [elr])))
                   cat))))
