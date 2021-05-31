(ns paintscript.ops.ops-cmpt
  (:require [paintscript.util :as u]
            [paintscript.nav :as nav]

            [paintscript.data :as data]
            [paintscript.ops.ops-elem :as ops-elem]
            [paintscript.ops.ops-path-tf :as ops-path-tf]

            [paintscript.paint :as paint]
            [paintscript.conv :as conv]))

(defn update-pcmd-seqs [cmpt f & args]
  (-> cmpt
      (update :defs   (partial u/map-vals #(if (vector? %) ;; NOTE: excludes sub-defs
                                             (apply f % args)
                                             %)))
      (update :script (partial mapv       #(if (= :path (:el-k %))
                                             (apply ops-elem/update-el-argv % f args)
                                             %)))))

(defn update-pcmd-seq-sel [cmpt {:as navr-sel :keys [src-k]} f & args]
  (if-not navr-sel
    (apply update-pcmd-seqs cmpt f args)
    (case src-k
      :defs   (apply nav/update-in-nav* cmpt navr-sel :x-el-k f args)
      :script (apply nav/update-in-nav* cmpt navr-sel :x-el-k ops-elem/update-el-argv f args))))

(defn append-pth
  [script pi]
  (-> script
      (u/vec-append pi
                    (data/elemv :path [(data/elemv :M [[10 10]])]))))

(defn del-pth [script pi] (-> script (u/vec-remove pi)))

(defn translate
  ([cmpt sel n] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/translate-p-els n)))
  ([cmpt     n] (-> cmpt (update-pcmd-seqs        ops-path-tf/translate-p-els n))))

(defn rotate
  ([cmpt sel c a] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/rotate-p-els c a)))
  ([cmpt     c a] (-> cmpt (update-pcmd-seqs        ops-path-tf/rotate-p-els c a))))

(defn scale
  ([cmpt sel c n] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/scale-p-els c n)))
  ([cmpt     c n] (-> cmpt (update-pcmd-seqs        ops-path-tf/scale-p-els c n))))

(defn absolute
  ([cmpt sel] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/normalize-pcmd-seq :op :rel->abs)))
  ([cmpt]     (-> cmpt (update-pcmd-seqs        ops-path-tf/normalize-pcmd-seq :op :rel->abs))))

(defn full
  ([cmpt sel] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/normalize-pcmd-seq :op :short->full)))
  ([cmpt]     (-> cmpt (update-pcmd-seqs        ops-path-tf/normalize-pcmd-seq :op :short->full))))

(defn normalize
  ([cmpt sel] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/normalize-pcmd-seq :op :all)))
  ([cmpt]     (-> cmpt (update-pcmd-seqs        ops-path-tf/normalize-pcmd-seq :op :all))))

(defn mirror
  ([cmpt axis pos sel] (-> cmpt (update-pcmd-seq-sel sel #(ops-path-tf/mirror-p-els axis pos %))))
  ([cmpt axis pos]     (-> cmpt (update-pcmd-seqs        #(ops-path-tf/mirror-p-els axis pos %)))))

(defn reverse-path
  ([cmpt sel] (-> cmpt (update-pcmd-seq-sel sel ops-path-tf/reverse-p-els)))
  ([cmpt]     (-> cmpt (update-pcmd-seqs        ops-path-tf/reverse-p-els))))

(defn update-p-opts [cmpt navr-sel f & args]
  (-> cmpt
      (nav/update-in-nav* (-> navr-sel
                              (assoc :p-el-i 1)) :p-el-i
                          (fn [pth-el]
                            (apply f pth-el args)))))

(defn toggle-d [cmpt navr-sel]
  (-> cmpt
      (nav/update-in-nav* navr-sel :x-el-k
                          (fn [{:as el-path :keys [el-argv], {:as el-opts :keys [d]} :el-opts}]
                            (cond
                              d     (-> ;; NOTE: path-d->els returns vecs so requires
                                        ;; parsing with contextual locr
                                        (vec
                                         (concat [:path (-> (:el-opts el-path) (dissoc :d))]
                                                 (conv/path-d->els d)))
                                        (->> (data/elvv->rr (:locr el-path))))
                              :else (-> el-path
                                        (update :el-opts assoc :d (paint/path-str cmpt el-opts el-argv))
                                        (assoc  :el-argv nil)))))))
