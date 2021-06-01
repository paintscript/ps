(ns paintscript.ops.ops-cmpt
  (:require [paintscript.util :as u]
            [paintscript.nav :as nav]

            [paintscript.data :as data]
            [paintscript.ops.ops-elem :as ops-elem]
            [paintscript.ops.ops-path-tf :as ops-path-tf]

            [paintscript.paint :as paint]
            [paintscript.conv :as conv]))

;; --- HOF

(defn update-el [cmpt navr-sel f & args]
  (-> cmpt
      (nav/update-in-nav* navr-sel :x-el-k #(apply f % args))))

(defn update-els [cmpt navr-sel f & args]
  (case (nav/nav-head-k navr-sel)
    (nil
     :cmpt-pth) (-> cmpt
                    (nav/update-in-nav navr-sel
                                       (fn [cmpt-sel]
                                         (-> cmpt-sel
                                             (update :script
                                                     (partial mapv
                                                              #(case (:el-k %)
                                                                 :path (apply f % args)
                                                                 %)))))))
    :x-el-k     (-> cmpt
                    (nav/update-in-nav navr-sel #(apply f % args)))))

(defn update-pcmds-src-k
  [cmpt src-k f-pcmds args]
  (case src-k
    :defs   (-> cmpt
                (update :defs   (partial u/map-vals
                                         #(if (vector? %) ;; NOTE: excludes sub-defs
                                            (apply f-pcmds % args)
                                            %))))
    :script (-> cmpt
                (update :script (partial mapv
                                         #(if (= :path (:el-k %))
                                            (apply ops-elem/update-el-argv % f-pcmds args)
                                            %))))))

(defn update-pcmds-cmpt [cmpt f & args]
  (-> cmpt
      (update-pcmds-src-k :defs   f args)
      (update-pcmds-src-k :script f args)))

(defn update-pcmds-sel
  [cmpt {:as navr-sel :keys [src-k]} f-pcmds & args]
  (if-not navr-sel
    (apply update-pcmds-cmpt cmpt f-pcmds args)
    (case (nav/nav-head-k navr-sel)
      (nil
       :cmpt-pth) (-> cmpt
                      (nav/update-in-nav* navr-sel :cmpt-pth
                                          (fn [cmpt-sel]
                                            (apply update-pcmds-cmpt cmpt-sel f-pcmds args))))
      ;; NOTE: dosn't come up?
      ; :src-k      (-> cmpt
      ;                 (update-pcmds-src-k (:src-k navr-sel) f-pcmds args))
      (:x-el-k
       :p-el-i
       :xy-i)     (-> cmpt
                      (nav/update-in-nav* navr-sel :x-el-k
                                          (fn [el]
                                            (apply ops-elem/update-el-argv el f-pcmds args)))))))

;; --- generic (TODO: add support for non-path elements)

(defn translate
  ([cmpt sel n] (-> cmpt (update-pcmds-sel sel ops-path-tf/translate-pcmds n)))
  ([cmpt     n] (-> cmpt (update-pcmds-cmpt    ops-path-tf/translate-pcmds n))))

(defn rotate
  ([cmpt sel c a] (-> cmpt (update-pcmds-sel sel ops-path-tf/rotate-pcmds c a)))
  ([cmpt     c a] (-> cmpt (update-pcmds-cmpt    ops-path-tf/rotate-pcmds c a))))

(defn scale
  ([cmpt sel c n] (-> cmpt (update-pcmds-sel sel ops-path-tf/scale-pcmds c n)))
  ([cmpt     c n] (-> cmpt (update-pcmds-cmpt    ops-path-tf/scale-pcmds c n))))

;; --- path-specific

(defn absolute
  ([cmpt sel] (-> cmpt (update-pcmds-sel sel ops-path-tf/normalize-pcmds :op :rel->abs)))
  ([cmpt]     (-> cmpt (update-pcmds-cmpt    ops-path-tf/normalize-pcmds :op :rel->abs))))

(defn full
  ([cmpt sel] (-> cmpt (update-pcmds-sel sel ops-path-tf/normalize-pcmds :op :short->full)))
  ([cmpt]     (-> cmpt (update-pcmds-cmpt    ops-path-tf/normalize-pcmds :op :short->full))))

(defn normalize
  ([cmpt sel] (-> cmpt (update-pcmds-sel sel ops-path-tf/normalize-pcmds :op :all)))
  ([cmpt]     (-> cmpt (update-pcmds-cmpt    ops-path-tf/normalize-pcmds :op :all))))

(defn mirror
  ([cmpt axis pos sel] (-> cmpt (update-pcmds-sel sel ops-path-tf/mirror-pcmds axis pos)))
  ([cmpt axis pos]     (-> cmpt (update-pcmds-cmpt    ops-path-tf/mirror-pcmds axis pos))))

(defn reverse-path
  ([cmpt sel] (-> cmpt (update-pcmds-sel sel ops-path-tf/reverse-pcmds)))
  ([cmpt]     (-> cmpt (update-pcmds-cmpt    ops-path-tf/reverse-pcmds))))

(defn toggle-d [cmpt navr-sel]
  (-> cmpt
      (update-els navr-sel
                  (fn [{:as el-path :keys [el-argv], {:as el-opts :keys [d]} :el-opts}]
                    {:pre [(-> el-path :el-k (= :path))]}
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

(defn append-pth
  [script pi]
  (-> script
      (u/vec-append pi
                    (data/elemv :path [(data/elemv :M [[10 10]])]))))

(defn del-pth [script pi] (-> script (u/vec-remove pi)))
