(ns paintscript.nav
  (:require [clojure.string :as str]
            [paintscript.util :as u]))

;; TODO: add layering capability either by stacking or nesting navs
(defrecord Nav [cmpt-pth0 ;; base cmpt
                ref-pth   ;; chain of references (starting from cmpt-base)
                cmpt-pth  ;; effective cmpt-pth after resolving references

                src-k     ;; :script or :defs (only used for path-seq defs)
                x-el-k    ;; main-element index (within :script or :defs)
                p-el-i    ;; sub-element index (within :path)
                xy-i      ;; point index (within :path element)
                ])

(def navr-root (map->Nav {}))

(def kk-levels [:src-k :x-el-k :p-el-i :xy-i])
(def kk-all    [:cmpt-pth :src-k :x-el-k :p-el-i :xy-i])
(def kk-rev    (reverse kk-all))

(defn nav-rec [& {:as args :keys [cmpt-pth src-k x-el-k p-el-i xy-i]}]
  {:pre [(case src-k
           :script (-> x-el-k integer?)
           :defs   (-> x-el-k string?)
           true)
         (-> p-el-i ((some-fn nil? integer?)))
         (-> xy-i   ((some-fn nil? integer?)))]}
  (map->Nav args))

(defn nav-vec->rec
  ;; NOTE: only used for xspace
  [[src-k x-el-k p-el-i xy-i]]
  (nav-rec :src-k  src-k
           :x-el-k x-el-k
           :p-el-i p-el-i
           :xy-i   xy-i))

(defn navr->str [{:as args :keys [cmpt-pth src-k x-el-k p-el-i xy-i]}]
  (-> []
      (cond-> cmpt-pth (conj (str/join "/" cmpt-pth))
              src-k    (conj (name src-k))
              x-el-k   (conj x-el-k)
              p-el-i   (conj p-el-i)
              xy-i     (conj xy-i))
      (->> (str/join "/"))))

(defn- cmpt-pth->data-pth [cmpt-pth]
  (->> cmpt-pth
       (mapcat (fn [cmpt-id]
                 (list :defs :components cmpt-id)))
       seq))

(defn nav-rec->data-pth
  ([navr] (nav-rec->data-pth navr nil))
  ([{:as navr :keys [cmpt-pth]} trunc-k]
   (when navr
     (seq
      (concat (-> cmpt-pth
                  cmpt-pth->data-pth)

              ;; truncate on nil or trunc-k
              (reduce (fn [pthv k]
                        (let [v (get navr k)]
                          (-> pthv
                              (cond-> (and v
                                           (-> k #{:p-el-i :xy-i})) (conj :el-argv)
                                      v
                                      (conj v)
                                      (or (not v)
                                          (= trunc-k k)) reduced))))
                      []
                      kk-levels))))))

(defn nav-truncate [navr k-last]
  (loop [[k & kk] kk-all
         trunc?   false
         navr     navr]
    (cond
      (not k)      navr
      (= k-last k) (recur kk true navr)
      :else        (let [navr (-> navr
                                  (cond-> trunc? (assoc k nil)))]
                     (recur kk trunc? navr)))))

(defn get-in-nav
  ([cmpt-root navr] (get-in-nav cmpt-root navr nil))
  ([cmpt-root navr trunc-k]
   (-> cmpt-root
       (get-in (nav-rec->data-pth navr trunc-k)))))

(defn- update-in* [value data-pth f args]
  (if data-pth
    (apply update-in value data-pth f args)
    (apply f value args)))

(defn update-in-nav [cmpt-root navr f & args]
  (update-in* cmpt-root (nav-rec->data-pth navr) f args))

(defn update-in-nav* [cmpt-root navr trunc-k f & args]
  (if-not (get navr trunc-k)
    cmpt-root
    (update-in* cmpt-root (nav-rec->data-pth navr trunc-k) f args)))

(defn- cmpt->index [cmpt-pth cmpt]
  (->> (keys (get-in cmpt [:defs :components]))
       (map (fn [cmpt-id]
              [cmpt-id (conj cmpt-pth cmpt-id)]))
       (into {})))

(defn ref-pth->cmpt-pth
  ([cmpt-root ref-pth] (ref-pth->cmpt-pth cmpt-root nil ref-pth))
  ([cmpt-root cmpt-pth0 ref-pth]
   (loop [cmpt (-> cmpt-root
                   (cond-> cmpt-pth0
                           (get-in (-> cmpt-pth0
                                       cmpt-pth->data-pth))))
          [{:as ref :keys [cmpt-id]}
           & ref-pth-tail] ref-pth
          cmpt-pth     (or cmpt-pth0 [])
          cmpt-id->pth (cmpt->index [] cmpt-root)]
     (if-not cmpt-id
       cmpt-pth
       (let [cmpt-sub-pth  (cmpt-id->pth cmpt-id)
             cmpt-sub      (get-in cmpt-root
                                   (-> cmpt-sub-pth
                                       cmpt-pth->data-pth))
             cmpt-id->pth' (-> cmpt-id->pth (merge (cmpt->index cmpt-sub-pth cmpt-sub)))]
         (recur cmpt-sub
                ref-pth-tail
                (get cmpt-id->pth' cmpt-id)
                cmpt-id->pth'))))))

(defn get-cmpt-sel [cmpt-root {:as navr-sel :keys [cmpt-pth0
                                                  ref-pth
                                                  cmpt-pth]}]
  (let [cmpt-base (-> cmpt-root
                      (cond-> cmpt-pth0
                              (get-in (-> cmpt-pth0
                                          cmpt-pth->data-pth))))
        cmpt-sel  (if-not ref-pth
                    cmpt-base
                    (-> cmpt-root
                        (cond-> cmpt-pth
                                (get-in (-> cmpt-pth
                                            cmpt-pth->data-pth)))))]
    [cmpt-base
     cmpt-sel]))

(defn cmpt-merge-canvas [cmpt cmpt-root {:as navr-sel :keys [cmpt-pth]}]
  (-> cmpt
      (cond-> cmpt-pth
              (merge (u/cascading-merges
                      cmpt-root
                      (->> cmpt-pth
                           (map (fn [cmpt-id]
                                  (cmpt-pth->data-pth [cmpt-id]))))
                      [:canvas])))))

(defn cmpt-merge-defs [cmpt cmpt-root {:as navr-sel :keys [cmpt-pth]}]
  (-> cmpt
      (cond-> cmpt-pth
              (merge (u/cascading-merges
                      cmpt-root
                      (->> cmpt-pth
                           (map (fn [cmpt-id]
                                  (cmpt-pth->data-pth [cmpt-id]))))
                      [:defs
                       :attr-classes])))))

(defn nav-head-k [navr]
  (->> kk-rev
       (filter #(get navr %))
       first))

(defn nav-up [navr & {:keys [drop-src-k?]}]
  (when-let [k (nav-head-k navr)]
    (-> navr
        (assoc k nil)
        (cond-> (and (= :x-el-k k)
                     drop-src-k?)
                (assoc :src-k nil)

                (and (= :cmpt-pth k)
                     (:ref-pth navr))
                (update :ref-pth butlast)))))

(defn- nav-max [navr cmpt]
  (-> (get-in-nav cmpt (-> navr
                           nav-up))
      count
      (- 1)))

(defn nav-next [navr cmpt]
  (if-let [k (nav-head-k navr)]
    (-> navr
        (update k #(-> % inc (min (nav-max navr cmpt)))))
    (nav-rec :src-k :script
             :x-el-k 0)))

(defn nav-prev [navr]
  (when-let [k (nav-head-k navr)]
    (-> navr
        (update k #(-> % dec (max 0))))))

(defn nav-into-ref [navr cmpt
                    {:as s-el
                     s-el-opts :el-opts
                     [cmpt-id] :el-argv}]
  (let [ref-item  (-> s-el-opts (assoc :cmpt-id cmpt-id))
        ref-pth'  (-> (:ref-pth navr) (u/conjv ref-item))
        cmpt-pth0 (:cmpt-pth0 navr)
        cmpt-pth  (ref-pth->cmpt-pth cmpt cmpt-pth0 ref-pth')]
    (nav-rec :cmpt-pth0 cmpt-pth0
             :ref-pth   ref-pth'
             :cmpt-pth  cmpt-pth)))

(defn nav-into [navr cmpt]
  (when-let [k (nav-head-k navr)]
    (case k
      :cmpt-pth (-> navr (assoc :src-k  :script
                                :x-el-k 0))
      :x-el-k   (let [ref-el (get-in-nav cmpt navr)]
                  (case (:el-k ref-el)
                    :ref (-> navr (nav-into-ref cmpt ref-el))
                    (-> navr (assoc :p-el-i 0))))
      :p-el-i   (-> navr (assoc :xy-i 0))
      navr)))

(defn xy-nav? [navr] (:xy-i navr))

(defn cmpt> [cmpt & {:keys [src-k s-eli ii iii] :or {src-k :script}}]
  (cond s-eli (get-in cmpt [src-k s-eli])
        ii    (get-in cmpt (cons src-k ii))
        iii   (get-in cmpt (cons src-k iii))))

(defn s-el>
  "given a script element `s-el` of type :path select one of its path elements"
  [s-el & {:keys [p-eli
                  p-eln]}]
  (cond p-eli (get s-el p-eli)
        p-eln (get s-el p-eln)))

;; TODO: path elements?
(defn els> [els & {:keys [eli eln]}]
  (cond eli (nth els eli)
        eln (nth els eln)))

;; TODO: path element?
(defn el> [el & {:keys [xyi xyn]}]
  (cond xyi (get el xyi)
        xyn (get el xyn)))

(defn xys> [xys & {:keys [xyi xyn]}]
  (cond xyi (nth xys xyi)
        xyn (nth xys xyn)))

;; --- rels

;; TODO: path elements?
(defn els-prev [els & {:keys [eli eln]}]
  (get els (cond eli (-> eli (- 1))
                 eln (-> eln (- 1)))))

(defn els-next [els & {:keys [eli eln]}]
  (get els (-> eli (+ 1))))
