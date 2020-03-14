(ns paintscript.conv
  (:require [clojure.string :as str]
            #?(:cljs [cljs.reader :refer [read-string]])))

(defn path-d->els-str [s]
  (-> s
      (str/replace #"\-?\d+\.?\d*e\-\d+", "0") ;; exponential numbers
      (str/replace #"," " ")
      (str/replace #"-" " -")
      (str/replace #"([a-zA-Z])" "\n$1 ") ;; path commands
      (str/replace #"\s+([\-\d\.]+)\s+([\-\d\.]+)" " [$1 $2]") ;; pair of numbers
      (str/replace #"([a-zA-Z])([^\n]*)" "[:$1 $2]")))

(defn post-proc-els [els]
  (mapcat (fn [[k & args :as el]]
            (case k
              (:c :C) (if (-> (count args) (> 3))
                        (->> (partition 3 args)
                             (map #(vec (cons k %))))
                        [el])
              [el]))
          els))

(defn path-d->els [s]
  (->> (read-string (str "[" (path-d->els-str s) "]"))
       post-proc-els
       vec))

(defn path-d->path [s] (vec (concat [:path {}] (path-d->els s))))

(defn path-d->script [s] {:script [(path-d->path s)]})

(defn points->els [s]
  (let [[xy0 & xy*]
        (->> (str/split s #" ")
             (mapv #(->> (str/split % #",") (mapv read-string))))]
    [[:M xy0]
     (vec (cons :L xy*))]))
