(ns paintscript.render-svg
  (:require [paintscript.render :as render]))

(defn- arcs
  [arc-xys {:as opts :keys [mode ctd?] :or {mode :concave}}]
  (let [[head & tail]    arc-xys
        paired-with-prev (map vector tail (drop-last arc-xys))
        arc-middle       (str (case mode :concave "0,1" :convex "0,0") )]
    (concat (when-not ctd?
              [["M" head]])
            (for [[[x  y  :as _xy]
                   [xp yp :as _xy-prev]] paired-with-prev]
              ["A" [(- x xp) (- y yp)] [0 arc-middle] [x y]]))))

(defn- el->out
  [{:as el :keys [el-k opts args]}]
  (case el-k
    :arc  (arcs args opts)
    :arc* (arcs args (assoc opts :ctd? true))
    :M    (cons "M" args)
    :m    (cons "m" args)
    :L    (cons "L" args)
    :l    (cons "l" args)

    :V    (cons "V" args)
    :H    (cons "H" args)
    :v    (cons "v" args)
    :h    (cons "h" args)

    :A    (let [[r  p  tgt] args] (list "A" r  p   tgt))
    :S    (let [[   c2 tgt] args] (list "S"    c2  tgt))
    :s    (let [[   c2 tgt] args] (list "s"    c2  tgt))
    :C    (let [[c1 c2 tgt] args] (list "C" c1 c2  tgt))
    :c    (let [[c1 c2 tgt] args] (list "c" c1 c2  tgt))
    :C1   (let [[c1    tgt] args] (list "C" c1 tgt tgt))
    :c1   (let [[c1    tgt] args] (list "c" c1 tgt tgt))
    :Q    (let [[c     tgt] args] (list "Q" c      tgt))
    :q    (let [[c     tgt] args] (list "q" c      tgt))
    :T    (let [[c1    tgt] args] (list "T" c1 tgt tgt))
    :t    (let [[c1    tgt] args] (list "t" c1 tgt tgt))
    :z    (list "z")))

(def svg-renderer
  (reify render/Renderer
    (els->out [_ els] (->> els
                           (map el->out)
                           flatten))))
