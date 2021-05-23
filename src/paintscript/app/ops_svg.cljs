(ns paintscript.app.ops-svg)

(defn svg-xml-b64 [svg-dom]
  (str "data:image/svg+xml;base64,"
       (-> (js/XMLSerializer.)
           (.serializeToString svg-dom)
           (js/btoa))))

(defn to-svg [_ svg-dom cont] (cont (svg-xml-b64 svg-dom)))

(defn to-png! [[w h] svg-dom cont]
  (let [canvas  (js/document.createElement "canvas")
        img     (js/Image.)
        svg-b64 (svg-xml-b64 svg-dom)]
    (doto canvas
          (-> .-width  (set! w))
          (-> .-height (set! h)))
    (doto img
          (-> .-onload (set! (fn []
                               (-> canvas
                                   (.getContext "2d")
                                   (.drawImage img 0 0 w h))
                               (let [png-b64 (-> canvas
                                                 (.toDataURL "image/png"))]
                                 (cont png-b64)))))
          (-> .-src    (set! svg-b64)))))

(defn download! [b64 name]
  (doto (js/document.createElement "a")
        (.. (setAttribute "href" b64))
        (.. (setAttribute "download" name))
        (.. (click))))
