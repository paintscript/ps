(defproject paintscript "0.1.1"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.520"]
                 [reagent "0.8.1"]
                 [svg-hiccup-kit "0.2.4"]
                 [xspace "0.2.14"]]

  :source-paths ["src/"]

  :profiles
  {:xx     {:test-paths ["xspace/"]}
   :kaocha [:xx
            {:dependencies [[lambdaisland/kaocha "0.0-565"]]}]
   :docs   {:resource-paths ["resources-docs/"]
            :dependencies   [[hiccup-icons "0.4.1"]
                             [urlkit "0.3.3"]
                             [urlkit-util "0.1.2"]]
            :plugins        [[lein-cljsbuild "1.1.7"]
                             [lein-figwheel "0.5.19"]]}}

  :cljsbuild
  {:builds
   [{:id           "docs--fig"
     :source-paths ["src/" "xspace/" "src-docs/"]
     :figwheel     {:on-jsload "paintscript.docs-index/on-js-reload" }
     :compiler     {:main       paintscript.docs-index
                    :output-to  "resources-docs/public/js/compiled/docs.js"
                    :output-dir "resources-docs/public/js/compiled/out"
                    :asset-path "js/compiled/out"
                    :source-map-timestamp true}}

    {:id           "docs--prod"
     :source-paths ["src/" "xspace/" "src-docs/"]
     :compiler     {:output-to     "resources-docs/public/js/compiled/docs.js"
                    :main          paintscript.docs-index
                    :optimizations :advanced
                    :pretty-print  false}}]}

  :figwheel
  {:css-dirs ["resources-docs/public/css"]}

  :aliases
  {"pub"   ["install"]
   "xx"    ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"
            "--focus-meta" ":test-refresh/focus" "--watch"]
   "fig"   ["with-profile" "+docs" "figwheel"         "docs--fig"]
   "bldui" ["with-profile" "+docs" "cljsbuild" "once" "docs--prod"]})
