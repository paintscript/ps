(defproject paintscript "0.2.20"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.520"]
                 [reagent "0.8.1"]
                 [svg-hiccup-kit "0.2.4"]
                 [xspace "0.2.14"]
                 [z-com "0.8.22"]
                 [keybind "2.2.0"]
                 [urlkit "0.3.3"]
                 [urlkit-util "0.1.2"]]

  :source-paths ["src/"]
  :plugins      [[lein-cljsbuild "1.1.7"]
                 [lein-figwheel "0.5.19"]]

  :clean-targets ^{:protect false} ["resources-app/public/js/compiled/"
                                    "resources-docs/public/js/compiled/"]

  :profiles
  {:xx     {:test-paths ["xspace/"]}
   :kaocha [:xx
            {:dependencies [[lambdaisland/kaocha "0.0-565"]]}]
   :docs   {:resource-paths ["resources-docs/"]
            :dependencies   [[hiccup-icons "0.4.2"]]
            :figwheel       {:css-dirs ["resources-docs/public/css"]}}
   :app    {:resource-paths ["resources-app/"]
            :figwheel       {:css-dirs ["resources-app/public/css"]}}}

  :cljsbuild
  {:builds
   [{:id           "app--fig"
     :source-paths ["src/"]
     :figwheel     {:on-jsload "paintscript.app/on-js-reload" }
     :compiler     {:main       paintscript.app
                    :output-to  "resources-app/public/js/compiled/app.js"
                    :output-dir "resources-app/public/js/compiled/out"
                    :asset-path "js/compiled/out"
                    :source-map-timestamp true}}

    {:id           "app--prod"
     :source-paths ["src/"]
     :compiler     {:main       paintscript.app
                    :output-to  "resources-app/public/js/compiled/app.js"
                    :output-dir "resources-app/public/js/compiled/out2"
                    :optimizations :advanced
                    :pretty-print  false}}

    {:id           "docs--fig"
     :source-paths ["src/" "xspace/" "src-docs/"]
     :figwheel     {:on-jsload "paintscript.docs-index/on-js-reload" }
     :compiler     {:main       paintscript.docs-index
                    :output-to  "resources-docs/public/js/compiled/docs.js"
                    :output-dir "resources-docs/public/js/compiled/out3"
                    :asset-path "js/compiled/out3"
                    :source-map-timestamp true}}

    {:id           "docs--prod"
     :source-paths ["src/" "xspace/" "src-docs/"]
     :compiler     {:output-to     "resources-docs/public/js/compiled/docs.js"
                    :main          paintscript.docs-index
                    :optimizations :advanced
                    :pretty-print  false}}]}

  :aliases
  {"pub"    ["install"]
   "xx"     ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"
             "--focus-meta" ":test-refresh/focus" "--watch"]
   "xx1"    ["with-profile" "+xx" "test"]

   "fig"    ["with-profile" "+app" "figwheel"         "app--fig"]
   "bldui"  ["with-profile" "+app" "cljsbuild" "once" "app--prod"]

   "figd"   ["with-profile" "+docs" "figwheel"         "docs--fig"]
   "blduid" ["with-profile" "+docs" "cljsbuild" "once" "docs--prod"]})
