(defproject paintscript "0.1.1"
  :dependencies [[reagent "0.8.1"]
                 [svg-hiccup-kit "0.2.4"]
                 [xspace "0.2.14"]]

  :source-paths ["src/"]

  :profiles
  {:xx     {:test-paths ["xspace/"]}
   :kaocha [:xx
            {:dependencies [[lambdaisland/kaocha "0.0-565"]]}]}

  :aliases
  {"pub" ["install"]
   "xx"  ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"
          "--focus-meta" ":test-refresh/focus" "--watch"]})
