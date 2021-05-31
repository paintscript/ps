(ns paintscript.xspace-index
  (:require [clojure.test :refer [deftest testing is]]
            [xspace.core :as x]

            [paintscript.data-xspace :as data-xs]
            [paintscript.nav-xspace :as nav-xs]
            [paintscript.ops-xspace :as ops-xs]
            [paintscript.path-xspace :as path-xs]
            [paintscript.util-xspace :as util-xs]))

(deftest data-test
  (x/traverse-xspace data-xs/data-xspace-cfg
                     data-xs/data-xspace))

(deftest nav-test
  (x/traverse-xspace nav-xs/nav-xspace-cfg
                     nav-xs/nav-xspace))

(deftest ops-test
  (x/traverse-xspace ops-xs/ops-xspace-cfg
                     ops-xs/ops-xspace))

(deftest util-test
  (x/traverse-xspace util-xs/util-xspace-cfg
                     util-xs/util-xspace))

(deftest path-test
  (x/traverse-xspace path-xs/path-xspace-cfg
                     path-xs/path-xspace))
