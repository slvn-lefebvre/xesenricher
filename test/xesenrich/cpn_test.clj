(ns xesenrich.cpn-test
  (:require [clojure.test :refer :all]
            [xesenrich.cpn :refer :all])
   (:import [xesenrich.cpn CPN]
           [xesenrich.cpn Place]
           [xesenrich.cpn Transition]
           [xesenrich.cpn Arc]))


(def simpleplusnet
  (CPN.
   "addnet",
   {"in" (Place. "in" #{} [[8],[1],[2],[3],[3],[8],[8]]),
    "added" (Place. "added" #{} [])},
   {"Add" (Transition. "Add" #(if (< 5 (:x %)) true false) #{:x} [])},
    [ (Arc. "in"  "Add" #(-> {:x (first %)}) false),
    (Arc. "Add" "added" #(-> {:x (inc (:x %))}) false)]
    ))

(def simpletimesnet
  (CPN.
   "timesnet",
   {"added" (Place. "added" #{} []),
    "timed" (Place. "timed" #{} []),
    },
   {"times" (Transition. "times" #(if (< 5 (:x %)) true false) #{:x} [])},
    [ (Arc. "added"  "times" #(-> {:x (first %)}) false),
    (Arc. "times" "timed" #(-> {:x (* 2 (:x %))}) false)]
    ))




(deftest merge-cpn-test
  (testing "CPN merge"
    (let [result (merge-cpn simpleplusnet simpletimesnet)]
      (is (= 3 (count (:places result))))
      (is (= 4 (count (:arcs result))))
      (is (= 2 (count (:transitions result)))))))

