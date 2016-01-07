(ns xesenrich.sim-test
  (:require [clojure.test :refer :all]
            [xesenrich.bpmn :refer :all]
            [xesenrich.cpn :as cpn]
            [xesenrich.sim :refer :all]           
            )
  (:import [xesenrich.bpmn Person]
           [xesenrich.bpmn Request]
           [xesenrich.bpmn Machine]))





(def model
  (build-model "resources/courseScheduling2.bpmn"))


(deftest init-sim-test
  (testing "Init sim test"
    (let [cpn (init-sim model {"start" [[(Request. 1 "OK")]]
                               "Idle_9" [[(Person. 1)]]
                               "Idle_10" [[(Person. 2)]]
                               "Idle_11" [[(Person. 3)]]})]
      (println (get-in [:places "start" :marking] cpn))
      (is (= 1 (count (get-in [:places "start" :marking] cpn))))
      (is (= 1 (count (get-in [:places "Idle_9" :marking] cpn))))
      (is (= 1 (count (get-in [:places "Idle_10" :marking] cpn))))
      (is (= 1 (count (get-in [:places "Idle_11" :marking] cpn))))
    )))
