(ns xesenrich.bpmn-test
  (:require [clojure.test :refer :all]
            [xesenrich.bpmn :refer :all]
            [xesenrich.cpn :as cpn]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip :as c-d-zip]
            [clojure.data.zip.xml :as zx]
            [clojure.java.io :as io]
            [clojure.pprint :as p]
            )
  (:import [xesenrich.cpn CPN]
           [xesenrich.cpn Place]
           [xesenrich.cpn Transition]
           [xesenrich.cpn Arc]))




(def troot
  (-> "/home/slefebvr/clojure/xesenrich/resources/courseScheduling2.bpmn"
      io/reader
      xml/parse
      zip/xml-zip))

(deftest parse-start-test
  (testing "Parse start net"
    (let [starts (parse-start troot)]
      (is (= 1 (count starts)))
      (let [st1 (first starts)]
        ;; id = _13 name = Request out = 15
        (is (= (get st1 :id) "_13"))
        (is (= (get-in st1 [:places "Request" :name]) "Request"))
        (is (= (get-in st1 [:places "_15" :name]) "_15"))
        ))))

(deftest make-activity-test
  (testing "Make activitynet"
    (let [net (make-activitynet "_1" "_2")]
      (is (= "_1" (:id net)))
      (is (not= nil (get-in net [:places "Idle_2"]))))))

(deftest parse-persons-test
  (testing "Parse swim lanes (persons)"
    (let [lanemap (parse-persons troot)]
      (p/pprint lanemap)
      (is (= 13 (count lanemap))))))

(deftest parse-activities-test
  (testing "Parse activities in the BP"
    (let [lanemap (parse-persons troot)
          activities (parse-activities troot lanemap)]
      (is (= 6 (count activities)))
      )))

(deftest parse-flows-test
  (testing "Parsing sequence flows in the BP"
    (let [flows (parse-flows
                 troot)]
      (is (= (count flows) 14)))))

(deftest build-model-test
  (testing "Build model test"
    (let [nets (build-model "./resources/courseScheduling2.bpmn")]
      (println (:arcs nets))
      )))
      
