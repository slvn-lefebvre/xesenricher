(ns xesenrich.sim
  (:require [xesenrich.cpn :as cpn]
            [xesenrich.xeslog :as log]
            [xesenrich.bpmn :as bp])
    (:import [xesenrich.bpmn Person]
           [xesenrich.bpmn Request]
           [xesenrich.bpmn Machine]
           ))

; CPN sim package
(defn init-sim
  [cpn token-map]
  (reduce #(update-in %1 [:places %2 :marking] concat (get token-map %2))
          cpn (keys token-map)))


;; limited timing support: only static global timeout on tasks without executors;
(defn start-sim [cpn id timeout nbtrans transprob]
  (log/create-instance id)
  (let [timings  (take nbtrans (repeatedly #(rand-int 100)))
        c2       (reduce (fn [net t]
                           (cpn/random-fire net t transprob))
                         cpn timings)]
    (println timings)
    (log/close-instance (str id ".xes"))
    (cpn/print-markings c2)
    c2))

(def init-map {:default 0.5,
               "Resume" 0.5,
               "LackExecutors" 0.1,
               "Come-Back" 0.2,
               "Activation" 1.0,
               "Unavailability" 0.1,
               "Termination" 0.5,
               })

(def token-map {
                "start" [[(bp/Request. 0 "lifecycleT" "" "" [])]],
                "Idle_9" [[(bp/Person. 1 "lifecycleP")]],
                "Idle_11" [[(bp/Person. 2 "lifecycleP")]],
                "Idle_10" [[(bp/Person. 3 "lifecycleP")]]
                })

(defn get-transprob
  [cpn init-map]
  (into {}
        (map (fn [t]
         (let [w (re-find #"[A-Za-z\-]+" t)]
           (if (not= nil w)
             [t (get init-map w)]
             [t (get init-map :default)])))
             (keys (:transitions cpn)))))

  
(defn test-sim []
  (let [cpn1 (bp/build-model "./resources/courseScheduling2.bpmn" "1.xes")
        cpn2 (init-sim cpn1 token-map)
        pbbs (get-transprob cpn2 init-map)
        cpn3 (start-sim cpn2 "1" 0 100 pbbs)]
    (cpn/print-markings cpn3)))
