(ns xesenrich.sim
  (:require [xesenrich.cpn :as cpn]
            [xesenrich.xeslog :as log]
            [xesenrich.bpmn :as bp])
    (:import [xesenrich.bpmn Person]
           [xesenrich.bpmn Request]
           [xesenrich.bpmn Machine]))

; CPN sim package
(defn init-sim
  [cpn token-map]
  (reduce #(update-in %1 [:places %2 :marking] concat (get token-map %2))
          cpn (keys token-map)))


(defn end-sim [cpn]
  (doseq [t (bp/get-instance-ids cpn)]
    (log/close-instance t)))

;; limited timing support: only static global timeout on tasks without executors;
(defn start-sim [cpn id timeout nbtrans transprob]
   ;;generates a seq of sum of rand numbers
  (let [timings  (take nbtrans
                       (reductions + (repeatedly
                                      #(rand-int 100))))]
    (reduce (fn [net t]
              (read-line)
                     (cpn/random-fire net t transprob))
                   cpn
                   timings)
    ))

(def init-map {:default 1,
               "Resume" 1,
               "LackExecutors" 1,
               "Come-Back" 0.9,
               "Activation" 1,
               "Unavailability" 0.9,
               "Termination" 1,
               })

(def probs {"YES" 0.5,
            "NO" 1.0})

(def statusprobs {
                  "Search full time trainers" probs,
                  "Search Freelance trainer" probs,
                  "Search Mixing Trainers" probs,
                  "Search equipment" probs
                 })

(def token-map { ;
                "start" [[(bp/Request. 0 "lifecycleT" "" "" [])], [(bp/Request. 5 "lifecycleT" "" "" [])]],
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
  (let [cpn1 (bp/build-model "./resources/courseScheduling2.bpmn" statusprobs)
        cpn2 (init-sim cpn1 token-map)
        pbbs (get-transprob cpn2 init-map)
        cpn3 (start-sim cpn2 "0" 0 10 pbbs)
        ]
    (end-sim cpn3)
    cpn3))
