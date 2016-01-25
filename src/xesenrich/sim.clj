(ns xesenrich.sim
  (:require [xesenrich.cpn :as cpn]
            [xesenrich.xeslog :as log]
            [xesenrich.bpmn :as bp]
            [clojure.java.io :as io])
  
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
    (reduce (fn [net t] (cpn/random-fire2 net t transprob))
                   cpn
                   timings)))

(def init-map {:default 1,
               "Resume" 1,
               "LackExecutors" 1,
               "Come-Back" 0.1,
               "Activation" 1,
               "Unavailability" 0.1,
               "Termination" 0.9,
               })

(def probs {"YES" 0.9,
            "NO" 0.1})

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

(defn generate-tokens
  [nbreq nbmap]
  (let [requests (map #(vec [(bp/Request. % "lifecycleT" "" "" [])]) (range 0 nbreq))
        pairs (for [[k v] nbmap]
                {k (map #(vec [(bp/Person. (str k "_" %) "lifecycleP")]) (range 0 v))})]
    (into {"start" requests} pairs))
  )

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
        cpn3 (start-sim cpn2 "0" 0 10 pbbs)]
    (end-sim cpn3)
    cpn3))

(defn count-marking [cpn state]
  (let [pattern (re-pattern (str state "_*"))]
  (->> (filter #(re-find pattern (first %)) (:places cpn))
       (map #(count (:marking (second %))))
       (reduce +))))


(defn load-bpmn 
  "Takes a bpmn file path as input"
  [path]
  (-> path io/reader))


(defn ask-token [req clock]
   (println (str "Request " (:id req) " is suspended, please provide a solution:"))
      (let [answer (read-line)
            v (clojure.string/split answer #":")
            t (clojure.string/trim (first v))
            id (clojure.string/trim (second v))
            rel (clojure.string/trim (nth v 2))
            out (case t
                  "person" (bp/Person. id "lifecycleP"))
            ]
        (spit "./social_log.csv"
              (clojure.string/join ";" [clock "" (:executors req) rel (:id out)])
              :append true)
        out))

(defn ask-solution
  "Asks the user to provide a solution"
  [cpn clock]
  (let
      [suspended (first (filter #(and
                           (< 0 (count (:marking (second %))))
                           (re-find #"Suspended_*"  (first %)))
                         (:places cpn)))
       req (->> (:marking (second suspended)) first first)
       id (second (clojure.string/split (first suspended) #"_"))
       token (ask-token req clock)
       place (:in (first (filter #(and
                                   (re-find #"Idle_*" (:in %))
                                   (= (str "Resume_" id) (:out %)))
                                 (:arcs cpn))))
       ]
    (println place token)
    (update-in cpn [:places place :marking] conj [token])))

(defn sim-suspend 
  "Whenever a task is suspended it asks the user for a solution"
  [cpn tokens transprobs solution-fun]
  (let [currentcpn (atom (init-sim cpn tokens))
        clock (atom 0)
        total (count (get-in @currentcpn [:places "start" :marking]))]
        (while (< (count-marking @currentcpn "stop") total)
          (do
            (swap! clock #(+ (rand-int 100) %))
            (println "Advancing cpn")
            (swap! currentcpn cpn/random-fire @clock transprobs)
            (if (< 0 (count-marking @currentcpn "Suspended"))
              (let [new-cpn (solution-fun @currentcpn @clock)]
                (swap! currentcpn (fn [_ new] new) new-cpn)
                (cpn/print-markings @currentcpn)))))
        (cpn/print-markings @currentcpn)
        (end-sim @currentcpn)))
        
            

;(and
                ;(not (filter #(re-find #"Resume*" (first %))
                  ;       (cpn/get-enabled-transitions @currentcpn))))
