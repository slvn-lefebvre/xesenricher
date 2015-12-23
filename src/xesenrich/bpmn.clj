(ns xesenrich.bpmn
  (:require [xesenrich.cpn :as cpn]
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


(defrecord Person [id])
(defrecord Request [id status])
(defrecord Machine [id])


;; Activity net names of places and transition
(def namesmap {:prepared "Prepared",
               :activated "Activated",
               :idle "Idle",
               :suspended "Suspended",
               :on-leave "On-leave",
               :done "Done",
               :lackexecutors "LackExecutors",
               :activation "Activation",
               :termination "Termination",
               :resume "Resume",
               :unavailability, "Unavailability"})

               
;; Returns a cpn representing a task. Each place and transition is identified by concatenating the network id with the name of the place / transition
;; this is required to avoid merging all nets in a single one when we build the final net.
;; However, Idle and on-leave places are shared between activities in the same lane
(defn make-activitynet
  "Returns a CPN representing a human activity mapped to bpmn tasks"
  [id lane_id]
  (let [task #{Request}
        person #{Person}
        taskperson #{Request, Person}
        act-names (into {} (for [[k v] namesmap] [k (str v id)]))] ; giving each place and transition a unique name is useful for merging cpns
  (cpn/CPN.
   id,
   {
    (:prepared act-names) (cpn/Place. (:prepared namesmap) task []),
    (:activated act-names) (cpn/Place. (:activated namesmap) taskperson []),
    (str (:idle namesmap) lane_id)  (cpn/Place. (:idle namesmap) person []),
    (str (:on-leave namesmap) lane_id)  (cpn/Place. (:on-leave namesmap) person []),
    (:suspended act-names)  (cpn/Place. (:suspended namesmap) task []),
    (:done act-names)  (cpn/Place. (:done namesmap) task [])
    },
   {
    (:lackexecutors act-names) (cpn/Transition. (:lackexecutors namesmap) (constantly true) #{:t} []),
    (:activation act-names)  (cpn/Transition. (:activation namesmap) (constantly true) #{:t, :p} []), ; must check for task and person adequation wrt network
    (:termination act-names) (cpn/Transition. (:termination namesmap) (constantly true) #{:t, :p} []),
    (:resume act-names)  (cpn/Transition. (:resume namesmap) (constantly true) #{:t, :p} []),
    (:unavailability act-names)  (cpn/Transition. (:unavailability namesmap) (constantly true) #{:t, :p} [])
     },
   [
    (cpn/Arc. (:prepared act-names) (:activation act-names) #(-> {:t (first %)}) false),
    (cpn/Arc. (:prepared act-names) (:lackexecutors act-names) #(-> {:t (first %)}) false),
    (cpn/Arc. (:activation act-names) (:activated act-names) #(identity %) false),
    (cpn/Arc. (:activated act-names) (:termination act-names) #(-> {:t (first %), :p  (second %)}) false),
    (cpn/Arc. (:termination act-names) (str (:idle namesmap) lane_id) #(-> {:p (:p %)}) false),
    (cpn/Arc. (:termination act-names) (:done act-names) #(-> {:t (:t %)}) false),
    (cpn/Arc. (:activated act-names) (:unavailability act-names) #(-> {:t (first %), :p  (second %)}) false),
    (cpn/Arc. (:unavailability act-names) (:suspended act-names) #(-> {:t (first %)}) false),
    (cpn/Arc. (:suspended act-names) (:resume act-names) #(-> {:t %}) false),
    (cpn/Arc. (str (:idle namesmap) lane_id) (:resume act-names) #(-> {:p %}) false),
    (cpn/Arc. (str (:idle namesmap) lane_id) (:activation act-names) #(-> {:p (first %)}) false),
    (cpn/Arc. (str (:idle namesmap) lane_id) (:lackexecutors act-names) #(-> {:p %}) true),
    (cpn/Arc. (:resume act-names) (:activated act-names) #(merge (first %) (second %)) false),
    (cpn/Arc. (:lackexecutors act-names) (:suspended act-names) #(identity %) false)
    ]
              
    )))


(defn make-start-net
  "Returns a CPN modelling a bpmn start point"
  [id out] ;out must be the act-name of the input place of the following net... 
  (cpn/CPN. id,
            {"start" (cpn/Place. "start" #{Request} [])
             (str (:prepared namesmap) out) (cpn/Place. (:prepared namesmap) #{Number} [])
             }
            {id (cpn/Transition. id (constantly true) #{:r} [])}
            [
             (cpn/Arc. "start" id #(-> {:r (first %)}) false)
             (cpn/Arc. id (str (:prepared namesmap) out) #(identity %) false)
             ]))

(defn make-stop-net
  [id in] ; in must be the act name of the preceding net.
    (cpn/CPN. id,
            {"stop" (cpn/Place. "stop" #{Request} [])
             (str (:done namesmap) in) (cpn/Place. (:done namesmap) #{Number} [])
             }
            {id (cpn/Transition. id (constantly true) #{:r} [])}
            [
             (cpn/Arc. (str (:done namesmap) in) id #(-> {:r (first %)}) false)
             (cpn/Arc. id "stop" #(identity %) false)
             ]))

(defn make-decision-net
  "Returns a CPN connecting a decision fork made by two subsequent flows"
  [name]
  (let [ in (str (:prepared namesmap) name)
         out (str (:done namesmap) name)]
  (cpn/CPN.
   name
   { in (cpn/Place. (:prepared namesmap) #{Request} []),
     out  (cpn/Place. (:done namesmap) #{Request} [])
     },
   { name (cpn/Transition. name (constantly true) #{:r} []) },
   [
    (cpn/Arc. in name #(-> {:r (first %)}) false)
    (cpn/Arc. name out #(identity %) false)
    ])))
  

(defn parse-forks
  "Returns a list of forks CPNs"
  [data]
  (into {} (for [d (zx/xml-> data :process :exclusiveGateway)]
             [(zx/attr d :id)
              (make-decision-net (zx/attr d :id))])))

(defn make-flownet [in name out cond]
  (cpn/CPN.
   name
   { (str (:done namesmap) in) (cpn/Place. (:done namesmap) #{Request} []),
     (str (:prepared namesmap) out) (cpn/Place. (:prepared namesmap) #{Request} [])}
   ;if a condititon expression is required then the transition is a fun on request status
   { name (cpn/Transition. name (if (= "" cond) 
                                  (constantly true)
                                  #(= (:status (:r %)))) #{:r} [])}
   [ (cpn/Arc.  (str (:done namesmap) in) name #(-> {:r (first %)}) false)
     (cpn/Arc. name (str (:prepared namesmap) out) #(identity %) false) ]))

(defn parse-flows
  "Returns a net connecting two nets. Adds an Arc condition if needed"
  [data]
  (into {} (for [f (zx/xml-> data :process :sequenceFlow)]
             [(zx/attr f :id)
              (make-flownet (zx/attr f :sourceRef)
                  (zx/attr f :id)
                  (zx/attr f :targetRef)
                  (zx/text f))])))


(def root (-> "/home/slefebvr/clojure/xesenrich/resources/courseScheduling2.bpmn" io/reader xml/parse zip/xml-zip))


(defrecord Lane [id name])
(defn parse-persons 
  "Returns a list of Persons from the BP swimlanes definition. Activities in the same swimlane share their persons. Henceforth the Idle,Onleave and corresponding transistions must be shared"
  [data]
  (into {}
        (mapcat identity
                (for [p (zx/xml-> data :process :laneSet :lane)]
                  (let [lane (Lane. (zx/attr p :id) (zx/attr p :name))]
                    (for [n (zx/xml-> p :flowNodeRef)] [(zx/text n) lane]))))))

          
;; <startEvent id="_13" isInterrupting="true" name="Request" parallelMultiple="false">      <outgoing>_15</outgoing> </startEvent>


(defn parse-activities
  "Returns a map of CPNs representing each Task in the bpmn description"
  [data lanemap]
  (into {} (for [t  (zx/xml-> data :process :userTask)]
             [(zx/attr t :id) (make-activitynet
                               (zx/attr t :id)
                               (:id (get lanemap (zx/attr t :id)))) ]
             )))

;need to find the corresponding net and its name / id combination.
(defn parse-start
  "Returns a list of start events"
  [data]
  (into {} (for [e (zx/xml-> data :process :startEvent)]
    [ (zx/attr e :id) (make-start-net  (zx/attr e :id)
                     (first (zx/xml-> e :outgoing zx/text)))
      ])))

(defn parse-end
  "Returns a map of end events"
  [data]
  (into {} (for [e (zx/xml-> data :process :endEvent)]
    [ (zx/attr e :id) (make-stop-net  (zx/attr e :id)
                     (first (zx/xml-> e :incoming zx/text)))
      ])))
  

(defn build-model
  "takes a path to bpmn file"
  [path]
  (let [data  (-> path  io/reader xml/parse zip/xml-zip)
        lanemap (parse-persons data)
        nets (merge (parse-activities data lanemap)
                    (parse-start data)
                    (parse-end data)
               (parse-flows data)
               (parse-forks data))]
    (reduce  cpn/merge-cpn (vals nets))))

