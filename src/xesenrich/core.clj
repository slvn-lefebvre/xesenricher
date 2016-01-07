(ns xesenrich.core
  (:gen-class)
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip :as c-d-zip]
            [clojure.data.zip.xml :as zx]
            [clojure.java.io :as io]
            [clojure.pprint :as p]))


(defn get-timestamp [loc]
  (:value (:attrs (first (:content (zip/node loc))))))
(defn get-executors [loc]
  (:value (:attrs (first (:content (zip/node loc))))))

(defn new-oponleave [timestamp]
  (xml/element :event {}
     (xml/element :date {:key "time:timestamp", :value timestamp}),
     (xml/element :string {:key "concept:name", :value "opManager"}),
     (xml/element :string {:key "lifecycleP:transition", :value "on-leave"})))

;; http://ravi.pckl.me/short/functional-xml-editing-using-zippers-in-clojure/
(defn tree-edit
  "Take a zipper, a function that matches a pattern in the tree,
   and a function that edits the current location in the tree.  Examine the tree
   nodes in depth-first order, determine whether the matcher matches, and if so
   applies the obstacle generator."
  [zipper matcher obstacle-gen]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if (matcher loc)
        (let [new-loc (zip/insert-left loc (obstacle-gen (get-timestamp loc)))]
            (recur (zip/next new-loc)))
        (recur (zip/next loc))))))

;; match predicate, suspended tasks
(defn match-suspended? [loc]
  (let [content (:content (zip/node loc))]
    (reduce (fn [b elem]
              (or b (let [attrs (:attrs elem)]
                      (and
                       (= (:key attrs) "lifecycleT:transition")
                       (= (:value attrs) "suspended")))))
            false
            content)))


;evt b is defined as a 5-tuple < id, inst, c, s, e > where:
;id is a logged event identifier, inst is a BP instance identifier, c is a BP
;component (either task, person, or machine); s is an execution state of c; e is the set of task executors ({p} and/or {m}) when c refers to t (otherwise e is set to null)
;and t is the task for which executors are assigned to when c refers to p and/or
;m (otherwise t is set to null).
(defrecord Bevent [id instance category timestamp component state executors task])


;evt s illustrates how the enterprise networks are used to address obstacles that
;hinder the execution of a BP instance. evt s is also defined as a 6-tuple
;< id, tg, en, c n , rel, out > where: id is a logged event identifier; tg is a trigger
;linked to a business event identifier; en is an enterprise network involved in
;addressing an obstacle; c n is a business process component (t, p, or m) that
;constitutes an obstacle as per the “condition” column in Table 1; *rel* is the
;social relation used to solve the obstacle; and *out* is the outcome of using *en* in terms of replacement task, person, and/or machine.
(defrecord Sevent [id trigger net component  relation])

(defrecord Obstacle [id instance component category executors])

(defn weight
  [task executors]
  true)
(defn expected? [ts c] true)

(defn detect-obstacle
  "Returns an obstacle if the event is considered to be one, or nil"
  [event blog] ;Bevent
  (let [make-obstacle (partial Obstacle (:id event) (:instance event))]
  (case (:category event)
    :task (if (= (:state event) :prepared)
            (if (= (count (:executors event)) 0)
              (make-obstacle (:component event) :lackexecutors nil)
              
              (if (= true (weight (:task event) (:executors event)))
                nil
                (make-obstacle (:component event) :inadequacy                       (:executors event))))
            nil)
    (:person :machine) (if (or (= :on-leave (:state event)) ; unavailability
                               (= :serviced (:state event)))
                         (let [suspensions
                               (clojure.set/select #(and (= (:task event) (:component %))
                                                         (= :activated (:state %)))
                                                   blog)]
                           (make-obstacle (:task event)
                                          (if (expected? (:timestamp event)
                                                         (:component event))
                                            :expected :unexpected)
                                          [(:component event)]))
                         nil))))
(defrecord solution [task, cn , en, rel, out])
(defn build-repository
  [blog slog repo]
  (let [exceptions (clojure.set/select #(and (= :suspended (:state %))
                                             (= :task (:category %)))
                                       blog)]
    ()                                           
  )
  

;; Idea is to have one log per person, machine, etc... with matching timestamps, eventually
;; this way you can detect "happen before relationships", but only keep interesting obstacles.
;; print out stats about tasks, machines and persons
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [xml-data (xml/parse-str (slurp (first args)))
        injected (tree-edit (zip/xml-zip xml-data) match-suspended? new-oponleave)]
    (spit  "./resources/result.xml" (xml/indent-str injected))))

