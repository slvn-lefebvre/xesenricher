(ns xesenrich.cpn
  (:require [clojure.math.combinatorics :as combo]))

(defrecord Arc [in out exp inhibitor])
(defrecord Place [name colorset marking])
(defrecord Transition [name guard variables bindings])
(defrecord CPN [id places transitions arcs])

; http://blog.jayfields.com/2011/01/clojure-select-keys-select-values-and.html
(def select-values (comp vals select-keys))

(defn print-markings [cpnet]
  (doseq [[n p] (:places cpnet)]
    (prn (str n " " (:marking p)))))

(defn get-in-arcs [cpn node] (filter #(= (:out %) node) (:arcs cpn)))
(defn get-out-arcs [cpn node] (filter #(= (:in %) node) (:arcs cpn)))


(defn get-output-places [cpn trans]
  (if (contains? (:transitions cpn) trans)
    (let [places (map #(:out %) (get-out-arcs cpn trans))]
      (select-values (:places cpn) places))
    nil))

(defn get-input-places
  "Filters the names of input places of the given transition, returns nil if it is not transition"
  [cpn transname]
  (if (contains? (:transitions cpn) transname)
    (let [places (map #(:in %) (get-in-arcs cpn transname))]
      (select-values (:places cpn) places))
    nil))

(defn get-tokens
  "returns the current tokens of the specified place"
  [cpn placename]
  (let  [place (get  (:places cpn) placename)]
    (if (nil? place) nil (:marking place))))

;to be removed
(defn get-binding
  "Calls each input arc expression of the specified transition returns the full binding of variables"
  [cpn trans]
  (if (contains? (:transitions cpn) trans)
    (let [arcs (get-in-arcs cpn trans)]
      (into {}
            (map #((:exp %)
                   (first (get-tokens cpn (:in %))))
                 arcs)))))
(defn get-bindings
  "Calls each input arc expression of the specified transition returns the full binding of variables in the form {placename bindings, placename2 bindings2}"
  [cpn trans]
  (if (contains? (:transitions cpn) trans)
    (let [arcs (get-in-arcs cpn trans)]
      (into {} (map
                (fn [arc] {(:in arc) (map #((:exp arc) %) (get-tokens cpn (:in arc)))})
                arcs)))))

(defn bind-arc
  [cpn arc]
  (map #((:exp arc) %) (get-tokens cpn (:in arc)))
  )

(comment
  Want to get a map of "pname [indexes]" of matching tokens
  Token matches are required when bindings have same variable
  If bindings have the same variable b, then b must be bound to the same value in all arcs
  1. Find the smallest binding
  2. Filter out all bindings not containing any of these bindings
  return a map of place name token index or nil if no match is found)

(defn merge-tpairs
  [vars t1 t2]
  (into {}
        (map
           #(-> {% (let [v1 (get t1 %)
                         v2 (get t2 %)]
                     (if (= v1 v2)
                       v1
                       (if (nil? v1)
                         v2
                         (if (nil? v2) v1 :mismatch))))})
           vars)))

(defn merge-tokens
  [vars t & tokens] (reduce (partial merge-tpairs vars) t tokens))


(defn match-bindings
  "Generates all combinations of tokens and returns a list of matching bindings"
  [cpn trans]
  (remove #(some #{:mismatch} (vals %))
   
   (let [bindings (apply
                   combo/cartesian-product
                   (vals (get-bindings cpn trans)))
         vars (get-in cpn  [:transitions trans :variables])]
     
     (map #(apply (partial merge-tokens vars) %)
          bindings))))

(defn inhibited? [cpn trans]
  (let [iarcs (filter #(:inhibitor %) (get-in-arcs cpn trans))]
    (< 0 (reduce + (map #(count (bind-arc cpn %)) iarcs)))))

(defn enabled?
  "Checks the transition guards against a binding for transition trans"
  [cpn trans bdg]
  (and 
   ((-> cpn :transitions (#(get % trans)) :guard)  bdg)
   (not (inhibited? cpn trans))))


(defn remove-matching-token
  [bdngvals tokens]
    (let [tok (first (filter #(not= nil  (some bdngvals %)) tokens))
          [n m] (split-with #(not= tok %) tokens)]
        (vec (concat n (rest m)))))

(defn remove-tokens [cpn trans binding]
  (let [arcs (get-in-arcs cpn trans) ;; going through arcs to get appropriate name for places
        rmv (partial remove-matching-token (into #{} (vals binding)))]
    (reduce (fn [cpn a]
              (assoc-in cpn [:places (:in a) :marking]
                        (rmv (get-in cpn [:places (:in a) :marking]))))
            cpn arcs)))

(defn get-enabled-bindings
  [cpn trans bdgs]
  (filter #(enabled? cpn trans %) bdgs))
  
(defn add-tokens [cpn trans bdg]
  (let [arcs (get-out-arcs cpn trans)]
    (println bdg)
    (reduce (fn [cpn a]
              (assoc-in cpn [:places (:out a) :marking]
                        (vec (conj (get-in cpn [:places (:out a) :marking])
                                   (do
                                     (println (str (:in a) (:out a)))
                                     (-> ((:exp a) bdg) vals vec))))))
                                   cpn arcs)))

(defn fire
   "Tries to fire the specified transition, and returns the resulting CPN state. If the transition is not enabled, returns false"
   [cpn trans]
  (let [bindings (get-enabled-bindings cpn trans (match-bindings cpn trans))]
    (if (< 0 (count bindings))      
      (add-tokens (remove-tokens cpn trans (first bindings)) trans (first bindings))
      cpn
      )))

(defn tenabled?
   [cpn trans]
   (< 0 (count  (get-enabled-bindings cpn trans (match-bindings cpn trans)))))
  
(defn get-enabled-transitions
  "returns vector of enabled transitions ids"
  [cpn]
  (let [etrans (filter #(tenabled? cpn %) (keys (:transitions cpn)))]
    (for [t etrans]
      [t (get-in cpn [:transitions t])])))
    

(defn random-fire
  "Fires an enabled transition chosen randomly"
  [cpn time transprob]
  (reduce (fn [net t]
            (let [rnd (rand)]
              (println t)
              (if (< rnd (get transprob (first t)))
                (fire cpn (first t))
                net)
              )) cpn (get-enabled-transitions cpn)))


; to avoid ArityException    ref: http://www.markhneedham.com/blog/2013/09/23/clojure-anonymous-functions-using-short-notation-and-the-arityexception-wrong-number-of-args-0-passed-to-persistentvector/

(defn merge-cpn
  "This function returns a cpn that is the merger of cpn1 and cpn2. Places with the same name will be merged together, arcs and transitions are preserved as is."
  [cpn1 cpn2]
  (CPN. (str (:id cpn1) (:id cpn2))
        (into (:places cpn1) (:places cpn2))
        (into (:transitions cpn1) (:transitions cpn2))
        (into (:arcs cpn1) (:arcs cpn2))))



(def simpleaddnet
  (CPN.
   "addnet",
   {"in" (Place. "in" #{} [[8],[1],[2],[3],[3],[8],[8]]),
    "out" (Place. "out" #{} [])},
   {"Add" (Transition. "Add" #(if (< 5 (:x %)) true false) #{:x} [])},
    [ (Arc. "in"  "Add" #(-> {:x (first %)}) false),
    (Arc. "Add" "out" #(-> {:x (inc (:x %))}) false)]
    ))

(def simpleprotocolnet
  (let [message #{Number,String},
        num #{Number},
        text #{String}]
(CPN.
   "simpleprotocolnet",
           {"ToSend" (Place. "ToSend" message [[1,"COL"], [2, "OUR"],[3,"ED"]]),
            "A" (Place. "A" message []),
            "NextSend" (Place. "NextSend" num [[1]])},
           
           {"SendPacket" (Transition. "SendPacket" (constantly true) #{:n :d} [])},
           
           [
            (Arc. "ToSend" "SendPacket" #(-> {:n (first %), :d (second %)}) false),
            (Arc. "SendPacket" "ToSend" #(identity %) false),
            (Arc. "SendPacket" "A" #(identity %) false),
            (Arc. "NextSend" "SendPacket" #(-> {:n (first %)}) false),
            (Arc. "SendPacket" "NextSend" #(-> {:n (:n %)}) false)
            ])))
           
            


(comment
(def simpleprotocolnet
  (let [message #{Number,String},
        num #{Number},
        text #{String}]
  (CPN.
   "simpleprotocolnet",
           {"ToSend" (Place. "ToSend" message [[1,"COL"], [2, "OUR"],[3,"ED"]]),
                  "A" (Place. "A" message []),
                  "B" (Place. "B" message []),
                  "DataRec" (Place. "DataRec" text [""]),
                  "NextRec" (Place. "NextRec" num [1]),
                  "C" (Place. "C" message []),
                  "D" (Place. "D" message []),
                  "NextSend" (Place. "NextSend" num [1])},

           {"SendPacket" (Transition. "SendPacket" (constantly true) #{:n :d} []),
            "TransPacket" (Transition. "TransPacket" (constantly true) #{:n :d} []),
            "ReceivePacket" (Transition. "ReceivePacket"  (constantly true) #{:k :n :d :data} []),
            "TransmitAck" (Transition. "TransmitAck" (constantly true) #{:n} []),
            "ReceiveAck" (Transition. "ReceiveAck" (constantly true) #{:n} [])},
           [
            (Arc. "ToSend" "SendPacket" #(-> {:n (first %), :d (second %)})),
            (Arc. "SendPacket" "ToSend" #(identity %)),
            (Arc. "Sendpacket" "A" #(identity %)),
                                        ;from there, need to rebind when out of place
            (Arc. "A" "TransPacket" #(-> {:n (first %), :d (second %)})),  
            (Arc. "TransPacket" "B" #(identity %)),
            (Arc. "B" "ReceivePacket" #(-> {:n (first %), :d (second %)})),
            (Arc. "ReceivePacket" "DataRec" #(if (= (:n %) (:k %))
                                               {:data (str (:data %) (:d %)) }
                                               {:data (:data %)})),
            
            (Arc. "DataRec" "ReceivePacket" #(-> {:data  %})),

            (Arc. "ReceivePacket" "NextRec" #(if (= (:n %) (:k %))
                                                     {:k (inc (:k %))}
                                                     {:k (:k %)})),
            
            (Arc. "NextRec" "ReceivePacket" #(-> {:k %})),

            (Arc. "ReceivePacket" "C" #(if (= (:n %) (:k %))
                                                     {:k (inc (:k %))}
                                                     {:k (:k %)})),
            (Arc. "C" "TransmitAck" #(-> {:n %})),
            (Arc. "TransmitAck" "D" #(identity %)),
            (Arc. "D" "ReceiveAck" #(-> {:n %})),
            (Arc. "ReceiveAck" "NextSend" #(identity %)),
            (Arc. "NextSend" "ReceiveAck" #(-> {:k %})),
            (Arc. "NextSend" "SendPacket" #(-> {:n %})),
            (Arc. "SendPacket" "NextSend" #(-> {:n (first %)}))
            ])))
)

