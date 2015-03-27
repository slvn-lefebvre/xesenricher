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


;; http://ravi.pckl.me/short/functional-xml-editing-using-zippers-in-clojure/
(defn tree-edit
  "Take a zipper, a function that matches a pattern in the tree,
   and a function that edits the current location in the tree.  Examine the tree
   nodes in depth-first order, determine whether the matcher matches, and if so
   apply the editor."
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

(defn new-oponleave [timestamp]
  (xml/element :event {}
     (xml/element :date {:key "time:timestamp", :value timestamp}),
     (xml/element :string {:key "concept:name", :value "opManager"}),
     (xml/element :string {:key "lifecycleP:transition", :value "on-leave"})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [xml-data (xml/parse-str (slurp (first args)))
        injected (tree-edit (zip/xml-zip xml-data) match-suspended? new-oponleave)]
    (spit  "./resources/result.xml" (xml/indent-str injected))))

