(ns crdt.sets.lww
  (:require [clojure.set :as set]
            [clojure.core :as core]
            [crdt.sets.lww :as lww]))

(def now #(System/currentTimeMillis))

(defn create-lww-element-set-atom
  "Creates LWW init object"
  []
  (atom {:add {}, :rem {}}))


(defn lookup
  "Seeks for the element in `s` and returns timestamp if found"
  [lww-set element]
  (when-let [add-timestamp (get-in lww-set [:add element]) ]
    (if-let [rem-timestamp (get-in lww-set [:rem element])]
      (when (< rem-timestamp add-timestamp)
        add-timestamp)
      add-timestamp)))


(defn add
  "Adds object to set. If no `timestamp` set, uses current time"
  ([*lww-set element] (add *lww-set element (now)))
  ([*lww-set element timestamp]
   (swap! *lww-set update :add
          core/merge {element timestamp})))


(defn remove
  "Removes object from set. If no `timestamp` set, uses current time"
  ([*lww-set element] (remove *lww-set element (now)))
  ([*lww-set element timestamp]
   (when (lookup @*lww-set element)
     (swap! *lww-set update :rem
            core/merge {element timestamp}))))


(defn compare
  "Compares `lww-set1` and `lww-set2` LWW sets according to theory rules"
  [lww-set1 lww-set2]
  (let [{lww-set1:add :add, lww-set1:rem :rem} lww-set1
        {lww-set2:add :add, lww-set2:rem :rem} lww-set2]
    (or (set/subset? (set lww-set1:add) (set lww-set2:add))
        (set/subset? (set lww-set1:rem) (set lww-set2:rem)))))


(defn- merge-elements-map [map1 map2]
  (->> (set map1)
       (set/union (set map2))
       (reduce (fn [result [element timestamp]]
                 (merge-with max result {element timestamp})))))


(defn merge
  "Merges `lww-set1` and `lww-set2` LWW sets according to theory rules"
  [lww-set1 lww-set2]
  {:add (merge-elements-map (:add lww-set1) (:add lww-set2))
   :rem (merge-elements-map (:rem lww-set1) (:rem lww-set2))})


(defn lww->set
  "Returns current (actual) items of LWW set `lww-set` as Clojure set"
  [lww-set]
  (->> (get lww-set :add)
       (keys)
       (filter #(lookup lww-set %))
       (set)))


(defn empty? [lww-set]
  (-> (lww->set lww-set)
      (clojure.core/empty?)))
