(ns crdt.sets.lww
  (:require [clojure.set :as set]
            [clojure.core :as core]
            [crdt.sets.lww :as lww]))

(def now #(System/currentTimeMillis))

(defn create-lww-element-set-atom
  "Creates LWW init object"
  []
  (atom {:a {}, :r {}}))


(defn lookup
  "Seeks for the element in `s` and returns timestamp if found"
  [s e]
  (when-let [at (-> s :a (get e))]
    (if-let [rt (-> s :r (get e))]
      (when (< rt at)
        at)
      at)))


(defn add
  "Adds object to set. If no `timestamp` set, uses current time"
  ([*s e] (add *s e (now)))
  ([*s e timestamp]
   (when-not (lookup @*s e)
     (swap! *s update :a
            core/merge {e timestamp}))))


(defn remove
  "Removes object from set. If no `timestamp` set, uses current time"
  ([*s e] (remove *s e (now)))
  ([*s e timestamp]
   (when (lookup @*s e)
     (swap! *s update :r
            core/merge {e timestamp}))))


(defn compare
  "Compares `s` and `t` LWW sets according to theory rules"
  [s t]
  (let [{sa :a sr :r} s
        {ta :a tr :r} t]
    ;; why or..?
    (or (set/subset? (set sa) (set ta))
        (set/subset? (set ta) (set tr)))))


(defn- max-timestamp-reducer [result [element timestamp]]
  (merge-with #(if (> %1 %2) %1 %2)
              result {element timestamp}))


(defn merge-elements-map [m1 m2]
  (->> (set m1)
       (set/union (set m2))
       (reduce max-timestamp-reducer {})))


(defn merge [s t]
  {:a (merge-elements-map (:a s) (:a t))
   :r (merge-elements-map (:r s) (:r t))})
