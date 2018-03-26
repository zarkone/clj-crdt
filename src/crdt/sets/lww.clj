(ns crdt.sets.lww
  (:require [clojure.set :as set]))


(defn create-lww-element-set-atom []
  (atom {:a {}, :r {}}))


(defn lookup [s e]
  (when-let [at (-> s :a (get e))]
    (if-let [rt (-> s :r (get e))]
      (< rt at)
      true)))


(defn add
  ([*s e] (add *s e (System/currentTimeMillis)))
  ([*s e timestamp]
   (when-not (lookup @*s e)
     (swap! *s update :a
            merge {e timestamp}))))


(defn remove
  ([*s e] (remove *s e (System/currentTimeMillis)))
  ([*s e timestamp]
   (when (lookup @*s e)
     (swap! *s update :r
            merge {e timestamp}))))


(defn compare [s t]
  (let [{sa :a sr :r} s
        {ta :a tr :r} t]
    ;; why or..?
    (or (set/subset? (set sa) (set ta))
        (set/subset? (set ta) (set tr)))))


(defn- max-timestamp-reducer [result [element timestamp]]
  (merge-with #(if (> %1 %2) %1 %2)
              result {element timestamp}))


(defn merge-element-timestamp-map [m1 m2]
  (->> (set m1)
       (set/union (set m2))
       (reduce max-timestamp-reducer {})))


(defn merge [s t]
  {:a (merge-element-timestamp-map (:a s) (:a t))
   :r (merge-element-timestamp-map (:r s) (:r t))})
