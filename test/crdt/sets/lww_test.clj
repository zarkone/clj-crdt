(ns crdt.sets.lww-test
  (:require [clojure.test :refer :all]
            [crdt.sets.lww :as lww]))

;; ----------------- -------------------------- ------------
;; When tests evaluates, it's sometimes the case that first and
;; next operatiotion happen at the same time, i.e. shares the same
;; timestamp. There's an option to use nanoseconds precision but
;; we can't actually rely on it because it doesn't guarantee accurasy --
;; it guaranties only precision due to restrictions of OS inner mechanics.
;;
;; So we have to use this hack wich adds artificient delay without actual
;; slow down of code evaluation.
(def delta (atom 0))

(defn inc-now []
  (swap! delta #(+ 5 %))
  (+ (System/currentTimeMillis) @delta))

(defn now-fixture [f]
  (with-redefs [lww/now #(inc-now)]
    (f)))

(use-fixtures :each now-fixture)


(deftest simple-add
  (testing "Simple add"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set lww/add :foo)
      (is
       (lww/lookup @lww-set :foo)))
    ))

(deftest lookup-empty
  (testing "Lookup in empty set"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (is
       (not (lww/lookup @lww-set :foo))))
    ))

(deftest empty
  (testing "Create set and test if it empty"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (is
       (not (lww/lookup @lww-set :foo))))
    ))

(deftest remove-empty
  (testing "Remove item from empty set"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set lww/remove :doesnt-exist)
      (is
       (lww/empty? @lww-set)))
    ))

(deftest double-remove
  (testing "Remove element from set multiple times"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set
             #(-> %
                  (lww/add :doesnt-exist)
                  (lww/add :exist)
                  (lww/remove :doesnt-exist)
                  (lww/remove :doesnt-exist)
                  ))
      (is
       (= #{:exist} (lww/to-set @lww-set))))
    ))

(deftest double-add
  (testing "Remove element from set multiple times"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set
             #(-> %
                  (lww/add :foo)
                  (lww/remove :foo)
                  (lww/add :foo)
                  ))
      (is
       (lww/lookup @lww-set :foo)))
    ))

(deftest lookup-removed
  (testing "Lookup removed item"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set
             #(-> %
                  (lww/add :foo)
                  (lww/remove :foo)
                  ))
      (is
       (not (lww/lookup @lww-set :foo)))
      )))

(deftest lookup-removed
  (testing "Lookup removed item"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set
             #(-> %
                  (lww/add :foo)
                  (lww/remove :foo)
                  (lww/add :foo)
                  ))
      (is
       (lww/lookup @lww-set :foo)))
    ))

(deftest lww-to-set-convert
  (testing "Converting to basic set"
    (let [lww-set (atom (lww/create-lww-element-set))]
      (swap! lww-set
             #(-> %
                  (lww/add :foo)
                  (lww/remove :foo)
                  (lww/add :foo)
                  (lww/add :wbar)
                  (lww/add :baz)
                  (lww/add :baz2)
                  (lww/remove :baz)
                  ))
      (is
       (= (lww/to-set @lww-set)
          #{:foo :baz2 :wbar})))))


(deftest merge-sets
  (testing "Do random lww-set add/remove and compare results with Clojure set"
    (let [simple-set (atom #{})
          lww-sets-count 42
          lww-sets-atom (->> (repeatedly lww-sets-count lww/create-lww-element-set)
                             (vec)
                             (atom))]
      (doseq [_ (range 100)]
        (let [operation (if (even? (rand-int 10000)) :add :rem)
              operand (if (= :add operation)
                        (rand)
                        (second @simple-set))]
          (case operation
            :add (do
                   (swap! simple-set conj operand)
                   (swap! lww-sets-atom
                          update-in [(rand-int lww-sets-count)]
                          #(lww/add % operand)))
            :rem (do
                   (swap! simple-set disj operand)
                   (swap! lww-sets-atom
                          (fn [lww-sets]
                            (->> lww-sets
                                 (map #(lww/remove % operand))
                                 (vec))))))))
      (is
       (= @simple-set
          (->> @lww-sets-atom
               (reduce lww/merge)
               (lww/to-set)))))))
