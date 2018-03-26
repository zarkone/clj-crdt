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
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :foo)
      (is
       (lww/lookup @s :foo)))
    ))

(deftest lookup-empty
  (testing "Lookup in empty set"
    (let [s (lww/create-lww-element-set-atom)]
      (is
       (not (lww/lookup @s :foo))))
    ))

(deftest empty
  (testing "Create set and test if it empty"
    (let [s (lww/create-lww-element-set-atom)]
      (is
       (not (lww/lookup @s :foo))))
    ))

(deftest remove-empty
  (testing "Remove item from empty set"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/remove s :doesnt-exist)
      (is
       (lww/empty? @s)))
    ))

(deftest double-remove
  (testing "Remove element from set multiple times"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :doesnt-exist)
      (lww/add s :exist)
      (lww/remove s :doesnt-exist)
      (lww/remove s :doesnt-exist)
      (is
       (= #{:exist} (lww/lww->set @s))))
    ))

(deftest double-add
  (testing "Remove element from set multiple times"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :foo)
      (lww/remove s :foo)
      (lww/add s :foo)
      (is
       (lww/lookup @s :foo)))
    ))

(deftest lookup-removed
  (testing "Lookup removed item"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :foo)
      (lww/remove s :foo)
      (is
       (not (lww/lookup @s :foo)))
      )))

(deftest lookup-removed
  (testing "Lookup removed item"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :foo)
      (lww/remove s :foo)
      (lww/add s :foo)
      (is
       (lww/lookup @s :foo)))
    ))

(deftest lww-to-set-convert
  (testing "Converting to basic set"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :foo)
      (lww/remove s :foo)
      (lww/add s :foo)
      (lww/add s :wbar)
      (lww/add s :baz)
      (lww/add s :baz2)
      (lww/remove s :baz)
      (is
       (= (lww/lww->set @s)
          #{:foo :baz2 :wbar})))))


;; (deftest compare-sets)

;; (deftest merge-sets)

;; generative testing?
