(ns crdt.sets.lww-test
  (:require [clojure.test :refer :all]
            [crdt.sets.lww :as lww]))

#_(let [s (lww/create-lww-element-set-atom)]
  (lww/add s :foo)
  ;; (Thread/sleep 100)
  ;; (lww/add s :bar)
  ;; (Thread/sleep 100)
  ;; (lww/add s :foo)
  @s
  )


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

(deftest lookup-removed
  (testing "Lookup removed item"
    (let [s (lww/create-lww-element-set-atom)]
      (lww/add s :foo)
      (lww/remove s :foo)
      (is
       (not (lww/lookup @s :foo))))
    ))
