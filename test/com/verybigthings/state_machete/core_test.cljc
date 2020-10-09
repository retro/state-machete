(ns com.verybigthings.state-machete.core-test
  (:require [clojure.test :refer :all]
            [com.verybigthings.state-machete.core :as c]))

(defn log [& forms]
  (doseq [f forms]
    (println (with-out-str (clojure.pprint/pprint f)))))

(defn assert-states [fsm initial-expected-state & events-expected-states]
  (is (= initial-expected-state (c/get-active-atomic-states fsm)))
  (loop [events-expected-states events-expected-states
         fsm fsm]
    (let [[[event expected-state] & rest-events-expected-states] events-expected-states
          fsm' (c/trigger fsm event)]
      (is (= expected-state (c/get-active-atomic-states fsm')))
      (if (seq rest-events-expected-states)
        (recur rest-events-expected-states fsm')
        fsm'))))

(defn make-fsm
  ([fsm] (make-fsm fsm {}))
  ([fsm context]
   (-> fsm (c/compile context) c/start)))

(deftest basic-0
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state {:fsm/id :a}]])]
    (is (= #{:a} (c/get-active-atomic-states fsm)))
    (is (= #{:fsm/root :a} (c/get-active-states fsm)))))

(deftest basic-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state {:fsm/id :a}
            [:fsm/transition {:fsm.transition/target :b :fsm.transition/event :t}]]
           [:fsm/state {:fsm/id :b}]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b}])))

(deftest basic-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event :t2}]]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b}]
      [{:fsm/event :t2} #{:c}])))

(deftest document-order-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state {:fsm/id :a}
            [:fsm/transition #:fsm.transition{:target :b :event :t}]
            [:fsm/transition #:fsm.transition{:target :c :event :t}]]
           [:fsm/state {:fsm/id :b}]
           [:fsm/state {:fsm/id :c}]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b}])))

(deftest parallel-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel {:fsm/id :p}
            [:fsm/state {:fsm/id :a}]
            [:fsm/state {:fsm/id :b}]]])]
    (is (= #{:a :b} (c/get-active-atomic-states fsm)))))

(deftest parallel-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel {:fsm/id :p}
            [:fsm/state {:fsm/id :a :fsm/initial :a1}
             [:fsm/state {:fsm/id :a1}
              [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
             [:fsm/state {:fsm/id :a2}]]
            [:fsm/state {:fsm/id :b :fsm/initial :b1}
             [:fsm/state {:fsm/id :b1}
              [:fsm/transition #:fsm.transition{:event :t :target :b2}]]
             [:fsm/state {:fsm/id :b2}]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a2 :b2}])))

(deftest parallel-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel {:fsm/id :p1}
            [:fsm/state {:fsm/id :s1 :fsm/initial :p2}
             [:fsm/parallel {:fsm/id :p2}
              [:fsm/state {:fsm/id :s3}]
              [:fsm/state {:fsm/id :s4}]
              [:fsm/transition #:fsm.transition{:target :p3 :event :t}]]
             [:fsm/parallel {:fsm/id :p3}
              [:fsm/state {:fsm/id :s5}]
              [:fsm/state {:fsm/id :s6}]]]
            [:fsm/state {:fsm/id :s2 :fsm/initial :p4}
             [:fsm/parallel {:fsm/id :p4}
              [:fsm/state {:fsm/id :s7}]
              [:fsm/state {:fsm/id :s8}]
              [:fsm/transition #:fsm.transition{:target :p5 :event :t}]]
             [:fsm/parallel {:fsm/id :p5}
              [:fsm/state {:fsm/id :s9}]
              [:fsm/state {:fsm/id :s10}]]]]])]
    (assert-states
      fsm #{:s3 :s4 :s7 :s8}
      [{:fsm/event :t} #{:s5 :s6 :s9 :s10}])))

(deftest parallel-3
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :p1}
           [:fsm/parallel#p1
            [:fsm/state#s1 {:fsm/initial :p2}
             [:fsm/parallel#p2
              [:fsm/state#s3 {:fsm/initial :s3_1}
               [:fsm/state#s3_1
                [:fsm/transition #:fsm.transition{:target :s3_2 :event :t}]]
               [:fsm/state#s3_2]]
              [:fsm/state#s4]]
             [:fsm/parallel#p3
              [:fsm/state#s5]
              [:fsm/state#s6]]]
            [:fsm/state#s2 {:fsm/initial :p4}
             [:fsm/parallel#p4
              [:fsm/state#s7]
              [:fsm/state#s8]
              [:fsm/transition #:fsm.transition{:target :p5 :event :t}]]
             [:fsm/parallel#p5
              [:fsm/state#s9]
              [:fsm/state#s10]]]]])]
    (assert-states
      fsm #{:s3_1 :s4 :s7 :s8}
      [{:fsm/event :t} #{:s3_2 :s4 :s9 :s10}])))

(deftest more-parallel-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a :event :t}]]
            [:fsm/state#b]]])]
    (assert-states
      fsm #{:a :b}
      [{:fsm/event :t} #{:a :b}])))

(deftest more-parallel-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]
            [:fsm/state#b
             [:fsm/state#b1]
             [:fsm/state#b2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a1 :b1}])))

(deftest more-parallel-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:target :b2 :event :t}]]
             [:fsm/state#b2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a1 :b1}])))

(deftest more-parallel-2-b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:target :b2 :event :t}]]
             [:fsm/state#b2]]
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a1 :b2}])))

(deftest more-parallel-3
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a2 :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:target :b2 :event :t}]]
             [:fsm/state#b2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a2 :b1}])))

(deftest more-parallel-3b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:target :b2 :event :t}]]
             [:fsm/state#b2]]
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a2 :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a1 :b2}])))

(deftest more-parallel-4
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]
            [:fsm/state#b
             [:fsm/transition #:fsm.transition{:target :b :event :t}]
             [:fsm/state#b1]
             [:fsm/state#b2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a1 :b1}])))

(deftest more-parallel-5
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a2 :event :t}]
             [:fsm/state#a1]
             [:fsm/state#a2]]
            [:fsm/state#b
             [:fsm/transition #:fsm.transition{:target :b2 :event :t}]
             [:fsm/state#b1]
             [:fsm/state#b2]]]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t} #{:a2 :b1}])))

(deftest more-parallel-6
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a22 :event :t}]
             [:fsm/state#a1
              [:fsm/state#a11]
              [:fsm/state#a12]]
             [:fsm/state#a2
              [:fsm/state#a21]
              [:fsm/state#a22]]]
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/state#b11
               [:fsm/transition #:fsm.transition{:target :b12 :event :t}]]
              [:fsm/state#b12]]
             [:fsm/state#b2
              [:fsm/state#b21]
              [:fsm/state#b22]]]]])]
    (assert-states
      fsm #{:a11 :b11}
      [{:fsm/event :t} #{:a22 :b11}])))

(deftest more-parallel-6b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/state#b11
               [:fsm/transition #:fsm.transition{:target :b12 :event :t}]]
              [:fsm/state#b12]]
             [:fsm/state#b2
              [:fsm/state#b21]
              [:fsm/state#b22]]]
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a22 :event :t}]
             [:fsm/state#a1
              [:fsm/state#a11]
              [:fsm/state#a12]]
             [:fsm/state#a2
              [:fsm/state#a21]
              [:fsm/state#a22]]]]])]
    (assert-states
      fsm #{:a11 :b11}
      [{:fsm/event :t} #{:a11 :b12}])))

(deftest more-parallel-7
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a22 :event :t}]
             [:fsm/state#a1
              [:fsm/state#a11]
              [:fsm/state#a12]]
             [:fsm/state#a2
              [:fsm/state#a21]
              [:fsm/state#a22]]]
            [:fsm/state#b
             [:fsm/transition #:fsm.transition{:target :b22 :event :t}]
             [:fsm/state#b1
              [:fsm/state#b11]
              [:fsm/state#b12]]
             [:fsm/state#b2
              [:fsm/state#b21]
              [:fsm/state#b22]]]]])]
    (assert-states
      fsm #{:a11 :b11}
      [{:fsm/event :t} #{:a22 :b11}])))

(deftest more-parallel-8
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#x
            [:fsm/transition #:fsm.transition{:event :t :target :a22}]]
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/state#a1
              [:fsm/state#a11]
              [:fsm/state#a12]]
             [:fsm/state#a2
              [:fsm/state#a21]
              [:fsm/state#a22]]]
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/state#b11]
              [:fsm/state#b12]]
             [:fsm/state#b2
              [:fsm/state#b21]
              [:fsm/state#b22]]]]])]
    (assert-states
      fsm #{:x}
      [{:fsm/event :t} #{:a22 :b11}])))

(deftest more-parallel-9
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#x
            [:fsm/transition #:fsm.transition{:event :t :target #{:a22 :b22}}]]
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/state#a1
              [:fsm/state#a11]
              [:fsm/state#a12]]
             [:fsm/state#a2
              [:fsm/state#a21]
              [:fsm/state#a22]]]
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/state#b11]
              [:fsm/state#b12]]
             [:fsm/state#b2
              [:fsm/state#b21]
              [:fsm/state#b22]]]]])]
    (assert-states
      fsm #{:x}
      [{:fsm/event :t} #{:a22 :b22}])))

(deftest hierarchy-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/state#a1
             [:fsm/transition #:fsm.transition{:target :a2 :event :t}]]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:a1}
      [{:fsm/event :t} #{:a2}])))

(deftest hierarchy-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/state#a1
             [:fsm/transition #:fsm.transition{:target :a2 :event :t}]]
            [:fsm/state#a2]
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b]])]
    (assert-states
      fsm #{:a1}
      [{:fsm/event :t} #{:a2}])))

(deftest hierarchy-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/state#a1
             [:fsm/transition #:fsm.transition{:target :b :event :t}]]
            [:fsm/state#a2]
            [:fsm/transition #:fsm.transition{:target :a1 :event :t}]]
           [:fsm/state#b]])]
    (assert-states
      fsm #{:a1}
      [{:fsm/event :t} #{:b}])))

(deftest hierarchy+document-order-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/state#a1
             [:fsm/transition #:fsm.transition{:target :a2 :event :t}]
             [:fsm/transition #:fsm.transition{:target :c :event :t}]]
            [:fsm/state#a2]
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a1}
      [{:fsm/event :t} #{:a2}])))

(deftest hierarchy+document-order-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/state#a1
             [:fsm/transition #:fsm.transition{:target :b :event :t}]
             [:fsm/transition #:fsm.transition{:target :c :event :t}]]
            [:fsm/state#a2]
            [:fsm/transition #:fsm.transition{:target :a2 :event :t}]]
           [:fsm/state#b]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a1}
      [{:fsm/event :t} #{:b}])))

;; TODO: figure out this behavior
#_(deftest misc-deep-initial
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :s2}
           [:fsm/state]
           [:fsm/state#uber
            [:fsm/state#s1
             [:fsm/transition #:fsm.transition{:event :ev1 :target :s2}]]
            [:fsm/state#s2]]])]
    (is (= #{:s2} (c/get-active-atomic-states fsm)))))

(deftest multiple-events-per-transition-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event #{:foo :bar :bat}}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event #{:foo :bar :bat}}]]
           [:fsm/state#c
            [:fsm/transition #:fsm.transition{:target :d :event #{:foo :bar :bat}}]]
           [:fsm/state#d]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :foo} #{:b}]
      [{:fsm/event :bar} #{:c}]
      [{:fsm/event :bat} #{:d}])))

(deftest parallel+interrupt-0
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a2}]]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-1
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :a1}]]]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:c2 :d1}])))

(deftest parallel+interrupt-2
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
             [:fsm/state#c2]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-3
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/parallel#c
             [:fsm/state#e
              [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
             [:fsm/state#f
              [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
             [:fsm/transition #:fsm.transition{:event :t :target :a3}]]
            [:fsm/state#d]]
           [:fsm/state#a1]
           [:fsm/state#a2]
           [:fsm/state#a3]
           [:fsm/state#a4]])]
    (assert-states
      fsm #{:e :f :d}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-4
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/parallel#p
             [:fsm/state#e
              [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
             [:fsm/state#f
              [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
             [:fsm/transition #:fsm.transition{:event :t :target :a3}]]
            [:fsm/state#d {:fsm/initial :g}
             [:fsm/state#g
              [:fsm/transition #:fsm.transition{:event :t :target :h}]]
             [:fsm/state#h]]]
           [:fsm/state#a1]
           [:fsm/state#a2]
           [:fsm/state#a3]
           [:fsm/state#a4]])]
    (assert-states
      fsm #{:e :f :g}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-5
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#d {:fsm/initial :g}
             [:fsm/state#g
              [:fsm/transition #:fsm.transition{:event :t :target :h}]]
             [:fsm/state#h]]
            [:fsm/parallel#p
             [:fsm/state#e
              [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
             [:fsm/state#f
              [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
             [:fsm/transition #:fsm.transition{:event :t :target :a3}]]]
           [:fsm/state#a1]
           [:fsm/state#a2]
           [:fsm/state#a3]
           [:fsm/state#a4]])]
    (assert-states
      fsm #{:e :f :g}
      [{:fsm/event :t} #{:e :f :h}])))

(deftest parallel+interrupt-6
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :g}
             [:fsm/state#g
              [:fsm/transition #:fsm.transition{:event :t :target :h}]]
             [:fsm/state#h]]
            [:fsm/parallel#d
             [:fsm/state#e {:fsm/initial :e1}
              [:fsm/state#e1
               [:fsm/transition #:fsm.transition{:event :t :target :e2}]]
              [:fsm/state#e2]]
             [:fsm/state#f {:fsm/initial :f1}
              [:fsm/state#f1
               [:fsm/transition #:fsm.transition{:event :t :target :f2}]]
              [:fsm/state#f2]]]]])]
    (assert-states
      fsm #{:g :e1 :f1}
      [{:fsm/event :t} #{:h :e2 :f2}])))

(deftest parallel+interrupt-7
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
            [:fsm/parallel#d
             [:fsm/state#e {:fsm/initial :e1}
              [:fsm/state#e1
               [:fsm/transition #:fsm.transition{:event :t :target :e2}]]
              [:fsm/state#e2]]
             [:fsm/state#f {:fsm/initial :f1}
              [:fsm/state#f1
               [:fsm/transition #:fsm.transition{:event :t :target :f2}]]
              [:fsm/state#f2]]]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c :e1 :f1}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-8
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state#a {:fsm/initial :b}
            [:fsm/parallel#b
             [:fsm/state#b1]
             [:fsm/state#b2]
             [:fsm/transition #:fsm.transition{:event :t :target :c}]]
            [:fsm/parallel#c
             [:fsm/state#c1]
             [:fsm/state#c2]]]])]
    (assert-states
      fsm #{:b1 :b2}
      [{:fsm/event :t} #{:c1 :c2}])))

(deftest parallel+interrupt-9
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state#a {:fsm/initial :b}
            [:fsm/parallel#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:event :t :target :c}]]
             [:fsm/state#b2]]
            [:fsm/parallel#c
             [:fsm/state#c1]
             [:fsm/state#c2]]]])]
    (assert-states
      fsm #{:b1 :b2}
      [{:fsm/event :t} #{:c1 :c2}])))

(deftest parallel+interrupt-10
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state#a {:fsm/initial :b}
            [:fsm/transition #:fsm.transition{:event :t :target :c}]
            [:fsm/parallel#b
             [:fsm/state#b1]
             [:fsm/state#b2]]
            [:fsm/parallel#c
             [:fsm/state#c1]
             [:fsm/state#c2]]]])]
    (assert-states
      fsm #{:b1 :b2}
      [{:fsm/event :t} #{:c1 :c2}])))

(deftest parallel+interrupt-11
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state#a {:fsm/initial :b}
            [:fsm/parallel#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:event :t :target :d}]]
             [:fsm/state#b2
              [:fsm/transition #:fsm.transition{:event :t :target :c}]]]
            [:fsm/parallel#c
             [:fsm/state#c1]
             [:fsm/state#c2]]]
           [:fsm/state#d]])]
    (assert-states
      fsm #{:b1 :b2}
      [{:fsm/event :t} #{:d}])))

(deftest parallel+interrupt-12
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state#a {:fsm/initial :b}
            [:fsm/parallel#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:event :t :target :c}]]
             [:fsm/state#b2
              [:fsm/transition #:fsm.transition{:event :t :target :d}]]]
            [:fsm/parallel#c
             [:fsm/state#c1]
             [:fsm/state#c2]]]
           [:fsm/state#d]])]
    (assert-states
      fsm #{:b1 :b2}
      [{:fsm/event :t} #{:c1 :c2}])))

(deftest parallel+interrupt-13
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/state#a {:fsm/initial :b}
            [:fsm/parallel#b
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:event :t :target :c}]]
             [:fsm/state#b2]
             [:fsm/transition #:fsm.transition{:event :t :target :d}]]
            [:fsm/parallel#c
             [:fsm/state#c1]
             [:fsm/state#c2]]]
           [:fsm/state#d]])]
    (assert-states
      fsm #{:b1 :b2}
      [{:fsm/event :t} #{:c1 :c2}])))

(deftest parallel+interrupt-14
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/parallel#a
            [:fsm/parallel#b
             [:fsm/parallel#c
              [:fsm/parallel#d
               [:fsm/parallel#e
                [:fsm/state#i {:fsm/initial :i1}
                 [:fsm/state#i1
                  [:fsm/transition #:fsm.transition{:target :l :event :t}]]
                 [:fsm/state#i2]]
                [:fsm/state#j]]
               [:fsm/state#h]]
              [:fsm/state#g]]
             [:fsm/state#f {:fsm/initial :f1}
              [:fsm/state#f1
               [:fsm/transition #:fsm.transition{:target :f2 :event :t}]]
              [:fsm/state#f2]]]
            [:fsm/state#k]]
           [:fsm/state#l]])]
    (assert-states
      fsm #{:i1 :j :h :g :f1 :k}
      [{:fsm/event :t} #{:l}])))

(deftest parallel+interrupt-15
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :a}
           [:fsm/parallel#a
            [:fsm/parallel#b
             [:fsm/parallel#c
              [:fsm/parallel#d
               [:fsm/parallel#e
                [:fsm/state#i {:fsm/initial :i1}
                 [:fsm/state#i1
                  [:fsm/transition #:fsm.transition{:target :i2 :event :t}]]
                 [:fsm/state#i2]]
                [:fsm/state#j]]
               [:fsm/state#h]]
              [:fsm/state#g]]
             [:fsm/state#f {:fsm/initial :f1}
              [:fsm/state#f1
               [:fsm/transition #:fsm.transition{:target :l :event :t}]]
              [:fsm/state#f2]]]
            [:fsm/state#k]]
           [:fsm/state#l]])]
    (assert-states
      fsm #{:i1 :j :h :g :f1 :k}
      [{:fsm/event :t} #{:i2 :j :h :g :f1 :k}])))

(deftest parallel+interrupt-16
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a}]]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a2}]]]
           [:fsm/state#a {:fsm/initial :a1}
            [:fsm/state#a1]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-17
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]]
           [:fsm/state#a {:fsm/initial :a1}
            [:fsm/state#a1]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a2}])))

(deftest parallel+interrupt-18
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
            [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a2}])))

(deftest parallel+interrupt-19
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]
            [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:c2 :d2}])))

(deftest parallel+interrupt-20
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c1 :d}
      [{:fsm/event :t} #{:c2 :d}])))

(deftest parallel+interrupt-21
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c :d1}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-21-b
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c :d1}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-21-c
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c :d1}
      [{:fsm/event :t} #{:c :d2}])))

(deftest parallel+interrupt-22
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]
            [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:c2 :d2}])))

(deftest parallel+interrupt-23
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
            [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a2}])))

(deftest parallel+interrupt-24
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]
            [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:c2 :d2}])))

(deftest parallel+interrupt-25
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
           [:fsm/state#a1]
           [:fsm/state#a2]])]
    (assert-states
      fsm #{:c1 :d}
      [{:fsm/event :t} #{:c2 :d}])))

(deftest parallel+interrupt-27
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :d2}]]
             [:fsm/state#d2]]
            [:fsm/transition #:fsm.transition{:event :t :target :a1}]]
           [:fsm/state#a1]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:c2 :d2}])))

(deftest parallel+interrupt-28
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c
             [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
            [:fsm/state#d]
            [:fsm/transition #:fsm.transition{:event :t :target :a}]]
           [:fsm/state#a {:fsm/initial :a1}
            [:fsm/state#a1]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a2}])))

(deftest parallel+interrupt-29
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a}]]
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
           [:fsm/state#a {:fsm/initial :a1}
            [:fsm/state#a1]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:c :d}
      [{:fsm/event :t} #{:a1}])))

(deftest parallel+interrupt-30
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :c2}]]
             [:fsm/state#c2]]
            [:fsm/state#d
             [:fsm/transition #:fsm.transition{:event :t :target :a}]]
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
           [:fsm/state#a {:fsm/initial :a1}
            [:fsm/state#a1]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:c1 :d}
      [{:fsm/event :t} #{:c2 :d}])))

(deftest parallel+interrupt-31
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :b}
           [:fsm/parallel#b
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:event :t :target :a}]]]
            [:fsm/state#d {:fsm/initial :d1}
             [:fsm/state#d1
              [:fsm/transition #:fsm.transition{:event :t :target :a}]]
             [:fsm/state#d2]]
            [:fsm/transition #:fsm.transition{:event :t :target :a2}]]
           [:fsm/state#a {:fsm/initial :a1}
            [:fsm/state#a1]
            [:fsm/state#a2]]])]
    (assert-states
      fsm #{:c1 :d1}
      [{:fsm/event :t} #{:a1}])))