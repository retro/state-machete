(ns com.verybigthings.state-machete.core-test
  (:require [clojure.test :refer :all]
            [com.verybigthings.state-machete.core :as c]
            [criterium.core :as cr]
            [clj-async-profiler.core :as prof]))

(set! *warn-on-reflection* true)

(defn log [& forms]
  (doseq [f forms]
    (println (with-out-str (clojure.pprint/pprint f)))))

(defn assert-states [fsm initial-expected-state & events-expected-states]
  (is (= initial-expected-state (c/get-active-atomic-states fsm)))
  (loop [events-expected-states events-expected-states
         fsm                    fsm]
    (let [[first-event-expected-state & rest-events-expected-states] events-expected-states]
      (if first-event-expected-state
        (let [[event expected-state] first-event-expected-state
              fsm' (c/trigger fsm event)]
          (is (= expected-state (c/get-active-atomic-states fsm')))
          (recur rest-events-expected-states fsm'))
        fsm))))

(defn make-fsm
  ([fsm] (make-fsm fsm {} {}))
  ([fsm context] (make-fsm fsm context {}))
  ([fsm context data]
   (-> fsm (c/compile context) (c/start data))))

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

(deftest basic-1b-with-partials
  (let [fsm
        (make-fsm
          [:fsm/root
           [:<>
            [:fsm/state {:fsm/id :a}
             [:<>
              [:<> [:fsm/transition {:fsm.transition/target :b :fsm.transition/event :t}]]]]
            [:fsm/state {:fsm/id :b}]]])]
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

(deftest misc-deep-initial
  (let [fsm
        (make-fsm
          [:fsm/root {:fsm/initial :s2}
           [:fsm/state]
           [:fsm/state#foo
            [:fsm/state#bar]
            [:fsm/state#baz]
            [:fsm/state#qux
             [:fsm/state#bat]
             [:fsm/state#uber
              [:fsm/state#s1
               [:fsm/transition #:fsm.transition{:event :ev1 :target :s2}]]
              [:fsm/state#s2]]]]])]
    (is (= #{:fsm/root :foo :qux :uber} (c/get-active-compound-states fsm)))
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

(deftest prefix-event-name-matching-star-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :*}]
            [:fsm/transition #:fsm.transition{:target :fail :event :foo}]]
           [:fsm/state#b]
           [:fsm/state#fail]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :foo} #{:b}])))

(deftest prefix-event-name-matching-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :foo}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event :foo.bar}]]
           [:fsm/state#c
            [:fsm/transition #:fsm.transition{:target :d :event :foo.bar.bat}]]
           [:fsm/state#d
            [:fsm/transition #:fsm.transition{:target :e :event :foo}]]
           [:fsm/state#e
            [:fsm/transition #:fsm.transition{:target :f :event :foo.bar.bat}]]
           [:fsm/state#f
            [:fsm/transition #:fsm.transition{:target :g :event :foo.bar.bat}]]
           [:fsm/state#g]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :foo} #{:b}]
      [{:fsm/event :foo.bar} #{:c}]
      [{:fsm/event :foo.bar.bat} #{:d}]
      [{:fsm/event :foo.bar.bat} #{:e}]
      [{:fsm/event :foo} #{:e}]
      [{:fsm/event :foo.bar.bat} #{:f}]
      [{:fsm/event :foobar} #{:f}]
      [{:fsm/event :foo.bar.bat.bif} #{:g}])))

(deftest prefix-event-name-matching-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :foo}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event :foo.bar}]]
           [:fsm/state#c
            [:fsm/transition #:fsm.transition{:target :d :event :foo.bar.bat}]]
           [:fsm/state#d
            [:fsm/transition #:fsm.transition{:target :e :event :foo.*}]
            [:fsm/transition #:fsm.transition{:target :fail :event :foo}]]
           [:fsm/state#e
            [:fsm/transition #:fsm.transition{:target :f :event :foo.bar.*}]
            [:fsm/transition #:fsm.transition{:target :fail :event :foo.bar}]]
           [:fsm/state#f
            [:fsm/transition #:fsm.transition{:target :g :event :foo.bar.bat.*}]
            [:fsm/transition #:fsm.transition{:target :fail :event :foo.bar.bar}]]
           [:fsm/state#g]
           [:fsm/state#fail]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :foo} #{:b}]
      [{:fsm/event :foo.bar} #{:c}]
      [{:fsm/event :foo.bar.bat} #{:d}]
      [{:fsm/event :foo.bar.bat} #{:e}]
      [{:fsm/event :foo} #{:e}]
      [{:fsm/event :foo.bar.bar} #{:f}]
      [{:fsm/event :foobar} #{:f}]
      [{:fsm/event :foo.bar.bat.bif} #{:g}])))

(deftest cond-test-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t :cond true}]]
           [:fsm/state#b]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b}])))

(deftest cond-test-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :f :event :t :cond false}]
            [:fsm/transition #:fsm.transition{:target :b :event :t :cond true}]]
           [:fsm/state#b]
           [:fsm/state#f]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b}])))

(deftest cond-test-conditional-transition
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event :t1}]]
           [:fsm/state#c
            [:fsm/transition #:fsm.transition{:target :d1}]
            [:fsm/transition #:fsm.transition{:target :d2}]]
           [:fsm/state#d1
            [:fsm/transition #:fsm.transition{:target :e1 :event :t2}]]
           [:fsm/state#d2]
           [:fsm/state#e1
            [:fsm/transition #:fsm.transition{:target :f1 :event :t3 :cond false}]
            [:fsm/transition #:fsm.transition{:target :f2 :event :t3 :cond true}]]
           [:fsm/state#e2]
           [:fsm/state#f1]
           [:fsm/state#f2
            [:fsm/transition #:fsm.transition{:target :g1 :event :t4 :cond false}]
            [:fsm/transition #:fsm.transition{:target :g2 :event :t4 :cond false}]
            [:fsm/transition #:fsm.transition{:target :g3 :event :t4 :cond true}]]
           [:fsm/state#g1]
           [:fsm/state#g2]
           [:fsm/state#g3 {:fsm/initial :h}
            [:fsm/state#h
             [:fsm/transition #:fsm.transition{:target :i :event :t5 :cond true}]]
            [:fsm/state#i
             [:fsm/transition #:fsm.transition{:target :j :event :t5 :cond false}]]
            [:fsm/state#j]
            [:fsm/transition #:fsm.transition{:target :last :event :t5 :cond true}]]
           [:fsm/state#last]])]
    (assert-states
      fsm #{:b}
      [{:fsm/event :t1} #{:d1}]
      [{:fsm/event :t2} #{:e1}]
      [{:fsm/event :t3} #{:f2}]
      [{:fsm/event :t4} #{:h}]
      [{:fsm/event :t5} #{:i}]
      [{:fsm/event :t5} #{:last}])))

(deftest assign-current-small-step-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            {:fsm.on/enter (fn [fsm & _]
                             (-> fsm
                               (c/update-data assoc :x -1)
                               (c/update-data assoc :x 99)))}
            [:fsm/transition
             #:fsm.transition{:event :t
                              :target :b
                              :cond (fn [fsm & _]
                                      (= 99 (c/get-in-data fsm [:x])))
                              :fsm/on (fn [fsm & _]
                                        (c/update-in-data fsm [:x] inc))}]]
           [:fsm/state#b
            {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm [:x] * 2))}
            [:fsm/transition #:fsm.transition{:target :c :cond (fn [fsm & _] (= 200 (c/get-in-data fsm [:x])))}]
            [:fsm/transition #:fsm.transition{:target :f}]]
           [:fsm/state#c]
           [:fsm/state#f]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest assign-current-small-step-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition {:event :t :target :b :fsm/on (fn [fsm & _] (c/assoc-in-data fsm [:i] 0))}]]
           [:fsm/state#b
            [:fsm/transition
             #:fsm.transition{:target :b
                              :cond (fn [fsm & _] (> 100 (c/get-in-data fsm [:i])))
                              :fsm/on (fn [fsm & _] (c/update-in-data fsm [:i] inc))}]
            [:fsm/transition
             #:fsm.transition{:target :c :cond (fn [fsm & _] (= 100 (c/get-in-data fsm [:i])))}]]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest assign-current-small-step-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:event :t
                              :target :b
                              :fsm/on (fn [fsm & _] (c/assoc-in-data fsm [:i] 0))}]]
           [:fsm/state#A
            [:fsm/state#b
             [:fsm/transition
              #:fsm.transition{:target :c
                               :cond (fn [fsm & _] (> 100 (c/get-in-data fsm [:i])))
                               :fsm/on (fn [fsm & _] (c/update-in-data fsm [:i] inc))}]]
            [:fsm/state#c
             [:fsm/transition
              #:fsm.transition{:target :b
                               :cond (fn [fsm & _] (> 100 (c/get-in-data fsm [:i])))
                               :fsm/on (fn [fsm & _] (c/update-in-data fsm [:i] inc))}]]
            [:fsm/transition
             #:fsm.transition{:target :d
                              :cond (fn [fsm & _] (= 100 (c/get-in-data fsm [:i])))
                              :fsm/on (fn [fsm & _] (c/update-in-data fsm [:i] * 2))}]]
           [:fsm/state#d
            [:fsm/transition #:fsm.transition{:target :e :cond (fn [fsm & _] (= 200 (c/get-in-data fsm [:i])))}]
            [:fsm/transition #:fsm.transition{:target :f}]]
           [:fsm/state#e]
           [:fsm/state#f]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:e}])))

(deftest assign-current-small-step-3
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:event :t1
                              :target :p
                              :fsm/on (fn [fsm & _] (c/assoc-in-data fsm [:i] 0))}]]
           [:fsm/parallel#p
            [:fsm/state#b {:fsm/initial :b1}
             [:fsm/state#b1
              [:fsm/transition
               #:fsm.transition{:event :t2
                                :target :b2
                                :fsm/on (fn [fsm & _] (c/update-in-data fsm [:i] inc))}]]
             [:fsm/state#b2]]
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition
               #:fsm.transition{:event :t2
                                :target :c2
                                :fsm/on (fn [fsm & _] (c/update-in-data fsm [:i] dec))}]]
             [:fsm/state#c2]]
            [:fsm/transition
             #:fsm.transition{:event :t3 :target :d :cond (fn [fsm & _] (zero? (c/get-in-data fsm [:i])))}]
            [:fsm/transition
             #:fsm.transition{:event :t3 :target :f}]]
           [:fsm/state#d]
           [:fsm/state#f]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b1 :c1}]
      [{:fsm/event :t2} #{:b2 :c2}]
      [{:fsm/event :t3} #{:d}])))

(deftest assign-current-small-step-4
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm [:x] 2))}
            [:fsm/transition #:fsm.transition{:event :t :target :b1}]]
           [:fsm/state#b
            {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm [:x] * 3))}
            [:fsm/state#b1
             {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm [:x] * 5))}]
            [:fsm/state#b2
             {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm [:x] * 7))}]
            [:fsm/transition #:fsm.transition{:target :c :cond (fn [fsm & _] (= 30 (c/get-in-data fsm [:x])))}]
            [:fsm/transition #:fsm.transition{:target :f}]]
           [:fsm/state#c]
           [:fsm/state#f]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest internal-transitions-0
  (let [inc-x (fn [fsm & _] (c/update-in-data fsm [:x] inc))
        get-x #(c/get-in-data % [:x])
        fsm   (make-fsm
                [:fsm/root
                 {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm [:x] 0))}
                 [:fsm/state#a
                  {:fsm.on/enter inc-x
                   :fsm.on/exit inc-x}
                  [:fsm/state#a1]
                  [:fsm/state#a2
                   [:fsm/transition
                    #:fsm.transition{:target :b :event :t2 :cond (fn [fsm & _] (= 1 (get-x fsm)))}]]
                  [:fsm/transition
                   #:fsm.transition{:target :a2 :event :t1 :type :internal :cond (fn [fsm & _] (= 1 (get-x fsm)))}]]
                 [:fsm/state#b
                  [:fsm/transition
                   #:fsm.transition{:target :c :event :t3 :cond (fn [fsm & _] (= 2 (get-x fsm)))}]]
                 [:fsm/state#c]])]
    (assert-states
      fsm #{:a1}
      [{:fsm/event :t1} #{:a2}]
      [{:fsm/event :t2} #{:b}]
      [{:fsm/event :t3} #{:c}])))

(deftest internal-transitions-1
  (let [inc-x (fn [fsm & _] (c/update-in-data fsm [:x] inc))
        fsm   (make-fsm
                [:fsm/root
                 {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :x 0))}
                 [:fsm/parallel#p
                  {:fsm.on/enter inc-x
                   :fsm.on/exit inc-x}
                  [:fsm/state#a
                   {:fsm.on/enter inc-x
                    :fsm.on/exit inc-x}
                   [:fsm/state#a1
                    {:fsm.on/enter inc-x
                     :fsm.on/exit inc-x}]
                   [:fsm/state#a2
                    {:fsm.on/enter inc-x
                     :fsm.on/exit inc-x}
                    [:fsm/transition
                     #:fsm.transition{:target :c :event :t2 :cond (fn [fsm & _] (= 5 (c/get-in-data fsm :x)))}]]
                   [:fsm/transition
                    #:fsm.transition{:target :a2 :event :t1 :type :internal :cond (fn [fsm & _] (= 3 (c/get-in-data fsm :x)))}]]
                  [:fsm/state#b
                   [:fsm/state#b1]
                   [:fsm/state#b2]]]
                 [:fsm/state#c
                  [:fsm/transition
                   #:fsm.transition{:target :d :event :t3 :cond (fn [fsm & _] (= 8 (c/get-in-data fsm :x)))}]]
                 [:fsm/state#d]])]
    (assert-states
      fsm #{:a1 :b1}
      [{:fsm/event :t1} #{:a2 :b1}]
      [{:fsm/event :t2} #{:c}]
      [{:fsm/event :t3} #{:d}])))

(deftest in-predicate
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p1
            [:fsm/state#r1 {:fsm/initial :a1}
             [:fsm/state#a1
              [:fsm/transition
               #:fsm.transition{:event :t1 :target :b1 :cond (fn [fsm & _] (c/in-state? fsm :a1))}]]
             [:fsm/state#b1
              [:fsm/transition
               #:fsm.transition{:event :t2 :target :c1 :cond (fn [fsm & _] (c/in-state? fsm :r1))}]]
             [:fsm/state#c1
              [:fsm/transition
               #:fsm.transition{:event :t3 :target :d1 :cond (fn [fsm & _] (c/in-state? fsm :p1))}]]
             [:fsm/state#d1
              [:fsm/transition
               #:fsm.transition{:event :t4 :target :e1 :cond (fn [fsm & _] (not (c/in-state? fsm :e2)))}]]
             [:fsm/state#e1
              [:fsm/transition
               #:fsm.transition{:event :t5 :target :f1 :cond (fn [fsm & _] (not (c/in-state? fsm :c2)))}]]
             [:fsm/state#f1
              [:fsm/transition
               #:fsm.transition{:event :t6
                                :target :g1
                                :cond (fn [fsm & _] (c/in-state? fsm :a2))
                                :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :gen1}))}]]
             [:fsm/state#g1
              [:fsm/transition
               #:fsm.transition{:event :t7
                                :target :h1
                                :cond (fn [fsm & _] (c/in-state? fsm :b2))
                                :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :gen2}))}]]
             [:fsm/state#h1
              [:fsm/transition
               #:fsm.transition{:event :t8 :target :i1 :cond (fn [fsm & _] (c/in-state? fsm :c2))}]]
             [:fsm/state#i1
              [:fsm/transition
               #:fsm.transition{:event :t9
                                :target :j1
                                :cond (fn [fsm & _] (c/in-state? fsm :d2))
                                :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :gen3}))}]]
             [:fsm/state#j1
              [:fsm/transition
               #:fsm.transition{:event :t10 :target :k1 :cond (fn [fsm & _] (c/in-state? fsm :e2))}]]
             [:fsm/state#k1]]
            [:fsm/state#r2 {:fsm/initial :a2}
             [:fsm/state#a2
              [:fsm/transition
               #:fsm.transition{:event :gen1 :target :b2}]]
             [:fsm/state#b2
              [:fsm/transition
               #:fsm.transition{:event :gen2 :target :c2}]]
             [:fsm/state#c2 {:fsm/initial :d2}
              [:fsm/state#d2
               [:fsm/transition
                #:fsm.transition{:event :gen3 :target :e2}]]
              [:fsm/state#e2]]]]])]
    (assert-states
      fsm #{:a1 :a2}
      [{:fsm/event :t1} #{:a2 :b1}]
      [{:fsm/event :t2} #{:a2 :c1}]
      [{:fsm/event :t3} #{:a2 :d1}]
      [{:fsm/event :t4} #{:a2 :e1}]
      [{:fsm/event :t5} #{:a2 :f1}]
      [{:fsm/event :t6} #{:g1 :b2}]
      [{:fsm/event :t7} #{:h1 :d2}]
      [{:fsm/event :t8} #{:i1 :d2}]
      [{:fsm/event :t9} #{:j1 :e2}]
      [{:fsm/event :t10} #{:k1 :e2}])))

(deftest send-data-send-1
  (let [fsm
                 (make-fsm
                   [:fsm/root
                    {:fsm.on/enter (fn [fsm & _] (c/assoc-data fsm {:foo 1 :bar 2 :bat 3}))}
                    [:fsm/state#a
                     [:fsm/transition
                      #:fsm.transition{:target :b
                                       :event :t
                                       :fsm/on (fn [fsm & _]
                                                 (let [fsm-data (c/get-data fsm)
                                                       data     {:foo (:foo fsm-data)
                                                                 :bar (:bar fsm-data)
                                                                 :bif (:bat fsm-data)
                                                                 :belt 4}]
                                                   (c/raise-delayed fsm 10 {:fsm/event :s1 :data data})))}]]
                    [:fsm/state#b
                     [:fsm/transition
                      #:fsm.transition{:event :s1
                                       :target :c
                                       :cond (fn [_ ev]
                                               (= (:data ev) {:foo 1 :bar 2 :bif 3 :belt 4}))
                                       :fsm/on (fn [fsm & _]
                                                 (c/raise fsm {:fsm/event :s2 :data "More content."}))}]
                     [:fsm/transition
                      #:fsm.transition{:event :s1 :target :f}]]
                    [:fsm/state#c
                     [:fsm/transition
                      #:fsm.transition{:event :s2
                                       :target :d
                                       :cond (fn [_ ev]
                                               (= (:data ev) "More content."))
                                       :fsm/on (fn [fsm & _]
                                                 (c/raise fsm {:fsm/event :s3 :data "Hello world."}))}]
                     [:fsm/transition
                      #:fsm.transition{:event :s2 :target :f}]]
                    [:fsm/state#d
                     [:fsm/transition
                      #:fsm.transition{:event :s3
                                       :target :e
                                       :cond (fn [_ ev]
                                               (= (:data ev) "Hello world."))}]
                     [:fsm/transition
                      #:fsm.transition{:event :s3 :target :f}]]
                    [:fsm/state#e]
                    [:fsm/state#f]])
        fsm'     (assert-states
                   fsm #{:a}
                   [{:fsm/event :t} #{:b}])
        fsm-time (c/get-time fsm')
        fsm''    (c/trigger fsm' {:fsm/event :t2} (+ fsm-time 10))]
    (is (= #{:e} (c/get-active-atomic-states fsm'')))))

(deftest delayed-send-send-1
  (let [fsm
                 (make-fsm
                   [:fsm/root
                    [:fsm/state#a
                     [:fsm/transition
                      #:fsm.transition{:target :b
                                       :event :t1
                                       :fsm/on (fn [fsm & _] (c/raise-delayed fsm 10 {:fsm/event :s}))}]]
                    [:fsm/state#b
                     [:fsm/transition #:fsm.transition{:target :c :event :s}]]
                    [:fsm/state#c
                     [:fsm/transition #:fsm.transition{:target :d :event :t2}]]
                    [:fsm/state#d]])
        fsm'     (assert-states
                   fsm #{:a}
                   [{:fsm/event :t1} #{:b}])
        fsm-time (c/get-time fsm')
        fsm''    (c/trigger fsm' {:fsm/event :t2} (+ fsm-time 10))]
    (is (= #{:d} (c/get-active-atomic-states fsm'')))))

(deftest delayed-send-send-2
  (let [fsm
                 (make-fsm
                   [:fsm/root
                    [:fsm/state#a
                     {:fsm.on/exit (fn [fsm & _]
                                     (c/raise-delayed fsm 10 {:fsm/event :s}))}
                     [:fsm/transition #:fsm.transition{:target :b :event :t1}]]
                    [:fsm/state#b
                     [:fsm/transition #:fsm.transition{:target :c :event :s}]]
                    [:fsm/state#c
                     [:fsm/transition #:fsm.transition{:target :d :event :t2}]]
                    [:fsm/state#d]])
        fsm'     (assert-states
                   fsm #{:a}
                   [{:fsm/event :t1} #{:b}])
        fsm-time (c/get-time fsm')
        fsm''    (c/trigger fsm' {:fsm/event :t2} (+ fsm-time 10))]
    (is (= #{:d} (c/get-active-atomic-states fsm'')))))

(deftest delayed-send-send-3
  (let [fsm
                 (make-fsm
                   [:fsm/root
                    [:fsm/state#a
                     [:fsm/transition #:fsm.transition{:target :b :event :t1}]]
                    [:fsm/state#b
                     {:fsm.on/enter (fn [fsm & _]
                                      (c/raise-delayed fsm 10 {:fsm/event :s}))}
                     [:fsm/transition #:fsm.transition{:target :c :event :s}]]
                    [:fsm/state#c
                     [:fsm/transition #:fsm.transition{:target :d :event :t2}]]
                    [:fsm/state#d]])
        fsm'     (assert-states
                   fsm #{:a}
                   [{:fsm/event :t1} #{:b}])
        fsm-time (c/get-time fsm')
        fsm''    (c/trigger fsm' {:fsm/event :t2} (+ fsm-time 10))]
    (is (= #{:d} (c/get-active-atomic-states fsm'')))))

(deftest action-send-send-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:target :b
                              :event :t
                              :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}]]
           [:fsm/state#b
            [:fsm/transition
             #:fsm.transition{:target :c :event :s}]]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest action-send-send-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            {:fsm.on/exit (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event :s}]]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest action-send-send-3
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            {:fsm.on/enter (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}
            [:fsm/transition #:fsm.transition{:target :c :event :s}]]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest action-send-send-4
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            {:fsm.on/enter (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}
            [:fsm/transition #:fsm.transition{:target :c :event :s}]
            [:fsm/transition #:fsm.transition{:target :f1}]]
           [:fsm/state#c
            [:fsm/transition #:fsm.transition{:target :f2 :event :s}]
            [:fsm/transition #:fsm.transition{:target :d}]]
           [:fsm/state#f1]
           [:fsm/state#d]
           [:fsm/state#f2]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:f1}])))

(deftest action-send-send-4b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            {:fsm.on/enter (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}
            [:fsm/transition #:fsm.transition{:target :c :event :s}]]
           [:fsm/state#c]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:c}])))

(deftest action-send-send-7
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:target :b
                              :event :t
                              :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}]]
           [:fsm/state#b {:fsm/initial :b1}
            [:fsm/state#b1
             [:fsm/transition #:fsm.transition{:event :s :target :b2}]
             [:fsm/transition #:fsm.transition{:target :b3}]]
            [:fsm/state#b2]
            [:fsm/state#b3]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b3}])))

(deftest action-send-send-7b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:target :b
                              :event :t
                              :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}]]
           [:fsm/state#b {:fsm/initial :b1}
            [:fsm/state#b1
             [:fsm/transition #:fsm.transition{:event :s :target :b2}]]
            [:fsm/state#b2]
            [:fsm/state#b3]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b2}])))

(deftest action-send-send-8
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:target :b1
                              :event :t
                              :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}]]
           [:fsm/state#b {:fsm/initial :b1}
            [:fsm/state#b1
             [:fsm/transition #:fsm.transition{:event :s :target :b2}]
             [:fsm/transition #:fsm.transition{:target :b3}]]
            [:fsm/state#b2]
            [:fsm/state#b3]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b3}])))

(deftest action-send-send-8b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:target :b1
                              :event :t
                              :fsm/on (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}]]
           [:fsm/state#b {:fsm/initial :b1}
            [:fsm/state#b1
             [:fsm/transition #:fsm.transition{:event :s :target :b2}]]
            [:fsm/state#b2]
            [:fsm/state#b3]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b2}])))

(deftest action-send-send-9
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition
             #:fsm.transition{:target :b1
                              :event :t
                              :on (fn [fsm & _] (c/raise fsm {:fsm/event :s}))}]]
           [:fsm/state#b {:fsm/initial :b1}
            [:fsm/state#b1
             [:fsm/transition #:fsm.transition{:event :s :target :b2}]
             [:fsm/transition #:fsm.transition{:target :b3}]]
            [:fsm/state#b2]
            [:fsm/state#b3]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:b3}])))

(deftest targetless-transition-0
  (let [fsm
        (make-fsm
          [:fsm/root
           {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :i 0))}
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :done :cond (fn [fsm & _] (= 100 (c/get-in-data fsm :i)))}]
            [:fsm/transition #:fsm.transition{:fsm/on (fn [fsm & _] (c/update-in-data fsm :i inc))}]]
           [:fsm/state#done]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t} #{:done}])))

(deftest targetless-transition-1
  (let [fsm
        (make-fsm
          [:fsm/root
           {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :i 1))}
           [:fsm/state#A
            [:fsm/transition #:fsm.transition{:event :foo :fsm/on (fn [fsm & _] (c/update-in-data fsm :i * 2))}]
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:event :bar :fsm/on (fn [fsm & _] (c/update-in-data fsm :i #(Math/pow % 3)))}]]
            [:fsm/transition #:fsm.transition{:target :done :cond (fn [fsm & _] (= 8.0 (c/get-in-data fsm :i)))}]]
           [:fsm/state#done]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :foo} #{:a}]
      [{:fsm/event :bar} #{:done}])))

(deftest targetless-transition-2
  (let [fsm
        (make-fsm
          [:fsm/root
           {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :i 1))}
           [:fsm/state#A
            [:fsm/transition
             #:fsm.transition{:event :foo :fsm/on (fn [fsm & _] (c/update-in-data fsm :i * 2))}]
            [:fsm/transition
             #:fsm.transition{:event :bar :fsm/on (fn [fsm & _] (c/update-in-data fsm :i #(Math/pow % 3)))}]
            [:fsm/state#a
             [:fsm/transition
              #:fsm.transition{:event :foo :fsm/on (fn [fsm & _] (c/update-in-data fsm :i + 2))}]]
            [:fsm/transition
             #:fsm.transition{:target :done :cond (fn [fsm & _] (= 27.0 (c/get-in-data fsm :i)))}]]
           [:fsm/state#done]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :foo} #{:a}]
      [{:fsm/event :bar} #{:done}])))

(deftest targetless-transition-3
  (let [fsm
        (make-fsm
          [:fsm/root
           {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :i 1))}
           [:fsm/parallel#p
            [:fsm/transition
             #:fsm.transition{:target :done
                              :cond (fn [fsm & _] (= 100.0 (c/get-in-data fsm :i)))}]
            [:fsm/transition
             #:fsm.transition{:event :bar
                              :fsm/on (fn [fsm & _] (c/update-in-data fsm :i * 20))}]
            [:fsm/state#a
             [:fsm/state#a1
              [:fsm/transition
               #:fsm.transition{:event :foo
                                :target :a2
                                :fsm/on (fn [fsm & _] (c/update-in-data fsm :i * 2))}]]
             [:fsm/state#a2]]
            [:fsm/state#b
             [:fsm/state#b1
              [:fsm/transition
               #:fsm.transition{:event :foo
                                :target :b2
                                :fsm/on (fn [fsm & _] (c/update-in-data fsm :i #(Math/pow % 3)))}]]
             [:fsm/state#b2]]
            [:fsm/state#c
             [:fsm/transition
              #:fsm.transition{:event :foo
                               :fsm/on (fn [fsm & _] (c/update-in-data fsm :i - 3))}]]]
           [:fsm/state#done]])]
    (assert-states
      fsm #{:a1 :b1 :c}
      [{:fsm/event :foo} #{:a2 :b2 :c}]
      [{:fsm/event :bar} #{:done}])))

(deftest w3c-ecma-144
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#s0
            {:fsm.on/enter (fn [fsm & _]
                             (-> fsm
                               (c/raise {:fsm/event :foo})
                               (c/raise {:fsm/event :bar})))}
            [:fsm/transition #:fsm.transition{:event :foo :target :s1}]
            [:fsm/transition #:fsm.transition{:event :* :target :fail}]]
           [:fsm/state#s1
            [:fsm/transition #:fsm.transition{:event :bar :target :pass}]
            [:fsm/transition #:fsm.transition{:event :* :target :fail}]]
           [:fsm/final#pass]
           [:fsm/final#fail]])]
    (assert-states fsm #{:pass})))

(deftest w3c-ecma-158
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#s0
            {:fsm.on/enter (fn [fsm & _]
                             (-> fsm
                               (c/raise {:fsm/event :event1})
                               (c/raise {:fsm/event :event2})))}
            [:fsm/transition #:fsm.transition{:event :event1 :target :s1}]
            [:fsm/transition #:fsm.transition{:event :* :target :fail}]]
           [:fsm/state#s1
            [:fsm/transition #:fsm.transition{:event :event2 :target :pass}]
            [:fsm/transition #:fsm.transition{:event :* :target :fail}]]
           [:fsm/final#pass]
           [:fsm/final#fail]]
          {}
          {:var1 0})]
    (assert-states fsm #{:pass})))

(deftest w3c-ecma-372
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#s0 {:fsm/initial :s0final}
            [:fsm/transition #:fsm.transition{:event :done.state.s0
                                              :target :pass
                                              :cond (fn [fsm & _] (= 2 (c/get-in-data fsm :var1)))}]
            [:fsm/transition #:fsm.transition{:event :*
                                              :target :fail}]
            [:fsm/final#s0final
             {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :var1 2))
              :fsm.on/exit (fn [fsm & _] (c/assoc-in-data fsm :var1 3))}]]
           [:fsm/final#pass]
           [:fsm/final#fail]]
          {}
          {:var1 1})]
    (assert-states fsm #{:pass})))

(deftest w3c-ecma-372b
  (let [fsm
        (make-fsm
          [:fsm/root
           ;; :done.state.some.namespace/s0
           [:fsm/state {:fsm/initial :s0final :fsm/id :some.namespace/s0}
            [:fsm/transition #:fsm.transition{:event :done.state.some.namespace/s0
                                              :target :pass
                                              :cond (fn [fsm & _] (= 2 (c/get-in-data fsm :var1)))}]
            [:fsm/transition #:fsm.transition{:event :*
                                              :target :fail}]
            [:fsm/final#s0final
             {:fsm.on/enter (fn [fsm & _] (c/assoc-in-data fsm :var1 2))
              :fsm.on/exit (fn [fsm & _] (c/assoc-in-data fsm :var1 3))}]]
           [:fsm/final#pass]
           [:fsm/final#fail]]
          {}
          {:var1 1})]
    (assert-states fsm #{:pass})))

(deftest w3c-ecma-570
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p0
            {:fsm.on/enter (fn [fsm & _]
                             (-> fsm
                               (c/raise {:fsm/event :e1})
                               (c/raise {:fsm/event :e2})))}
            [:fsm/transition #:fsm.transition{:event :done.state.p0s1
                                              :fsm/on (fn [fsm & _] (c/assoc-in-data fsm :var1 1))}]
            [:fsm/transition #:fsm.transition{:event :done.state.p0s2
                                              :target :s1}]
            [:fsm/state#p0s1 {:fsm.initial :p0s11}
             [:fsm/state#p0s11
              [:fsm/transition #:fsm.transition{:event :e1 :target :p0s1final}]]
             [:fsm/final#p0s1final]]

            [:fsm/state#p0s2 {:fsm.initial :p0s21}
             [:fsm/state#p0s21
              [:fsm/transition #:fsm.transition{:event :e2 :target :p0s2final}]]
             [:fsm/final#p0s2final]]]
           [:fsm/state#s1
            [:fsm/transition #:fsm.transition{:event :done.state.p0
                                              :cond (fn [fsm & _] (= 1 (c/get-in-data fsm :var1)))
                                              :target :pass}]]
           [:fsm/final#pass]
           [:fsm/final#fail]]
          {}
          {:var1 0})]
    (assert-states fsm #{:pass})))

(deftest w3c-ecma-570b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p0
            {:fsm.on/enter (fn [fsm & _]
                             (-> fsm
                               (c/raise {:fsm/event :e1})
                               (c/raise {:fsm/event :e2})))}
            [:fsm/transition #:fsm.transition{:event :done.state.some.namespace/p0s1
                                              :fsm/on (fn [fsm & _] (c/assoc-in-data fsm :var1 1))}]
            [:fsm/transition #:fsm.transition{:event :done.state.p0s2
                                              :target :s1}]
            [:fsm/state {:fsm.initial :p0s11 :fsm/id :some.namespace/p0s1}
             [:fsm/state#p0s11
              [:fsm/transition #:fsm.transition{:event :e1 :target :p0s1final}]]
             [:fsm/final#p0s1final]]

            [:fsm/state#p0s2 {:fsm.initial :p0s21}
             [:fsm/state#p0s21
              [:fsm/transition #:fsm.transition{:event :e2 :target :p0s2final}]]
             [:fsm/final#p0s2final]]]
           [:fsm/state#s1
            [:fsm/transition #:fsm.transition{:event :done.state.p0
                                              :cond (fn [fsm & _] (= 1 (c/get-in-data fsm :var1)))
                                              :target :pass}]]
           [:fsm/final#pass]
           [:fsm/final#fail]]
          {}
          {:var1 0})]
    (assert-states fsm #{:pass})))

(deftest history-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :h :event :t1}]]
           [:fsm/state#b
            [:fsm/history#h
             [:fsm/transition #:fsm.transition{:target :b2}]]
            [:fsm/state#b1]
            [:fsm/state#b2
             [:fsm/transition #:fsm.transition{:event :t2 :target :b3}]]
            [:fsm/state#b3
             [:fsm/transition #:fsm.transition{:event :t3 :target :a}]]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b2}]
      [{:fsm/event :t2} #{:b3}]
      [{:fsm/event :t3} #{:a}]
      [{:fsm/event :t1} #{:b3}])))

(deftest history-1
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :h :event :t1}]]
           [:fsm/state#b {:fsm/initial :h}
            [:fsm/history#h {:fsm.history/type :deep}
             [:fsm/transition #:fsm.transition{:target :b1_2}]]
            [:fsm/state#b1 {:fsm/initial :b1_1}
             [:fsm/state#b1_1]
             [:fsm/state#b1_2
              [:fsm/transition #:fsm.transition{:event :t2 :target :b1_3}]]
             [:fsm/state#b1_3
              [:fsm/transition #:fsm.transition{:event :t3 :target :a}]]]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b1_2}]
      [{:fsm/event :t2} #{:b1_3}]
      [{:fsm/event :t3} #{:a}]
      [{:fsm/event :t1} #{:b1_3}])))

(deftest history-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :h :event :t1}]]
           [:fsm/state#b {:fsm/initial :h}
            [:fsm/history#h
             [:fsm/transition #:fsm.transition{:target :b1_2}]]
            [:fsm/state#b1 {:fsm/initial :b1_1}
             [:fsm/state#b1_1]
             [:fsm/state#b1_2
              [:fsm/transition #:fsm.transition{:event :t2 :target :b1_3}]]
             [:fsm/state#b1_3
              [:fsm/transition #:fsm.transition{:event :t3 :target :a}]]]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b1_2}]
      [{:fsm/event :t2} #{:b1_3}]
      [{:fsm/event :t3} #{:a}]
      [{:fsm/event :t1} #{:b1_1}])))

(deftest history-3
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :p :event :t1}]
            [:fsm/transition #:fsm.transition{:target :h :event :t4}]]
           [:fsm/parallel#p
            [:fsm/history#h {:fsm.history/type :deep}
             [:fsm/transition #:fsm.transition{:target :b}]]
            [:fsm/state#b {:fsm/initial :b1}
             [:fsm/state#b1
              [:fsm/transition #:fsm.transition{:target :b2 :event :t2}]]
             [:fsm/state#b2]]
            [:fsm/state#c {:fsm/initial :c1}
             [:fsm/state#c1
              [:fsm/transition #:fsm.transition{:target :c2 :event :t2}]]
             [:fsm/state#c2]]
            [:fsm/transition #:fsm.transition{:target :a :event :t3}]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b1 :c1}]
      [{:fsm/event :t2} #{:b2 :c2}]
      [{:fsm/event :t3} #{:a}]
      [{:fsm/event :t4} #{:b2 :c2}])))

(deftest history-4
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :p :event :t1}]
            [:fsm/transition #:fsm.transition{:target :p :event :t6}]
            [:fsm/transition #:fsm.transition{:target :hp :event :t9}]]

           [:fsm/parallel#p
            [:fsm/history#hp {:fsm.history/type :deep}
             [:fsm/transition #:fsm.transition{:target :b1}]]

            [:fsm/state#b {:fsm/initial :hb}
             [:fsm/history#hb {:fsm.history/type :deep}
              [:fsm/transition #:fsm.transition{:target :b1}]]

             [:fsm/state#b1 {:fsm/initial :b1_1}
              [:fsm/state#b1_1
               [:fsm/transition #:fsm.transition{:target :b1_2 :event :t2}]]
              [:fsm/state#b1_2
               [:fsm/transition #:fsm.transition{:target :b2 :event :t3}]]]

             [:fsm/state#b2 {:fsm/initial :b2_1}
              [:fsm/state#b2_1
               [:fsm/transition #:fsm.transition{:target :b2_2 :event :t4}]]
              [:fsm/state#b2_2
               [:fsm/transition #:fsm.transition{:target :a :event :t5}]
               [:fsm/transition #:fsm.transition{:target :a :event :t8}]]]]

            [:fsm/state#c {:fsm/initial :hc}
             [:fsm/history#hc {:fsm.history/type :shallow}
              [:fsm/transition #:fsm.transition{:target :c1}]]

             [:fsm/state#c1 {:fsm/initial :c1_1}
              [:fsm/state#c1_1
               [:fsm/transition #:fsm.transition{:target :c1_2 :event :t2}]]

              [:fsm/state#c1_2
               [:fsm/transition #:fsm.transition{:target :c2 :event :t3}]]]

             [:fsm/state#c2 {:fsm/initial :c2_1}
              [:fsm/state#c2_1
               [:fsm/transition #:fsm.transition{:target :c2_2 :event :t4}]
               [:fsm/transition #:fsm.transition{:target :c2_2 :event :t7}]]

              [:fsm/state#c2_2]]]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b1_1 :c1_1}]
      [{:fsm/event :t2} #{:b1_2 :c1_2}]
      [{:fsm/event :t3} #{:b2_1 :c2_1}]
      [{:fsm/event :t4} #{:b2_2 :c2_2}]
      [{:fsm/event :t5} #{:a}]
      [{:fsm/event :t6} #{:b2_2 :c2_1}]
      [{:fsm/event :t7} #{:b2_2 :c2_2}]
      [{:fsm/event :t8} #{:a}]
      [{:fsm/event :t9} #{:b2_2 :c2_2}])))

(deftest history-4b
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :p :event :t1}]
            [:fsm/transition #:fsm.transition{:target #{:hb :hc} :event :t6}]
            [:fsm/transition #:fsm.transition{:target :hp :event :t9}]]

           [:fsm/parallel#p
            [:fsm/history#hp {:fsm.history/type :deep}
             [:fsm/transition #:fsm.transition{:target :b1}]]

            [:fsm/state#b {:fsm/initial :hb}
             [:fsm/history#hb {:fsm.history/type :deep}
              [:fsm/transition #:fsm.transition{:target :b1}]]

             [:fsm/state#b1 {:fsm/initial :b1_1}
              [:fsm/state#b1_1
               [:fsm/transition #:fsm.transition{:target :b1_2 :event :t2}]]
              [:fsm/state#b1_2
               [:fsm/transition #:fsm.transition{:target :b2 :event :t3}]]]

             [:fsm/state#b2 {:fsm/initial :b2_1}
              [:fsm/state#b2_1
               [:fsm/transition #:fsm.transition{:target :b2_2 :event :t4}]]
              [:fsm/state#b2_2
               [:fsm/transition #:fsm.transition{:target :a :event :t5}]
               [:fsm/transition #:fsm.transition{:target :a :event :t8}]]]]

            [:fsm/state#c {:fsm/initial :hc}
             [:fsm/history#hc {:fsm.history/type :shallow}
              [:fsm/transition #:fsm.transition{:target :c1}]]

             [:fsm/state#c1 {:fsm/initial :c1_1}
              [:fsm/state#c1_1
               [:fsm/transition #:fsm.transition{:target :c1_2 :event :t2}]]

              [:fsm/state#c1_2
               [:fsm/transition #:fsm.transition{:target :c2 :event :t3}]]]

             [:fsm/state#c2 {:fsm/initial :c2_1}
              [:fsm/state#c2_1
               [:fsm/transition #:fsm.transition{:target :c2_2 :event :t4}]
               [:fsm/transition #:fsm.transition{:target :c2_2 :event :t7}]]

              [:fsm/state#c2_2]]]]])]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b1_1 :c1_1}]
      [{:fsm/event :t2} #{:b1_2 :c1_2}]
      [{:fsm/event :t3} #{:b2_1 :c2_1}]
      [{:fsm/event :t4} #{:b2_2 :c2_2}]
      [{:fsm/event :t5} #{:a}]
      [{:fsm/event :t6} #{:b2_2 :c2_1}]
      [{:fsm/event :t7} #{:b2_2 :c2_2}]
      [{:fsm/event :t8} #{:a}]
      [{:fsm/event :t9} #{:b2_2 :c2_2}])))

(deftest history-5
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/history#ha {:fsm.history/type :deep}
             [:fsm/transition #:fsm.transition{:target :b}]]
            [:fsm/parallel#b
             [:fsm/parallel#c
              [:fsm/parallel#d
               [:fsm/parallel#e
                [:fsm/state#i
                 [:fsm/state#i1
                  [:fsm/transition #:fsm.transition{:target :i2 :event :t1}]]
                 [:fsm/state#i2
                  [:fsm/transition #:fsm.transition{:target :l :event :t2}]]]
                [:fsm/state#j]]
               [:fsm/state#h]]
              [:fsm/state#g]]
             [:fsm/state#f
              [:fsm/state#f1
               [:fsm/transition #:fsm.transition{:target :f2 :event :t1}]]
              [:fsm/state#f2]]]
            [:fsm/state#k]]
           [:fsm/state#l
            [:fsm/transition #:fsm.transition{:target :ha :event :t3}]]])]
    (assert-states
      fsm #{:i1 :j :h :g :f1 :k}
      [{:fsm/event :t1} #{:i2 :j :h :g :f2 :k}]
      [{:fsm/event :t2} #{:l}]
      [{:fsm/event :t3} #{:i2 :j :h :g :f2 :k}])))

(deftest history-6
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :h :event :t1}]]
           [:fsm/state#b
            {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm :x * 3))}
            [:fsm/history#h
             [:fsm/transition #:fsm.transition{:target :b2}]]
            [:fsm/state#b1]
            [:fsm/state#b2
             {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm :x * 5))}
             [:fsm/transition #:fsm.transition{:target :b3 :event :t2}]]
            [:fsm/state#b3
             {:fsm.on/enter (fn [fsm & _] (c/update-in-data fsm :x * 7))}
             [:fsm/transition #:fsm.transition{:target :a :event :t3}]]

            [:fsm/transition #:fsm.transition{:target :success
                                              :event :t4
                                              :cond (fn [fsm & _] (= 4410 (c/get-in-data fsm :x)))}]
            [:fsm/transition #:fsm.transition{:target :fail
                                              :event :t4}]]
           [:fsm/state#success]
           [:fsm/state#fail]]
          {}
          {:x 2})]
    (assert-states
      fsm #{:a}
      [{:fsm/event :t1} #{:b2}]
      [{:fsm/event :t2} #{:b3}]
      [{:fsm/event :t3} #{:a}]
      [{:fsm/event :t1} #{:b3}]
      [{:fsm/event :t4} #{:success}])))

(deftest event-is-passed-to-handlers
  (let [fsm  (make-fsm
               [:fsm/root
                [:fsm/state#a
                 {:fsm.on/exit (fn [fsm ev]
                                 (is (= {:fsm/event :t :foo :bar} ev))
                                 (c/update-in-data fsm :handler-calls conj [:exit :a]))}
                 [:fsm/transition
                  #:fsm.transition{:event :t
                                   :target :b
                                   :fsm/on (fn [fsm ev]
                                             (is (= {:fsm/event :t :foo :bar} ev))
                                             (c/update-in-data fsm :handler-calls conj :transition))}]]
                [:fsm/state#b
                 {:fsm.on/enter (fn [fsm ev]
                                  (is (= {:fsm/event :t :foo :bar} ev))
                                  (c/update-in-data fsm :handler-calls conj [:enter :b]))}]]
               {}
               {:handler-calls []})
        fsm' (c/trigger fsm {:fsm/event :t :foo :bar})]
    (is (= #{:a} (c/get-active-atomic-states fsm)))
    (is (= #{:b} (c/get-active-atomic-states fsm')))
    (is (= [[:exit :a] :transition [:enter :b]] (get-in fsm' [:fsm/data :handler-calls])))))

(deftest sending-data
  (let [fsm  (make-fsm
               [:fsm/root
                [:fsm/state#a
                 {:fsm.on/exit (fn [fsm & _]
                                 (c/send fsm {:fsm/event :t1}))}
                 [:fsm/transition
                  #:fsm.transition{:event :t
                                   :target :b
                                   :fsm/on (fn [fsm & _]
                                             (c/send fsm {:fsm/event :t2}))}]]
                [:fsm/state#b
                 {:fsm.on/enter (fn [fsm & _]
                                  (c/send fsm {:fsm/event :t3}))}]])
        fsm' (c/trigger fsm {:fsm/event :t :foo :bar})]
    (is (= #{:a} (c/get-active-atomic-states fsm)))
    (is (= #{:b} (c/get-active-atomic-states fsm')))
    (is (= [{:fsm/event :t1} {:fsm/event :t2} {:fsm/event :t3}] (c/get-events fsm')))))

(defn benchmark []
  (let [fsm [:fsm/root {:fsm/initial :a}
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
             [:fsm/state#l]]
        compiled-fsm (c/compile fsm)
        run-fn (fn []
                 (let [f (c/start compiled-fsm)]
                   (c/trigger f {:fsm/event :t})))
        started-fsm (c/start compiled-fsm)
        run-fn1 (fn []
                  (c/trigger started-fsm {:fsm/event :t}))]
    (cr/with-progress-reporting (cr/bench (run-fn) :verbose))))

(defn profile []
  (let [fsm [:fsm/root {:fsm/initial :a}
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
             [:fsm/state#l]]
        compiled-fsm (c/compile fsm)
        run-fn (fn profile-runner []
                 (let [f (c/start compiled-fsm)]
                   (c/trigger f {:fsm/event :t})))]
    (prof/profile (dotimes [i 100000] (run-fn)))))

(comment
  (benchmark)
  (profile))