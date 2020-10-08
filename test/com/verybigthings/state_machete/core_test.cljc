(ns com.verybigthings.state-machete.core-test
  (:require [clojure.test :refer :all]
            [com.verybigthings.state-machete.core :as c]))

(defn log [& forms]
  (doseq [f forms]
    (println (with-out-str (clojure.pprint/pprint f)))))

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
    (is (= #{:a} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:b} (c/get-active-atomic-states fsm'))))))

(deftest basic-2
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state#a
            [:fsm/transition #:fsm.transition{:target :b :event :t}]]
           [:fsm/state#b
            [:fsm/transition #:fsm.transition{:target :c :event :t2}]]
           [:fsm/state#c]])]
    (is (= #{:a} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:b} (c/get-active-atomic-states fsm')))
      (let [fsm'' (c/trigger fsm' {:fsm/event :t2})]
        (is (= #{:c} (c/get-active-atomic-states fsm'')))))))

(deftest document-order-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/state {:fsm/id :a}
            [:fsm/transition #:fsm.transition{:target :b :event :t}]
            [:fsm/transition #:fsm.transition{:target :c :event :t}]]
           [:fsm/state {:fsm/id :b}]
           [:fsm/state {:fsm/id :c}]])]
    (is (= #{:a} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:b} (c/get-active-atomic-states fsm'))))))

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
    (is (= #{:a1 :b1} (c/get-active-atomic-states fsm))
      (let [fsm' (c/trigger fsm {:fsm/event :t})]
        (is (= #{:a2 :b2} (c/get-active-atomic-states fsm')))))))

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
    (is (= #{:s3 :s4 :s7 :s8} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:s5 :s6 :s9 :s10} (c/get-active-atomic-states fsm'))))))

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
    (is (= #{:s3_1 :s4 :s7 :s8} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:s3_2 :s4 :s9 :s10} (c/get-active-atomic-states fsm'))))))

(deftest more-parallel-0
  (let [fsm
        (make-fsm
          [:fsm/root
           [:fsm/parallel#p
            [:fsm/state#a
             [:fsm/transition #:fsm.transition{:target :a :event :t}]]
            [:fsm/state#b]]])]
    (is (= #{:a :b} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:a :b} (c/get-active-atomic-states fsm'))))))

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
    (is (= #{:a1 :b1} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:a1 :b1} (c/get-active-atomic-states fsm'))))))

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
    (is (= #{:a1 :b1} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:a1 :b1} (c/get-active-atomic-states fsm'))))))

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
    (is (= #{:a1 :b1} (c/get-active-atomic-states fsm)))
    (let [fsm' (c/trigger fsm {:fsm/event :t})]
      (is (= #{:a1 :b1} (c/get-active-atomic-states fsm'))))))