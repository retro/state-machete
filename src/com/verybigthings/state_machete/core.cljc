(ns com.verybigthings.state-machete.core
  (:refer-clojure :exclude [compile])
  (:require [com.verybigthings.state-machete.hiccup :as hiccup]
            [com.verybigthings.state-machete.util :refer [lexicographic-compare]]
            [clojure.set :as set]))

(def compile hiccup/compile)

(defn get-active-states [fsm]
  (-> (get-in fsm [:fsm/state :active]) keys set))

(defn get-active-atomic-states [fsm]
  (->> (get-in fsm [:fsm/state :active])
    (filter (fn [[_ is-atomic]] is-atomic))
    (map first)
    set))

(defn enter-states [fsm]
  (reduce
    (fn [acc state-id]
      (let [state (get-in acc [:fsm/index :by-id state-id])]
        (-> acc
          (update-in [:fsm/session :to-enter] rest)
          (assoc-in [:fsm/state :active state-id] (= :atomic (:fsm.state/type state))))))
    fsm
    (get-in fsm [:fsm/session :to-enter])))

(defn exit-states [fsm]
  (reduce
    (fn [acc state-id]
      (-> acc
        (update-in [:fsm/session :to-exit] rest)
        (update-in [:fsm/state :active] dissoc state-id)))
    fsm
    (get-in fsm [:fsm/session :to-exit])))

(defn descendant-path? [source target]
  (let [source-length (count source)
        target-length (count target)]
    (and
      (> target-length source-length)
      (= source (subvec target 0 source-length)))))

(defn select-self-and-descendant-states-to-enter
  ([fsm state] (select-self-and-descendant-states-to-enter fsm state {} []))
  ([fsm state initial-ids] (select-self-and-descendant-states-to-enter fsm state initial-ids []))
  ([fsm state initial-ids selected-ids]
   (let [selected-ids' (conj selected-ids (:fsm/id state))]
     (cond
       (= :fsm/parallel (:fsm/type state))
       (reduce
         (fn [acc child-state]
           (select-self-and-descendant-states-to-enter fsm child-state initial-ids acc))
         selected-ids'
         (:fsm.children/states state))

       (= :compound (:fsm.state/type state))
       (let [initial-child-id    (or (get initial-ids (:fsm/id state)) (:fsm/initial state))
             initial-child-state (get-in fsm [:fsm/index :by-id initial-child-id])]
         (select-self-and-descendant-states-to-enter fsm initial-child-state initial-ids selected-ids'))

       :else
       selected-ids'))))

(defn select-transition-states-to-exit [fsm {:keys [transition domain]}]
  (let [domain-path   (:fsm/path domain)
        active-states (get-in fsm [:fsm/state :active])
        exit-paths    (reduce
                        (fn [acc state-id]
                          (let [state (get-in fsm [:fsm/index :by-id state-id])]
                            (if (and (contains? active-states state-id) (descendant-path? domain-path (:fsm/path state)))
                              (conj acc (:fsm/path state))
                              acc)))
                        #{}
                        (get-active-states fsm))]
    (->> exit-paths
      (sort lexicographic-compare)
      reverse
      (map #(get-in fsm [:fsm/index :by-path % :fsm/id]))
      seq)))

(defn select-transition-states-to-enter [fsm {:keys [transition domain]}]
  (let [domain-id (:fsm/id domain)
        target-state-ids (:fsm.transition/target transition)
        initial-ids (reduce
                      (fn [acc state-id]
                        (loop [id           state-id
                               selected-ids acc]
                          (let [state (get-in fsm [:fsm/index :by-id id])]
                            (if (= id domain-id)
                              selected-ids
                              (let [parent-id (get-in state [:fsm/parent-state :fsm/id])]
                                (recur parent-id (assoc selected-ids parent-id id)))))))
                    {}
                    target-state-ids)]
    (->> (select-self-and-descendant-states-to-enter fsm domain initial-ids)
      rest
      (sort #(lexicographic-compare
               (get-in fsm [:fsm/index :by-id %1])
               (get-in fsm [:fsm/index :by-id %2])))
      seq)))

(defn get-event-transition-for-state [fsm event state-id]
  (when-let [state (get-in fsm [:fsm/index :by-id state-id])]
    (let [event-name          (:fsm/event event)
          matching-transition (reduce
                                (fn [acc transition]
                                  (cond
                                    (contains? (:fsm.transition/event transition) event-name) (reduced transition)
                                    (= :* (:fsm.transition/event transition)) transition
                                    :else acc))
                                nil
                                (:fsm.children/transitions state))]
      (or
        matching-transition
        (recur fsm event (get-in state [:fsm/parent-state :fsm/id]))))))

(defn get-transition-domain [fsm transition]
  (when transition
    (let [transition-parent-state-path (get-in transition [:fsm/parent-state :fsm/path])
          transition-target-ids        (:fsm.transition/target transition)
          transition-targets-paths     (->> transition-target-ids
                                         (map #(get-in fsm [:fsm/index :by-id % :fsm/path])))
          common-ancestor-path         (loop [p transition-parent-state-path
                                              i 0]
                                         (let [comparing-path (or (subvec p 0 i) [])]
                                           (cond
                                             (not (every? #(descendant-path? comparing-path %) transition-targets-paths))
                                             (subvec p 0 (dec i))

                                             (= p comparing-path)
                                             comparing-path

                                             :else
                                             (recur p (inc i)))))]
      (loop [p common-ancestor-path]
        (when-let [state (get-in fsm [:fsm/index :by-path p])]
          (if (= :compound (:fsm.state/type state))
            state
            (recur (get-in state [:fsm/parent-state :fsm/path]))))))))

(defn get-event-transitions-with-domain [fsm event]
  (let [atomic-states (get-active-atomic-states fsm)
        transitions   (reduce
                        (fn [acc state-id]
                          (let [transition (get-event-transition-for-state fsm event state-id)
                                domain     (get-transition-domain fsm transition)]
                            (if transition
                              (conj acc {:transition transition :domain domain})
                              acc)))
                        #{}
                        atomic-states)
        sorted-transitions (sort
                             #(lexicographic-compare
                                (get-in %1 [:domain :fsm/path])
                                (get-in %2 [:domain :fsm/path]))
                             transitions)]
    ;; TODO: Filter out transitions with overlapping domains
    (reduce
      (fn [acc t]
        (let [paths (map #(get-in % [:domain :fsm/path]) acc)
              path (get-in t [:domain :fsm/path])]
          (if (some #(descendant-path? % path) paths)
            acc
            (conj acc t))))
      #{}
      sorted-transitions)))

(defn start
  ([fsm] (start fsm {}))
  ([fsm data]
   (let [root-state    (get-in fsm [:fsm/index :by-path []])
         to-enter  (select-self-and-descendant-states-to-enter fsm root-state)]
     (-> fsm
       (assoc :fsm/data data
              :fsm/session {:inbound [] :outbound [] :to-exit [] :to-enter to-enter})
       enter-states))))

(defn trigger [fsm event]
  (let [transitions-with-domain (get-event-transitions-with-domain fsm event)
        to-exit                 (mapcat #(select-transition-states-to-exit fsm %) transitions-with-domain)
        to-enter                (mapcat #(select-transition-states-to-enter fsm %) transitions-with-domain)]
    (println (map #(get-in % [:domain :fsm/id]) transitions-with-domain))
    (println "TO EXIT" to-exit)
    (println "TO ENTER" to-enter)
    (println "--------------------------------------")
    (-> fsm
      (assoc :fsm/session {:inbound [] :outbound [] :to-exit to-exit :to-enter to-enter})
      exit-states
      ;; TODO: run transition functions
      enter-states)))

