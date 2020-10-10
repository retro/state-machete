(ns com.verybigthings.state-machete.core
  (:refer-clojure :exclude [compile])
  (:require [com.verybigthings.state-machete.hiccup :as hiccup]
            [com.verybigthings.state-machete.util :refer [lexicographic-compare keyword-or-coll->set]]
            [clojure.set :as set]))

(def compile hiccup/compile)

(defn get-active-states [fsm]
  (-> (get-in fsm [:fsm/state :active]) keys set))

(defn get-active-atomic-states [fsm]
  (->> (get-in fsm [:fsm/state :active])
    (filter (fn [[_ is-atomic]] is-atomic))
    (map first)
    set))

(defn get-nil-event-transition [fsm state-id]
  (let [state      (get-in fsm [:fsm/index :by-id state-id])
        transition (->> (:fsm.children/transitions state)
                     (filter (fn [t]
                               (let [t-cond  (:fsm.transition/cond t)
                                     t-event (:fsm.transition/event t)]
                                 (and (nil? t-event) (t-cond fsm nil)))))
                     first)]
    (if transition
      transition
      (when-let [parent-state-id (get-in state [:fsm/parent-state :fsm/id])]
        (recur fsm parent-state-id)))))

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

(defn select-transition-states-to-exit [fsm {:keys [domain]}]
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
  (let [domain-id        (:fsm/id domain)
        target-state-ids (:fsm.transition/target transition)
        initial-ids      (reduce
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
               (get-in fsm [:fsm/index :by-id %1 :fsm/path])
               (get-in fsm [:fsm/index :by-id %2 :fsm/path])))
      seq)))

(defn get-event-transition-for-state [fsm event state-id]
  (when-let [state (get-in fsm [:fsm/index :by-id state-id])]
    (let [event-name          (when-let [e (:fsm/event event)] (name e))
          matching-transition (->> (:fsm.children/transitions state)
                                (filter (fn [t]
                                          (let [t-cond (:fsm.transition/cond t)]
                                            (->> (:fsm.transition/event t)
                                              (filter (fn [matcher] (and (t-cond fsm event) (matcher event-name))))
                                              first))))
                                first)]
      (or
        matching-transition
        (recur fsm event (get-in state [:fsm/parent-state :fsm/id]))))))

(defn get-transition-parent-state [fsm transition]
  (let [transition-state-id (get-in transition [:fsm/parent-state :fsm/id])
        transition-state    (get-in fsm [:fsm/index :by-id transition-state-id])]
    (loop [state-id (get-in transition-state [:fsm/parent-state :fsm/id])]
      (let [state (get-in fsm [:fsm/index :by-id state-id])]
        (when state
          (if (= :compound (:fsm.state/type state))
            state
            (recur (get-in state [:fsm/parent-state :fsm/id]))))))))

(defn get-transition-domain [fsm transition]
  (when transition
    (let [transition-parent-state      (get-transition-parent-state fsm transition)
          transition-parent-state-path (:fsm/path transition-parent-state)
          transition-target-ids        (:fsm.transition/target transition)
          transition-targets-paths     (remove nil? (map #(get-in fsm [:fsm/index :by-id % :fsm/path]) transition-target-ids))
          common-ancestor-path         (when (seq transition-targets-paths)
                                         (loop [p transition-parent-state-path
                                                i 0]
                                           (let [comparing-path (or (subvec p 0 i) [])]
                                             (cond
                                               (not (every? #(descendant-path? comparing-path %) transition-targets-paths))
                                               (subvec p 0 (dec i))

                                               (= p comparing-path)
                                               comparing-path

                                               :else
                                               (recur p (inc i))))))]
      (loop [p common-ancestor-path]
        (when-let [state (get-in fsm [:fsm/index :by-path p])]
          (if (= :compound (:fsm.state/type state))
            state
            (recur (get-in state [:fsm/parent-state :fsm/path]))))))))

(defn paths-overlap? [p1 p2]
  (let [c1 (count p1)
        c2 (count p2)]
    (if (or (zero? c1) (zero? c2))
      true
      (let [c (min c1 c2)]
        (= (subvec p1 0 c) (subvec p2 0 c))))))

(defn get-event-transitions-with-domain [fsm event]
  (let [atomic-states      (get-active-atomic-states fsm)
        transitions        (reduce
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
                                (get-in %1 [:transition :fsm/path])
                                (get-in %2 [:transition :fsm/path]))
                             transitions)]
    (reduce
      (fn [acc t]
        (let [paths (map #(get-in % [:domain :fsm/path]) acc)
              path  (get-in t [:domain :fsm/path])]
          (if (some #(paths-overlap? % path) paths)
            acc
            (conj acc t))))
      #{}
      sorted-transitions)))

(defn enter-states [fsm to-enter]
  (println "TO ENTER" to-enter)
  (reduce
    (fn [acc state-id]
      (let [state                (get-in acc [:fsm/index :by-id state-id])
            enter-handler        (:fsm.on/enter state)
            fsm'                 (-> acc
                                   (assoc-in [:fsm/state :active state-id] (= :atomic (:fsm.state/type state)))
                                   enter-handler)
            nil-event-transition (get-nil-event-transition fsm' state-id)]
        (if nil-event-transition
          (update-in fsm' [:fsm/session :inbound] conj {:transition nil-event-transition})
          fsm')))
    fsm
    to-enter))

(defn exit-states [fsm to-exit]
  (println "TO EXIT" to-exit)
  (reduce
    (fn [acc state-id]
      (let [exit-handler (get-in fsm [:fsm/index :by-id state-id :fsm.on/exit])]
        (-> acc
          (update-in [:fsm/session :to-exit] rest)
          (update-in [:fsm/state :active] dissoc state-id)
          exit-handler)))
    fsm
    to-exit))

(defn transition-states [fsm]
  (let [transitions-with-domain (get-in fsm [:fsm/session :transitions])]
    (reduce
      (fn [acc t]
        (let [to-exit  (select-transition-states-to-exit fsm t)
              to-enter (select-transition-states-to-enter fsm t)
              handler  (get-in t [:transition :fsm/on])]
          (println (get-in t [:transition :fsm/parent-state :fsm/id]) (get-in t [:transition :fsm.transition/event]))
          (-> acc
            (exit-states to-exit)
            handler
            (enter-states to-enter)
            (update-in [:fsm/session :transitions] rest))))
      fsm
      transitions-with-domain)))

(declare trigger)
(declare run-small-step)

(defn trigger-nil [fsm transition]
  (println "TRIGGER NIL --------------------------------")
  (let [domain                 (get-transition-domain fsm transition)
        transition-with-domain {:transition transition :domain domain}]
    (-> fsm
      (update :fsm/session merge {:transitions [transition-with-domain]})
      transition-states
      run-small-step)))

(defn run-small-step [fsm]
  (let [inbound (get-in fsm [:fsm/session :inbound])]
    (if (seq inbound)
      (let [{:keys [event transition]} (first inbound)
            fsm' (update-in fsm [:fsm/session :inbound] rest)]
        (if (nil? event)
          (trigger-nil fsm' transition)
          (trigger fsm' event)))
      fsm)))

(defn start
  ([fsm] (start fsm {}))
  ([fsm data]
   (println "START --------------------------------------")
   (let [root-state (get-in fsm [:fsm/index :by-path []])
         to-enter   (select-self-and-descendant-states-to-enter fsm root-state)]
     (-> fsm
       (assoc :fsm/data data
              :fsm/session {:inbound [] :outbound []})
       (enter-states to-enter)
       run-small-step))))

(defn trigger [fsm event]
  (println "TRIGGER ------------------------------------")
  (println event)
  (let [transitions-with-domain (get-event-transitions-with-domain fsm event)]
    (println (map #(get-in % [:domain :fsm/id]) transitions-with-domain))
    (-> fsm
      (update :fsm/session merge {:outbound [] :transitions transitions-with-domain})
      transition-states
      run-small-step)))

(defn update-data [fsm & args]
  (apply update fsm :fsm/data args))

(defn update-in-data [fsm path & args]
  (apply update-in fsm (into [:fsm/data] path) args))

(defn get-data [fsm]
  (:fsm/data fsm))

(defn get-in-data [fsm path]
  (get-in fsm (into [:fsm/data] path)))

(defn assoc-data [fsm data]
  (assoc fsm :fsm/data data))

(defn assoc-in-data [fsm path val]
  (assoc-in fsm (into [:fsm/data] path) val))

(defn dissoc-data [fsm]
  (dissoc fsm :fsm/data))



