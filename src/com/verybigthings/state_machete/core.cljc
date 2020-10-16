(ns com.verybigthings.state-machete.core
  (:refer-clojure :exclude [compile send])
  (:require [com.verybigthings.state-machete.hiccup :as hiccup]
            [com.verybigthings.state-machete.util :refer [lexicographic-compare keyword-or-coll->set first-identity]]
            [clojure.set :as set]))

(def compile hiccup/compile)

(defn- get-system-time [] #?(:cljs (js/Date.now) :clj (System/currentTimeMillis)))

(defn as-path [path]
  (if (sequential? path) path [path]))

(defn descendant-path? [source target]
  (let [source-length (count source)
        target-length (count target)]
    (and
      (> target-length source-length)
      (= source (subvec target 0 source-length)))))

(def set-conj (fnil conj #{}))
(def vec-conj (fnil conj []))

(defn paths-overlap? [p1 p2]
  (let [c1 (count p1)
        c2 (count p2)]
    (if (or (zero? c1) (zero? c2))
      true
      (let [c (min c1 c2)]
        (= (subvec p1 0 c) (subvec p2 0 c))))))

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
      {:transition transition}
      (when-let [parent-state-id (get-in state [:fsm/parent-state :fsm/id])]
        (recur fsm parent-state-id)))))

(defn get-history-target-states [fsm state]
  (let [transition (-> state :fsm.children/transitions first)]
    (or (get-in fsm [:fsm/state :histories (:fsm/id state)]) (:fsm.transition/target transition))))

(defn expand-history-states-ids [fsm state-ids]
  (reduce
    (fn [acc state-id]
      (let [state (get-in fsm [:fsm/index :by-id state-id])]
        (if (= :fsm/history (:fsm/type state))
          (set/union acc (get-history-target-states fsm state))
          acc)))
    state-ids
    state-ids))

(declare select-self-and-descendant-states-to-enter*)

(defn select-descendant-states-to-enter-for-parallel* [fsm state initial-ids selected-ids]
  (reduce
    (fn [acc child-state]
      (select-self-and-descendant-states-to-enter* fsm child-state initial-ids acc))
    selected-ids
    (remove #(= :fsm/history (:fsm/type %)) (:fsm.children/states state))))

(defn select-descendant-states-to-enter-for-history* [fsm state initial-ids selected-ids]
  (reduce
    (fn [acc history-target-state-id]
      (let [history-target-state (get-in fsm [:fsm/index :by-id history-target-state-id])]
        (select-self-and-descendant-states-to-enter* fsm history-target-state initial-ids acc)))
    selected-ids
    (get-history-target-states fsm state)))

(defn select-descendant-states-to-enter-for-compound* [fsm state initial-ids selected-ids]
  (let [initial-child-id (or (get initial-ids (:fsm/id state)) (:fsm/initial state))]
    (if (coll? initial-child-id)
      ;; When entering history states, we might have multiple children we want to enter -> history one
      ;; and the normal state child. So we might receive a set of ids here
      (reduce
        (fn [acc state-id]
          (let [state (get-in fsm [:fsm/index :by-id state-id])]
            (select-self-and-descendant-states-to-enter* fsm state initial-ids acc)))
        selected-ids
        initial-child-id)
      (let [initial-child-state (get-in fsm [:fsm/index :by-id initial-child-id])]
        (select-self-and-descendant-states-to-enter* fsm initial-child-state initial-ids selected-ids)))))

(defn select-self-and-descendant-states-to-enter* [fsm state initial-ids selected-ids]
  (let [selected-ids' (conj selected-ids [(:fsm/path state) (:fsm/id state)])
        fsm-type      (:fsm/type state)]
    (cond
      (= :fsm/parallel fsm-type)
      (select-descendant-states-to-enter-for-parallel* fsm state initial-ids selected-ids')

      (= :fsm/history fsm-type)
      (select-descendant-states-to-enter-for-history* fsm state initial-ids selected-ids')

      (= :compound (:fsm.state/type state))
      (select-descendant-states-to-enter-for-compound* fsm state initial-ids selected-ids')

      :else
      selected-ids')))

(defn select-self-and-descendant-states-to-enter
  ([fsm state] (select-self-and-descendant-states-to-enter fsm state {}))
  ([fsm state initial-ids]
   (let [selected-ids (select-self-and-descendant-states-to-enter* fsm state initial-ids #{})]
     (->> selected-ids
       seq
       (sort #(lexicographic-compare (first %1) (first %2)))
       (map last)))))

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
                        (get-active-states fsm))
        states-index  (get-in fsm [:fsm/index :by-path])]
    (->> exit-paths
      (sort lexicographic-compare)
      reverse
      (map #(get-in states-index [% :fsm/id]))
      seq)))

(defn select-transition-states-for-targetless-transition [fsm transition-with-domain]
  (reverse (select-transition-states-to-exit fsm transition-with-domain)))

(defn select-transition-states-to-enter [fsm {:keys [transition domain]}]
  (let [domain-id        (:fsm/id domain)
        target-state-ids (expand-history-states-ids fsm (:fsm.transition/target transition))
        initial-ids      (reduce
                           (fn [acc state-id]
                             (loop [id           state-id
                                    selected-ids acc]
                               (if-let [state (get-in fsm [:fsm/index :by-id id])]
                                 (if (= id domain-id)
                                   selected-ids
                                   (let [parent-id (get-in state [:fsm/parent-state :fsm/id])]
                                     (recur parent-id (update selected-ids parent-id set-conj id))))
                                 selected-ids)))
                           {}
                           target-state-ids)
        states-to-enter  (rest (select-self-and-descendant-states-to-enter fsm domain initial-ids))
        state-index      (get-in fsm [:fsm/index :by-id])]
    (->> states-to-enter
      (map (fn [state-id]
             (let [state (get state-index state-id)]
               (if (= :fsm/history (:fsm/type state))
                 ;; Force order of history states, so they are always immediately after their parent (if present in the list)
                 ;; This way it's ensured that history state's transition handler is ran when it should be
                 (update state :fsm/path #(concat (butlast %) [-1]))
                 state))))
      (sort #(lexicographic-compare (:fsm/path %1) (:fsm/path %2)))
      (map :fsm/id)
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
        transition-state    (get-in fsm [:fsm/index :by-id transition-state-id])
        starting-state-id   (if (= :internal (:fsm.transition/type transition))
                              transition-state-id
                              (get-in transition-state [:fsm/parent-state :fsm/id]))]
    (loop [state-id starting-state-id]
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
          common-ancestor-path         (if (seq transition-targets-paths)
                                         (loop [p transition-parent-state-path
                                                i 0]
                                           (let [comparing-path (or (subvec p 0 i) [])]
                                             (cond
                                               (not (every? #(descendant-path? comparing-path %) transition-targets-paths))
                                               (subvec p 0 (dec i))

                                               (= p comparing-path)
                                               comparing-path

                                               :else
                                               (recur p (inc i)))))
                                         transition-parent-state-path)]
      (loop [p common-ancestor-path]
        (when-let [state (get-in fsm [:fsm/index :by-path p])]
          (if (= :compound (:fsm.state/type state))
            state
            (recur (get-in state [:fsm/parent-state :fsm/path]))))))))

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
        (let [paths              (map #(get-in % [:domain :fsm/path]) acc)
              path               (get-in t [:domain :fsm/path])
              is-targetless      (nil? (get-in t [:transition :fsm.transition/target]))
              is-without-overlap (not (some #(paths-overlap? % path) paths))]
          (if (or is-targetless is-without-overlap)
            (conj acc t)
            acc)))
      []
      sorted-transitions)))

(defn get-nil-event-transitions-with-domain [fsm transition]
  (let [domain (get-transition-domain fsm transition)]
    [{:transition transition :domain domain}]))

(defn get-final-event-name [state-id]
  (let [state-id-name (name state-id)
        state-id-ns   (namespace state-id)
        event-name           (if state-id-ns
                               (str "done.state." state-id-ns "/" state-id-name)
                               (str "done.state." state-id-name))]
    (keyword event-name)))

(defn get-final-state-done-event-for-parallel-state [fsm state-id]
  (let [parent-state-id         (get-in fsm [:fsm/index :by-id state-id :fsm/parent-state :fsm/id])
        parent-state            (get-in fsm [:fsm/index :by-id parent-state-id])
        parent-state-path       (:fsm/path parent-state)
        parent-state-path-count (count parent-state-path)]

    (when (= :fsm/parallel (:fsm/type parent-state))
      (let [active-states              (get-active-states fsm)
            active-final-grandchildren (filter
                                         (fn [state-id]
                                           (let [state (get-in fsm [:fsm/index :by-id state-id])
                                                 path  (:fsm/path state)]
                                             (and (= :fsm/final (:fsm/type state))
                                               (= (+ 2 parent-state-path-count) (count path))
                                               (= parent-state-path (subvec path 0 parent-state-path-count)))))
                                         active-states)]
        (when (= (count active-final-grandchildren)
                (count (:fsm.children/states parent-state)))

          {:event {:fsm/event (get-final-event-name parent-state-id) }})))))

(defn state-active? [fsm state]
  (let [state-id (:fsm/id state)]
    (contains? (get-in fsm [:fsm/state :active]) state-id)))

(defn get-final-state-done-events [fsm state]
  (when (= :fsm/final (:fsm/type state))
    (let [parent-state-id                (get-in state [:fsm/parent-state :fsm/id])
          parent-state                   (get-in fsm [:fsm/index :by-id parent-state-id])
          event                          {:event {:fsm/event (get-final-event-name parent-state-id)}}
          event-for-parallel-grandparent (when (= :fsm/state (:fsm/type parent-state))
                                           (get-final-state-done-event-for-parallel-state fsm parent-state-id))]
      (if (and event event-for-parallel-grandparent)
        [event event-for-parallel-grandparent]
        [event]))))

(defn enter-history-state [fsm event state-id]
  ;;(println "ENTERING HISTORY:" state-id)
  (let [state   (get-in fsm [:fsm/index :by-id state-id])
        handler (-> state :fsm.children/transitions first :fsm/on)]
    (handler fsm event)))

(defn enter-state [fsm event state-id]
  ;;(println "ENTERING:" state-id)
  (let [state                   (get-in fsm [:fsm/index :by-id state-id])
        ;; State can be active if we're entering as a result of an targetless transition
        ;; in this case, we don't want to re-run the fsm.on/enter handler
        is-state-active         (state-active? fsm state)
        enter-handler           (if is-state-active first-identity (:fsm.on/enter state))
        pre-inbound             (get-in fsm [:fsm/session :inbound])
        fsm'                    (-> fsm
                                  (assoc-in [:fsm/state :active state-id] (= :atomic (:fsm.state/type state)))
                                  (assoc-in [:fsm/session :inbound] [])
                                  (enter-handler event))
        post-inbound            (get-in fsm' [:fsm/session :inbound])
        nil-event-transition    (get-nil-event-transition fsm' state-id)
        final-state-done-events (when-not is-state-active (get-final-state-done-events fsm' state))
        final-inbound           (vec (concat
                                       pre-inbound
                                       (when nil-event-transition [nil-event-transition])
                                       post-inbound
                                       final-state-done-events))]
    (assoc-in fsm' [:fsm/session :inbound] final-inbound)))

;; TODO: terminate fsm when final state that is a direct child of the root is entered
(defn enter-states [fsm event to-enter]
  ;;(println "TO ENTER" to-enter)
  (reduce
    (fn [acc state-id]
      (let [state-type (get-in acc [:fsm/index :by-id state-id :fsm/type])]
        (if (= :fsm/history state-type)
          (enter-history-state acc event state-id)
          (enter-state acc event state-id))))
    fsm
    to-enter))

(defn get-history [fsm state-id history-type]
  (let [state-path (get-in fsm [:fsm/index :by-id state-id :fsm/path])]
    (if (= :deep history-type)
      (let [active-atomic-states (get-active-atomic-states fsm)]
        (->> active-atomic-states
          (map #(get-in fsm [:fsm/index :by-id %]))
          (filter #(descendant-path? state-path (:fsm/path %)))
          (map :fsm/id)
          set))
      (let [active-states        (get-active-states fsm)
            state-path-length    (count state-path)
            matching-path-length (inc state-path-length)]
        (->> active-states
          (map #(get-in fsm [:fsm/index :by-id %]))
          (filter (fn [s]
                    (let [path (:fsm/path s)]
                      (and (= matching-path-length (count path))
                        (descendant-path? state-path path)))))
          (map :fsm/id)
          set)))))

(defn record-history [fsm state-id]
  (let [history-states (get-in fsm [:fsm/index :by-id state-id :fsm.children.states/history])
        history-types  (set (map :fsm.history/type history-states))
        histories      (reduce
                         (fn [acc history-type]
                           (assoc acc history-type (get-history fsm state-id history-type)))
                         {}
                         history-types)]
    (reduce
      (fn [acc history-state]
        (assoc-in acc [:fsm/state :histories (:fsm/id history-state)] (get histories (:fsm.history/type history-state))))
      fsm
      history-states)))

(defn record-histories [fsm to-exit]
  (let [state-ids-with-history (filter #(get-in fsm [:fsm/index :by-id % :fsm.children.states/history]) to-exit)]
    (if (seq state-ids-with-history)
      (reduce
        record-history
        fsm
        state-ids-with-history)
      fsm)))

(defn exit-states [fsm event to-exit]
  ;;(println "TO EXIT" to-exit)
  (let [fsm' (record-histories fsm to-exit)]
    ;;(println (:fsm/state fsm'))
    (reduce
      (fn [acc state-id]
        (let [exit-handler (get-in fsm [:fsm/index :by-id state-id :fsm.on/exit])]
          (-> acc
            (update-in [:fsm/state :active] dissoc state-id)
            (exit-handler event))))
      fsm'
      to-exit)))

(defn transition-states [fsm event]
  (let [transitions-with-domain (get-in fsm [:fsm/session :transitions])]
    (reduce
      (fn [acc t]
        ;; Targetless transitions are specific in a way that they don't cause any state to exit or to enter
        ;; but we still want to re-run transitions as if we're entering the state.
        (let [is-targetless (nil? (get-in t [:transition :fsm.transition/target]))
              to-exit       (when (not is-targetless) (select-transition-states-to-exit acc t))
              to-enter      (if is-targetless
                              (select-transition-states-for-targetless-transition acc t)
                              (select-transition-states-to-enter acc t))
              handler       (get-in t [:transition :fsm/on])]
          ;;(println "IS TARGETLESS" is-targetless)
          (-> acc
            (exit-states event to-exit)
            (handler event)
            (enter-states event to-enter)
            (update-in [:fsm/session :transitions] rest))))
      fsm
      transitions-with-domain)))

(declare run-small-step)

(defn trigger-scheduled [fsm system-time]
  (let [scheduled (get-in fsm [:fsm/state :scheduled-events])
        first-scheduled (->> scheduled (filter #(>= system-time (:at %))) first)
        rest-scheduled (if first-scheduled (remove #(= % first-scheduled) scheduled) scheduled)]
    (if first-scheduled
      (run-small-step
        (-> fsm
          (assoc-in [:fsm/state :scheduled-events] rest-scheduled)
          (assoc-in [:fsm/session :inbound] [first-scheduled]))
        system-time)
      fsm)))

(defn run-small-step [fsm system-time]
  (let [inbound (get-in fsm [:fsm/session :inbound])]
    (if (seq inbound)
      (let [{:keys [event transition]} (first inbound)
            transitions-with-domain (if (nil? event)
                                      (get-nil-event-transitions-with-domain fsm transition)
                                      (get-event-transitions-with-domain fsm event))]
        ;;(println "EVENT" event)
        (-> fsm
          (update-in [:fsm/session :inbound] #(vec (rest %)))
          (assoc-in [:fsm/state :time] system-time)
          (assoc-in [:fsm/session :transitions] transitions-with-domain)
          (transition-states event)
          (recur system-time)))
      (trigger-scheduled fsm system-time))))

(defn start
  ([fsm] (start fsm {}))
  ([fsm data]
   ;;(println "START --------------------------------------")
   (let [root-state (get-in fsm [:fsm/index :by-path []])
         to-enter   (select-self-and-descendant-states-to-enter fsm root-state)]
     (-> fsm
       (assoc :fsm/data data
              :fsm/session {:inbound [] :outbound []})
       (enter-states {:fsm/event :fsm/start} to-enter)
       (run-small-step (get-system-time))))))

(defn trigger
  ([fsm event] (trigger fsm event (get-system-time)))
  ([fsm event system-time]
   (-> fsm
     (update :fsm/session merge {:outbound [] :inbound []})
     (trigger-scheduled system-time)
     (assoc-in [:fsm/session :inbound] [{:event event}])
     (run-small-step system-time))))

(defn get-time [fsm]
  (get-in fsm [:fsm/state :time]))

(defn update-data [fsm & args]
  (apply update fsm :fsm/data args))

(defn update-in-data [fsm path & args]
  (apply update-in fsm (into [:fsm/data] (as-path path)) args))

(defn get-data [fsm]
  (:fsm/data fsm))

(defn get-in-data [fsm path]
  (get-in fsm (into [:fsm/data] (as-path path))))

(defn assoc-data [fsm data]
  (assoc fsm :fsm/data data))

(defn assoc-in-data [fsm path val]
  (assoc-in fsm (into [:fsm/data] (as-path path)) val))

(defn dissoc-data [fsm]
  (dissoc fsm :fsm/data))

(defn in-state? [fsm state-id]
  (contains? (get-active-states fsm) state-id))

(defn in-atomic-state? [fsm state-id]
  (contains? (get-active-atomic-states fsm) state-id))

(defn raise [fsm event]
  ;;(println "RAISE" event)
  (update-in fsm [:fsm/session :inbound] conj {:event event}))

(defn raise-delayed [fsm event-delay event]
  (let [time (get-time fsm)]
    (update-in fsm [:fsm/state :scheduled-events] vec-conj {:at (+ time event-delay) :event event})))

(defn send [fsm event]
  (update-in fsm [:fsm/session :outbound] vec-conj event))

(defn get-events [fsm]
  (get-in fsm [:fsm/session :outbound]))



