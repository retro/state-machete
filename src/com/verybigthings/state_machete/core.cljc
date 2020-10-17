(ns com.verybigthings.state-machete.core
  (:refer-clojure :exclude [compile send])
  (:require [com.verybigthings.state-machete.hiccup :as hiccup]
            [com.verybigthings.state-machete.util
             :refer [lexicographic-compare keyword-or-coll->set first-identity descendant-path?]]
            [clojure.set :as set]))

(def compile hiccup/compile)

(defn- get-system-time [] #?(:cljs (js/Date.now) :clj (System/currentTimeMillis)))

(defn as-path [path]
  (if (sequential? path) path [path]))

(def set-conj (fnil conj #{}))
(def set-disj (fnil disj #{}))
(def vec-conj (fnil conj []))

(defn get-active-states [fsm]
  (let [active (get-in fsm [:fsm/state :active])]
    (set/union (:atomic active) (:compound active))))

(defn get-active-atomic-states [fsm]
  (get-in fsm [:fsm/state :active :atomic]))

(defn get-nil-event-transition [fsm state-id]
  (let [fsm-index-by-id (get-in fsm [:fsm/index :by-id])
        state           (fsm-index-by-id state-id)
        transition      (reduce
                          (fn [_ t-id]
                            (let [t      (fsm-index-by-id t-id)
                                  t-cond (:fsm.transition/cond t)]
                              (when (t-cond fsm nil)
                                (reduced t))))
                          nil
                          (:fsm.transitions/nil state))]
    (when transition
      {:transition transition})))

(defn get-history-target-states [fsm state]
  (let [t-id       (-> state :fsm.children.transitions/nil first)
        transition (get-in fsm [:fsm/index :by-id t-id])]
    (or (get-in fsm [:fsm/state :histories (:fsm/id state)]) (:fsm.transition/target transition))))

(defn expand-history-states-ids [fsm state-ids]
  (let [fsm-index-by-id (get-in fsm [:fsm/index :by-id])]
    (reduce
      (fn [acc state-id]
        (let [state (fsm-index-by-id state-id)]
          (if (= :fsm/history (:fsm/type state))
            (set/union acc (get-history-target-states fsm state))
            acc)))
      state-ids
      state-ids)))

(defn select-self-and-descendant-states-to-enter
  ([fsm state] (select-self-and-descendant-states-to-enter fsm state {}))
  ([fsm state initial-ids]
   (let [fsm-index-by-id (get-in fsm [:fsm/index :by-id])]
     (loop [selected-ids []
            to-process   [state]
            seen-ids     #{}]
       (let [[state & rest-states-to-process] to-process
             id (:fsm/id state)]
         (if (contains? seen-ids id)
           (if (seq rest-states-to-process)
             (recur selected-ids rest-states-to-process seen-ids)
             selected-ids)
           (let [fsm-type            (:fsm/type state)
                 selected-ids'       (conj selected-ids id)
                 children-to-process (cond
                                       (= :fsm/parallel fsm-type)
                                       (map fsm-index-by-id (:fsm.children.states/history-excluded state))

                                       (= :fsm/history fsm-type)
                                       (map fsm-index-by-id (get-history-target-states fsm state))

                                       (= :compound (:fsm.state/type state))
                                       (let [initial-child-id (or (get initial-ids (:fsm/id state)) (:fsm/initial state))]
                                         ;; When entering history states, we might have multiple children we want to enter -> history one
                                         ;; and the normal state child. So we might receive a set of ids here
                                         (if (coll? initial-child-id)
                                           (map fsm-index-by-id initial-child-id)
                                           [(fsm-index-by-id initial-child-id)]))

                                       :else [])
                 to-process'         (concat rest-states-to-process children-to-process)]
             (if (seq to-process')
               (recur selected-ids' to-process' (conj seen-ids id))
               selected-ids'))))))))

(defn select-transition-states-to-exit [fsm {:keys [transition]}]
  (let [active-states (get-active-states fsm)
        exit-paths    (filter #(contains? active-states %) (:fsm.transition.domain/ids transition))]
    (->> exit-paths reverse seq)))

(defn select-transition-states-for-targetless-transition [fsm transition-with-domain]
  (reverse (select-transition-states-to-exit fsm transition-with-domain)))

(defn select-transition-states-to-enter [fsm {:keys [transition domain]}]
  (let [domain-id        (:fsm/id domain)
        target-state-ids (expand-history-states-ids fsm (:fsm.transition/target transition))
        fsm-index-by-id  (get-in fsm [:fsm/index :by-id])
        initial-ids      (reduce
                           (fn [acc state-id]
                             (loop [id           state-id
                                    selected-ids acc]
                               (if (or (nil? id) (= id domain-id))
                                 selected-ids
                                 (let [state (fsm-index-by-id id)
                                       parent-id (get-in state [:fsm/parent-state :fsm/id])]
                                   (recur parent-id (update selected-ids parent-id set-conj id))))))
                           {}
                           target-state-ids)
        states-to-enter  (rest (select-self-and-descendant-states-to-enter fsm domain initial-ids))]
    (seq states-to-enter)))

(defn get-event-transition-for-state [fsm event state-id]
  (let [fsm-index-by-id (get-in fsm [:fsm/index :by-id])
        event-name      (when-let [e (:fsm/event event)] (name e))
        state           (fsm-index-by-id state-id)]
    (reduce
      (fn [_ t-id]
        (let [t       (fsm-index-by-id t-id)
              t-cond  (:fsm.transition/cond t)
              t-event (:fsm.transition/event t)]
          (when (and (t-event event-name) (t-cond fsm event)) (reduced t))))
      nil
      (:fsm.transitions/event state))))

(defn get-transition-domain [fsm transition]
  (let [transition-domain-id (:fsm.transition/domain transition)]
    (get-in fsm [:fsm/index :by-id transition-domain-id])))

(defn get-event-transitions-with-domain [fsm event]
  (let [atomic-states (get-active-atomic-states fsm)
        transitions   (reduce
                        (fn [acc state-id]
                          (let [transition (get-event-transition-for-state fsm event state-id)]
                            (if transition
                              (conj acc {:transition transition
                                         :domain  (get-transition-domain fsm transition)
                                         :path (:fsm/path transition)})
                              acc)))
                        (sorted-set-by #(lexicographic-compare (:path %1) (:path %2)))
                        atomic-states)]
    transitions))

(defn get-nil-event-transitions-with-domain [fsm transition]
  (let [domain (get-transition-domain fsm transition)]
    [{:transition transition :domain domain}]))

(defn get-final-event-name [state-id]
  (let [state-id-name (name state-id)
        state-id-ns   (namespace state-id)
        event-name    (if state-id-ns
                        (str "done.state." state-id-ns "/" state-id-name)
                        (str "done.state." state-id-name))]
    (keyword event-name)))

(defn get-final-state-done-event-for-parallel-state [fsm state-id]
  (let [fsm-index-by-id         (get-in fsm [:fsm/index :by-id])
        parent-state-id         (get-in fsm-index-by-id [state-id :fsm/parent-state :fsm/id])
        parent-state            (fsm-index-by-id parent-state-id)
        parent-state-path       (:fsm/path parent-state)
        parent-state-path-count (count parent-state-path)]

    (when (= :fsm/parallel (:fsm/type parent-state))
      (let [active-states              (get-active-states fsm)
            active-final-grandchildren (filter
                                         (fn [state-id]
                                           (let [state (fsm-index-by-id state-id)
                                                 path  (:fsm/path state)]
                                             (and (= :fsm/final (:fsm/type state))
                                               (= (+ 2 parent-state-path-count) (count path))
                                               (= parent-state-path (subvec path 0 parent-state-path-count)))))
                                         active-states)]
        (when (= (count active-final-grandchildren)
                (count (:fsm.children/states parent-state)))
          {:event {:fsm/event (get-final-event-name parent-state-id)}})))))

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
  (let [t-id    (first (get-in fsm [:fsm/index :by-id state-id :fsm.children.transitions/nil]))
        handler (get-in fsm [:fsm/index :by-id t-id :fsm/on])]
    (handler fsm event)))

(defn visit-state [fsm event state-id]
  ;;(println "VISITING:" state-id)
  (let [pre-inbound          (get-in fsm [:fsm/session :inbound])
        fsm'                 (assoc-in fsm [:fsm/session :inbound] [])
        post-inbound         (get-in fsm' [:fsm/session :inbound])
        nil-event-transition (get-nil-event-transition fsm' state-id)
        final-inbound        (vec (concat
                                    pre-inbound
                                    (when nil-event-transition [nil-event-transition])
                                    post-inbound))]
    (assoc-in fsm' [:fsm/session :inbound] final-inbound)))

(defn visit-states [fsm event to-visit]
  ;;(println "TO VISIT" to-enter)
  (reduce
    (fn [acc state-id]
      (visit-state acc event state-id))
    fsm
    to-visit))

(defn enter-state [fsm event state-id]
  ;;(println "ENTERING:" state-id)
  (let [state                   (get-in fsm [:fsm/index :by-id state-id])
        is-atomic               (= :atomic (:fsm.state/type state))
        enter-handler           (:fsm.on/enter state)
        pre-inbound             (get-in fsm [:fsm/session :inbound])
        fsm'                    (-> fsm
                                  (update-in [:fsm/state :active (if is-atomic :atomic :compound)] set-conj state-id)
                                  (assoc-in [:fsm/session :inbound] [])
                                  (enter-handler event))
        post-inbound            (get-in fsm' [:fsm/session :inbound])
        nil-event-transition    (get-nil-event-transition fsm' state-id)
        final-state-done-events (get-final-state-done-events fsm' state)
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
  (let [state-path      (get-in fsm [:fsm/index :by-id state-id :fsm/path])
        fsm-index-by-id (get-in fsm [:fsm/index :by-id])]
    (if (= :deep history-type)
      (let [active-atomic-states (get-active-atomic-states fsm)]
        (->> active-atomic-states
          (map fsm-index-by-id)
          (filter #(descendant-path? state-path (:fsm/path %)))
          (map :fsm/id)
          set))
      (let [active-states        (get-active-states fsm)
            state-path-length    (count state-path)
            matching-path-length (inc state-path-length)]
        (->> active-states
          (map fsm-index-by-id)
          (filter (fn [s]
                    (let [path (:fsm/path s)]
                      (and (= matching-path-length (count path))
                        (descendant-path? state-path path)))))
          (map :fsm/id)
          set)))))

(defn record-history [fsm state-id]
  (let [fsm-index-by-id (get-in fsm [:fsm/index :by-id])
        history-states  (map fsm-index-by-id (get-in fsm-index-by-id [state-id :fsm.children.states/history]))
        history-types   (set (map :fsm.history/type history-states))
        histories       (reduce
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
  (let [fsm'            (record-histories fsm to-exit)
        fsm-index-by-id (get-in fsm [:fsm/index :by-id])]
    ;;(println (:fsm/state fsm'))
    (reduce
      (fn [acc state-id]
        (let [state        (fsm-index-by-id state-id)
              exit-handler (:fsm.on/exit state)
              is-atomic    (= :atomic (:fsm.state/type state))]
          (-> acc
            (update-in [:fsm/state :active (if is-atomic :atomic :compound)] set-disj state-id)
            (exit-handler event))))
      fsm'
      to-exit)))

(defn transition-states [fsm event]
  (let [transitions-with-domain (get-in fsm [:fsm/session :transitions])]
    (-> (reduce
          (fn [{:keys [fsm seen-ids] :as acc} t]
            ;; Targetless transitions are specific in a way that they don't cause any state to exit or to enter
            ;; but we still want to re-run nil event transitions as if we're entering the state.
            (if (nil? (get-in t [:transition :fsm.transition/target]))
              (let [to-visit (select-transition-states-for-targetless-transition fsm t)
                    handler  (get-in t [:transition :fsm/on])]
                {:fsm (-> fsm
                        (handler event)
                        (visit-states event to-visit)
                        (update-in [:fsm/session :transitions] rest))
                 :seen-ids seen-ids})
              (let [domain-ids (get-in t [:transition :fsm.transition.domain/idset])]
                (if (seq (set/intersection seen-ids domain-ids))
                  acc
                  (let [to-exit  (select-transition-states-to-exit fsm t)
                        to-enter (select-transition-states-to-enter fsm t)
                        handler  (get-in t [:transition :fsm/on])]
                    {:fsm (-> fsm
                            (exit-states event to-exit)
                            (handler event)
                            (enter-states event to-enter)
                            (update-in [:fsm/session :transitions] rest))
                     :seen-ids (set/union domain-ids)})))))
          {:fsm fsm :seen-ids #{}}
          transitions-with-domain)
      :fsm)))

(declare run-small-step)

(defn trigger-scheduled
  ([fsm] (trigger-scheduled fsm (get-system-time)))
  ([fsm system-time]
   (let [scheduled       (get-in fsm [:fsm/state :scheduled-events])
         first-scheduled (->> scheduled (filter #(>= system-time (:at %))) first)
         rest-scheduled  (if first-scheduled (remove #(= % first-scheduled) scheduled) scheduled)]
     (if first-scheduled
       (run-small-step
         (-> fsm
           (assoc-in [:fsm/state :scheduled-events] rest-scheduled)
           (assoc-in [:fsm/session :inbound] [first-scheduled]))
         system-time)
       fsm))))

(defn run-small-step [fsm system-time]
  (let [inbound (get-in fsm [:fsm/session :inbound])]
    (if (seq inbound)
      (let [{:keys [event transition]} (first inbound)
            transitions-with-domain (if (nil? event)
                                      (get-nil-event-transitions-with-domain fsm transition)
                                      (get-event-transitions-with-domain fsm event))]
        ;;(println "EVENT" event (keys (get-in fsm [:fsm/state :active])))
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



