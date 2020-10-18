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

(defn get-active-atomic-states [fsm]
  (-> fsm :fsm/state :active :atomic))

(defn get-active-compound-states [fsm]
  (-> fsm :fsm/state :active :compound))

(defn get-active-states [fsm]
  (let [active (-> fsm :fsm/state :active)]
    (set/union (:atomic active) (:compound active))))

(defn in-compound-state? [fsm state-id]
  (contains? (get-active-compound-states fsm) state-id))

(defn in-atomic-state? [fsm state-id]
  (contains? (get-active-atomic-states fsm) state-id))

(defn in-state? [fsm state-id]
  (or (in-atomic-state? fsm state-id) (in-compound-state? fsm state-id)))

(defn get-history-target-state-ids [fsm state]
  (let [t-id       (-> state :fsm.children.transitions/nil first)
        transition (get-in fsm [:fsm/index :by-id t-id])]
    (or (get-in fsm [:fsm/state :histories (:fsm/id state)]) (:fsm.transition/target transition))))

(defn get-nil-event-transition-for-state [fsm state-id]
  (let [fsm-index-by-id (-> fsm :fsm/index :by-id)
        state           (fsm-index-by-id state-id)]
    (loop [t-ids (:fsm.transitions/nil state)]
      (when (seq t-ids)
        (let [[t-id & rest-t-ids] t-ids
              t      (fsm-index-by-id t-id)
              t-cond (:fsm.transition/cond t)]
          (if (t-cond fsm nil)
            {:transition t}
            (recur rest-t-ids)))))))

(defn get-event-transition-for-state [fsm event state-id]
  (let [fsm-index-by-id (-> fsm :fsm/index :by-id)
        event-name      (when-let [e (:fsm/event event)] (name e))
        state           (fsm-index-by-id state-id)]
    (loop [t-ids (:fsm.transitions/event state)]
      (when (seq t-ids)
        (let [[t-id & rest-t-ids] t-ids
              t       (fsm-index-by-id t-id)
              t-cond  (:fsm.transition/cond t)
              t-event (:fsm.transition/event t)]
          (if (and (t-event event-name) (t-cond fsm event))
            t
            (recur rest-t-ids)))))))

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
                                         :domain (get-transition-domain fsm transition)
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
  (let [fsm-index-by-id         (-> fsm :fsm/index :by-id)
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

(defn enter-history-state [fsm event state]
  ;;(println "ENTERING HISTORY:" state-id)
  (let [t-id    (first (:fsm.children.transitions/nil state))
        handler (get-in fsm [:fsm/index :by-id t-id :fsm/on])]
    (handler fsm event)))

(defn enter-state [fsm event state]
  ;;(println "ENTERING:" state-id)
  (let [state-id                (:fsm/id state)
        state-type              (:fsm.state/type state)
        active-states           (set-conj (-> fsm :fsm/state :active state-type) state-id)
        enter-handler           (:fsm.on/enter state)
        pre-inbound             (-> fsm :fsm/session :inbound)
        fsm'                    (-> fsm
                                  (update :fsm/state #(update % :active assoc state-type active-states))
                                  (update :fsm/session assoc :inbound [])
                                  (enter-handler event))
        post-inbound            (-> fsm' :fsm/session :inbound)
        nil-event-transition    (get-nil-event-transition-for-state fsm' state-id)
        final-state-done-events (get-final-state-done-events fsm' state)
        final-inbound           (concat
                                  pre-inbound
                                  (when nil-event-transition [nil-event-transition])
                                  post-inbound
                                  final-state-done-events)]
    (update fsm' :fsm/session assoc :inbound final-inbound)))

(defn enter-states [fsm event {:keys [transition domain]}]
  (let [fsm-index             (:fsm/index fsm)
        fsm-index-by-id       (:by-id fsm-index)
        fsm-index-by-path     (:by-path fsm-index)
        id->indexed-state     #(let [s (fsm-index-by-id %)] [(:fsm/document-index s) s])
        entry-state           (or domain (fsm-index-by-path []))
        domain-id-path        (:fsm/id-path domain)
        domain-id-path-length (count domain-id-path)
        targets               (:fsm.transition/target transition)
        target-state-ids      (when domain (mapcat #(subvec (:fsm/id-path (fsm-index-by-id %)) domain-id-path-length) targets))
        initial-states        (conj (map id->indexed-state target-state-ids) [(:fsm/document-index entry-state) entry-state])]
    (loop [fsm        fsm
           to-process (apply conj (sorted-map-by <) initial-states)]
      (let [[state-document-index state] (first to-process)
            rest-states-to-process  (dissoc to-process state-document-index)
            fsm-type                (:fsm/type state)
            enter-fn                (if (= :fsm/history fsm-type) enter-history-state enter-state)
            fsm'                    (if (= state domain) fsm (enter-fn fsm event state))
            children-ids-to-process (cond
                                      ;; For parallel states, we don't want to enter history states. We enter parallel's
                                      ;; history state only if it's the target of a transition.
                                      (= :fsm/parallel fsm-type)
                                      (:fsm.children.states/history-excluded state)

                                      ;; For each history target state we need to add states between history parent and
                                      ;; history target. That way we'll ensure that we visit and enter all of them
                                      (= :fsm/history fsm-type)
                                      (let [path-length-to-drop (-> state :fsm/id-path count dec)
                                            target-state-ids    (get-history-target-state-ids fsm state)]
                                        (mapcat #(subvec (:fsm/id-path (fsm-index-by-id %)) path-length-to-drop) target-state-ids))

                                      ;; When entering compound state, we're checking if the next state's parent is this state
                                      ;; We could have some states added to the to-process map by one of the previous actions
                                      ;; (target states at the beginning or history state). If next state is not the direct
                                      ;; descendant of the current state, we add our initial state. For initial state, we need
                                      ;; to add it's id and any id between the current and initial state because we support
                                      ;; deep initial states
                                      (and (= :compound (:fsm.state/type state))
                                        (not= (:fsm/id state) (-> rest-states-to-process first last :fsm/parent-state :fsm/id)))
                                      (let [path-length-to-drop (-> state :fsm/id-path count)
                                            initial-state-id    (:fsm/initial state)]
                                        (subvec (:fsm/id-path (fsm-index-by-id initial-state-id)) path-length-to-drop))

                                      :else [])
            to-process'             (apply conj rest-states-to-process (map id->indexed-state children-ids-to-process))]
        (if (seq to-process')
          (recur fsm' to-process')
          fsm')))))

(defn visit-state [fsm event state-id]
  ;;(println "VISITING:" state-id)
  (let [pre-inbound          (get-in fsm [:fsm/session :inbound])
        fsm'                 (assoc-in fsm [:fsm/session :inbound] [])
        post-inbound         (get-in fsm' [:fsm/session :inbound])
        nil-event-transition (get-nil-event-transition-for-state fsm' state-id)
        final-inbound        (concat
                               pre-inbound
                               (when nil-event-transition [nil-event-transition])
                               post-inbound)]
    (assoc-in fsm' [:fsm/session :inbound] final-inbound)))

(defn visit-states [fsm event {:keys [transition]}]
  ;;(println "TO VISIT" to-enter)
  (reduce
    (fn [acc state-id]
      (if (in-state? acc state-id)
        (visit-state acc event state-id)
        acc))
    fsm
    (:fsm.transition.domain/ids transition)))

(defn get-history [fsm state-id history-type]
  (let [state-path      (get-in fsm [:fsm/index :by-id state-id :fsm/path])
        fsm-index-by-id (-> fsm :fsm/index :by-id)]
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

(defn record-histories [fsm state-id history-state-ids]
  (if (seq history-state-ids)
    (let [fsm-index-by-id (-> fsm :fsm/index :by-id)
          history-states  (map fsm-index-by-id history-state-ids)
          history-types   (set (map :fsm.history/type history-states))
          histories       (reduce #(assoc %1 %2 (get-history fsm state-id %2)) {} history-types)]
      (reduce
        (fn [acc history-state]
          (assoc acc (:fsm/id history-state) (get histories (:fsm.history/type history-state))))
        {}
        history-states))))

(defn exit-states [fsm event {:keys [transition]}]
  ;;(println "TO EXIT" to-exit)
  (let [fsm-index-by-id (-> fsm :fsm/index :by-id)]
    ;;(println (:fsm/state fsm'))
    (reduce
      (fn [acc state-id]
        (if (in-state? acc state-id)
          (let [state             (fsm-index-by-id state-id)
                exit-handler      (:fsm.on/exit state)
                state-type        (:fsm.state/type state)
                active-states     (set-disj (-> acc :fsm/state :active state-type) state-id)
                history-state-ids (:fsm.children.states/history state)
                ;; We need to record histories from the state _before_ we started the
                ;; exit loop. We're exiting states from the inside out, which means that by
                ;; the time we record histories, child states will already be exited
                histories         (record-histories fsm state-id history-state-ids)
                next-state        (-> acc
                                    :fsm/state
                                    (update :active assoc state-type active-states)
                                    (update :histories merge histories))]
            (-> acc
              (assoc :fsm/state next-state)
              (exit-handler event)))
          acc))
      fsm
      (reverse (:fsm.transition.domain/ids transition)))))

(defn transition-states [fsm event]
  (let [transitions-with-domain (get-in fsm [:fsm/session :transitions])]
    (-> (reduce
          (fn [{:keys [fsm seen-ids] :as acc} t]
            ;; Targetless transitions are specific in a way that they don't cause any state to exit or to enter
            ;; but we still want to re-run nil event transitions as if we're entering the state.
            (if (nil? (get-in t [:transition :fsm.transition/target]))
              (let [handler (get-in t [:transition :fsm/on])]
                {:fsm (-> fsm
                        (handler event)
                        (visit-states event t)
                        (update-in [:fsm/session :transitions] rest))
                 :seen-ids seen-ids})
              (let [domain-ids (get-in t [:transition :fsm.transition.domain/idset])]
                (if (seq (set/intersection seen-ids domain-ids))
                  acc
                  (let [handler (get-in t [:transition :fsm/on])]
                    {:fsm (-> fsm
                            (exit-states event t)
                            (handler event)
                            (enter-states event t)
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
          (update-in [:fsm/session :inbound] #(rest %))
          (assoc-in [:fsm/state :time] system-time)
          (assoc-in [:fsm/session :transitions] transitions-with-domain)
          (transition-states event)
          (recur system-time)))
      (trigger-scheduled fsm system-time))))

(defn start
  ([fsm] (start fsm {}))
  ([fsm data]
   ;;(println "START --------------------------------------")
   (-> fsm
     (assoc :fsm/data data
            :fsm/session {:inbound [] :outbound []})
     (enter-states {:fsm/event :fsm/start} nil)
     (run-small-step (get-system-time)))))

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