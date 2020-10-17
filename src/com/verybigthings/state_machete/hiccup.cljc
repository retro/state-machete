(ns com.verybigthings.state-machete.hiccup
  (:refer-clojure :exclude [compile])
  (:require [com.verybigthings.state-machete.util
             :refer [keyword-or-coll->set first-identity descendant-path? lexicographic-compare]]
            [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn >def | ? =>]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lambdaisland.regal :as regal]))

(defn make-valid-node-name? [base-node-kw]
  (let [base-node-ns (namespace base-node-kw)
        base-node-name (name base-node-kw)]
    (fn [node-kw]
      (let [node-ns (namespace node-kw)
            node-name (-> (name node-kw) (str/split #"#") first)]
        (and
          (= base-node-ns node-ns)
          (= base-node-name node-name))))))

(def state-nodes #{:fsm/parallel :fsm/state :fsm/history :fsm/final})

(>def :fsm/on-handler
  (s/or
    :keyword keyword?
    :fn fn?))

(>def :fsm/on :fsm/on-handler)

(>def :fsm.on/enter :fsm/on-handler)

(>def :fsm.on/exit :fsm/on-handler)

(>def :fsm.on/initial :fsm/on-handler)

(>def :fsm/id
  keyword?)

(>def :fsm.transition/cond
  (s/or
    :keyword keyword?
    :boolean boolean?
    :function fn?))

(>def :fsm.transition.event/multiple
  (s/coll-of keyword?))

(>def :fsm.transition/event
  (s/or
    :keyword keyword?
    :multiple :fsm.transition.event/multiple))

(>def :fsm.transition.target/multiple
  (s/coll-of keyword?))

(>def :fsm.transition/target
  (s/or
    :keyword keyword?
    :multiple :fsm.transition.target/multiple))

(>def :fsm/initial
  keyword?)

(>def :fsm.transition/type
  #{:internal :external})

(>def ::transition-node
  (s/cat
    :name (make-valid-node-name? :fsm/transition)
    :params ::transition-params))

(>def ::transition-params
  (s/and
    map?
    (s/keys :opt [:fsm.transition/event :fsm.transition/cond :fsm.transition/target :fsm.transition/type :fsm/on])))

(>def :fsm.history/type
  #{:deep :shallow})

(>def ::history-node
  (s/cat
    :name (make-valid-node-name? :fsm/history)
    :params (s/? ::history-params)
    :body (s/? (s/or :transition ::transition-node))
    ))

(>def ::history-params
  (s/and
    map?
    (s/keys :opt [:fsm/id :fsm.history/type])))

(>def ::final-node
  (s/cat
    :name (make-valid-node-name? :fsm/final)
    :params (s/? ::final-params)))

(>def ::final-params
  (s/and
    map?
    (s/keys :opt [:fsm/id :fsm.on/enter :fsm.on/exit])))

(defn root-node? [v]
  (= v :fsm/root))

(>def ::root-node
  (s/and
    vector?
    (s/cat
      :name root-node?
      :params (s/? ::root-params)
      :body (s/+ ::node))))

(>def ::root-params
  (s/and
    map?
    (s/keys :opt [:fsm/id :fsm/initial])))

(>def ::state-node
  (s/cat
    :name (make-valid-node-name? :fsm/state)
    :params (s/? ::state-params)
    :body (s/* ::node)))

(>def ::partial-node
  (s/cat
    :name #(= :<> %)
    :body (s/* ::node)))

(>def ::state-params
  (s/and
    map?))

(>def ::parallel-node
  (s/cat
    :name (make-valid-node-name? :fsm/parallel)
    :params (s/? ::parallel-params)
    :body (s/+ ::node)))

(>def ::parallel-params
  (s/and
    map?))

(>def ::node
  (s/or
    :transition ::transition-node
    :history ::history-node
    :final ::final-node
    :state ::state-node
    :parallel ::parallel-node
    :partial ::partial-node))

(>def ::expanded-node
  map?)

(def split-at-dot-re (regal/regex [:cat "."]))
(def split-at-hash-re (regal/regex [:cat "#"]))

(defn get-node-name-and-id [node-name-id]
  (let [ns (namespace node-name-id)
        [node-name id] (-> node-name-id name (str/split split-at-hash-re))
        node-name-kw (keyword ns node-name)]
    (if (= :fsm/root node-name-kw)
      [:fsm/state :fsm/root]
      [node-name-kw (keyword id)])))

(defn get-initial-child-state-id [attrs child-states]
  (let [child-state-ids (mapv :fsm/id child-states)
        initial-id (:fsm/initial attrs)]
    (if initial-id
      (do
        (assert (contains? (set child-state-ids) initial-id) (str "Child state with id:" initial-id " doesn't exist"))
        initial-id)
      (first (remove #(= :fsm/final (:fsm/type %)) child-state-ids)))))


(defn process-handler [handler context]
  (if (keyword? handler)
    (let [handler' (get-in context [:fsm/handlers handler])]
      (assert handler' (str "Handler named " handler " doesn't exist"))
      handler')
    (or handler first-identity)))

(defn drop-last-wildcard [event-name-parts]
  (if (= "*" (last event-name-parts))
    (butlast event-name-parts)
    event-name-parts))

(defn make-event-name-matcher [matchers]
  (let [matchings         (map :matching matchers)
        matching-partials (map :matching-partial matchers)]
    (memoize
      (fn [e-name]
        (or
          (reduce
            (fn [_ m]
              (when (= m e-name) (reduced true)))
            nil
            matchings)
          (reduce
            (fn [_ m]
              (when (str/starts-with? e-name m) (reduced true)))
            nil
            matching-partials))))))

(defn process-transition-event [event]
  (when event
    (let [event-set (keyword-or-coll->set event)]
      (->> event-set
        (map
          (fn [e]
            (if (= :* e)
              {:length -1 :matching-partial ""}
              (let [e-name  (name e)
                    e-parts (-> e-name (str/split split-at-dot-re) drop-last-wildcard)
                    matching-e-name (str/join "." e-parts)
                    matching-e-name-partial (str matching-e-name ".")]
                {:length (count e-parts)
                 :matching matching-e-name
                 :matching-partial matching-e-name-partial}))))
        (sort-by :length)
        (make-event-name-matcher)))))

(defn make-transition-guard [guards]
  (fn [& args]
    (reduce
      (fn [acc guard]
        (if (apply guard args)
          acc
          (reduced false)))
      true
      guards)))

(defn process-transition-cond [t-cond context]
  (if (nil? t-cond)
    (constantly true)
    (let [t-cond-coll (if (coll? t-cond) t-cond [t-cond])]
      (->> t-cond-coll
        (map
          (fn [t-cond]
            (cond
              (boolean? t-cond)
              (constantly t-cond)

              (fn? t-cond)
              t-cond

              (keyword? t-cond)
              (let [guard (get-in context [:fsm/guards t-cond])]
                (assert guard (str "Guard " t-cond " is not provided"))
                guard))))
        make-transition-guard))))

(defn process-transition [attrs context]
  (-> attrs
    (update :fsm.transition/event process-transition-event)
    (update :fsm.transition/target #(when % (keyword-or-coll->set %)))
    (update :fsm.transition/type #(or % :external))
    (update :fsm.transition/cond process-transition-cond context)
    (update :fsm/on process-handler context)))

(defn process-state-handlers [attrs context]
  (-> attrs
    (update :fsm.on/enter process-handler context)
    (update :fsm.on/exit process-handler context)))


(defn process-history-type [attrs]
  (update attrs :fsm.history/type #(or % :shallow)))

(defn expand-partials [child-nodes]
  (mapcat
    (fn [node]
      (if (= :<> (first node))
        (expand-partials (rest node))
        [node]))
    child-nodes))

(defn expand-node [context [node-name-id & children]]
  (let [[node-name inline-id] (get-node-name-and-id node-name-id)
        [first-child & rest-children] children
        attrs             (if (map? first-child) first-child {})
        id                (or (:fsm/id attrs) inline-id (keyword (gensym "fsm/id-")))
        path              (get-in context [:fsm/cursor :path])
        is-state-node     (contains? state-nodes node-name)
        child-context     (cond-> (update-in context [:fsm/cursor :id-path] conj id)
                            is-state-node
                            (assoc-in [:fsm/cursor :parent-state] {:fsm/id id :fsm/path path}))
        child-nodes       (->> (if (map? first-child) rest-children children)
                            expand-partials
                            (map-indexed (fn [idx s]
                                           (expand-node (update-in child-context [:fsm/cursor :path] conj idx) s)))
                            vec)
        child-states      (filterv #(contains? state-nodes (:fsm/type %)) child-nodes)
        child-transitions (filterv #(= :fsm/transition (:fsm/type %)) child-nodes)
        history-states    (filterv #(= :fsm/history (:fsm/type %)) child-states)

        event-transitions (filter :fsm.transition/event child-transitions)
        nil-transitions   (remove :fsm.transition/event child-transitions)]

    (cond->
      (merge
        attrs
        {:fsm/type node-name
         :fsm/id id
         :fsm/parent-state (get-in context [:fsm/cursor :parent-state])
         :fsm/id-path (conj (get-in context [:fsm/cursor :id-path]) id)
         :fsm/path (get-in context [:fsm/cursor :path])})

      (and (= :fsm/state node-name) (seq child-states))
      (assoc :fsm.state/type :compound
             :fsm/initial (get-initial-child-state-id attrs child-states))

      (or (and (= :fsm/state node-name) (not (seq child-states)))
        (= :fsm/final node-name))
      (assoc :fsm.state/type :atomic)

      (= :fsm/history node-name)
      process-history-type

      (and (= :fsm/transition node-name))
      (process-transition context)

      is-state-node
      (process-state-handlers context)

      (seq child-nodes)
      (assoc :fsm/children child-nodes)

      (seq child-states)
      (as-> attrs'
        (-> attrs'
          (assoc :fsm.children.states/history-excluded (map :fsm/id (remove #(= :fsm/history (:fsm/type %)) child-states)))
          (assoc :fsm.children/states (map :fsm/id child-states))))

      (seq history-states)
      (assoc :fsm.children.states/history (map :fsm/id history-states))

      (seq event-transitions)
      (assoc :fsm.children/transitions (map :fsm/id event-transitions))

      (seq nil-transitions)
      (assoc :fsm.children.transitions/nil (map :fsm/id nil-transitions)))))

(defn get-transition-parent-state [index transition]
  (let [fsm-index-by-id (get-in index [:by-id])
        transition-state-id (get-in transition [:fsm/parent-state :fsm/id])
        transition-state    (fsm-index-by-id transition-state-id)
        starting-state-id   (if (= :internal (:fsm.transition/type transition))
                              transition-state-id
                              (get-in transition-state [:fsm/parent-state :fsm/id]))]
    (loop [state-id starting-state-id]
      (let [state (fsm-index-by-id state-id)]
        (when state
          (if (= :compound (:fsm.state/type state))
            state
            (recur (get-in state [:fsm/parent-state :fsm/id]))))))))

(defn get-transition-domain [index transition]
  (let [fsm-index-by-path            (get-in index [:by-path])
        fsm-index-by-id              (get-in index [:by-id])
        transition-parent-state      (get-transition-parent-state index transition)
        transition-parent-state-path (:fsm/path transition-parent-state)
        transition-target-ids        (:fsm.transition/target transition)
        transition-targets-paths     (remove nil? (map #(get-in fsm-index-by-id [% :fsm/path]) transition-target-ids))
        common-ancestor-path         (if (seq transition-targets-paths)
                                       (loop [p transition-parent-state-path
                                              i 0]
                                         (let [comparing-path (or (subvec p 0 i) [])]
                                           (cond
                                             (not (every? #(descendant-path? comparing-path %) transition-targets-paths))
                                             (drop-last comparing-path)

                                             (= p comparing-path)
                                             comparing-path

                                             :else
                                             (recur p (inc i)))))
                                       transition-parent-state-path)]
    (loop [p common-ancestor-path]
      (when-let [state (fsm-index-by-path p)]
        (if (= :compound (:fsm.state/type state))
          state
          (recur (get-in state [:fsm/parent-state :fsm/path])))))))

(defn build-index
  ([node] (build-index node {}))
  ([node index]
   (let [children (:fsm/children node)
         node'    (dissoc node :fsm/children)]
     (reduce
       (fn [acc n]
         (build-index n acc))
       (-> index
         (assoc-in [:by-path (:fsm/path node')] node')
         (assoc-in [:by-id (:fsm/id node')] node'))
       children))))

(defn visit-nodes [index context]
  (let [sorted-nodes (->> (:by-path index)
                       (sort #(lexicographic-compare (first %1) (first %2)))
                       (map last))]
    (reduce
      (fn [index' node]
        (let [path (:fsm/path node)
              id (:fsm/id node)
              node-type (:fsm/type node)
              node' (reduce
                      (fn [node' v]
                        (v node' index'))
                      node
                      (get-in context [:visitors node-type]))]
          (-> index'
            (assoc-in [:by-path path] node')
            (assoc-in [:by-id id] node'))))
      index
      sorted-nodes)))

(defn get-ids-in-domain [index transition-domain]
  (let [path (:fsm/path transition-domain)]
    (->> (:by-path index)
      (filter
        (fn [[s-path state]]
          (and (contains? state-nodes (:fsm/type state))
            (descendant-path? path s-path))))
      (map last)
      (sort #(lexicographic-compare (:fsm/path %1) (:fsm/path %2)))
      (map :fsm/id))))

(defn calculate-transition-domain-visitor [node index]
  (let [transition-domain (get-transition-domain index node)
        ids-in-domain (get-ids-in-domain index transition-domain)]
    (-> node
      (assoc :fsm.transition.domain/ids ids-in-domain)
      (assoc :fsm.transition.domain/idset (set ids-in-domain))
      (assoc :fsm.transition/domain (:fsm/id transition-domain)))))

(defn calculate-transitions-for-state-visitor [state index]
  (let [parent-state-id (get-in state [:fsm/parent-state :fsm/id])]
    (if parent-state-id
      (let [parent-state (get-in index [:by-id parent-state-id])]
        (-> state
          (assoc :fsm.transitions/event (concat (:fsm.children/transitions state) (:fsm.transitions/event parent-state)))
          (assoc :fsm.transitions/nil (concat (:fsm.children.transitions/nil state) (:fsm.transitions/nil parent-state)))))
      (-> state
        (assoc :fsm.transitions/event (:fsm.children/transitions state))
        (assoc :fsm.transitions/nil (:fsm.children.transitions/nil state))))))

(>defn compile
  ([node]
   [::root-node => map?]
   (compile node {}))
  ([node context]
   [::root-node map? => map?]
   (let [expanded (expand-node (assoc context :fsm/cursor {:path [] :id-path []}) node)
         context' (-> context
                    (update-in [:visitors :fsm/state] conj calculate-transitions-for-state-visitor)
                    (update-in [:visitors :fsm/parallel] conj calculate-transitions-for-state-visitor)
                    (update-in [:visitors :fsm/final] conj calculate-transitions-for-state-visitor)
                    (update-in [:visitors :fsm/transition] conj calculate-transition-domain-visitor))]
     {:fsm/index (visit-nodes (build-index expanded) context')
      :fsm/state {}})))