(ns com.verybigthings.state-machete.hiccup
  (:refer-clojure :exclude [compile])
  (:require [com.verybigthings.state-machete.util :refer [keyword-or-coll->set]]
            [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn >def | ? =>]]
            [clojure.string :as str]
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
    :body (s/? ::transition-node)))

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
    :parallel ::parallel-node))

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
      (first child-state-ids))))

(defn default-handler [fsm & _] fsm)

(defn process-handler [handler context]
  (if (keyword? handler)
    (let [handler' (get-in context [:fsm/handlers handler])]
      (assert handler' (str "Handler named " handler " doesn't exist"))
      handler')
    (or handler default-handler)))

(defn drop-last-wildcard [event-name-parts]
  (if (= "*" (last event-name-parts))
    (butlast event-name-parts)
    event-name-parts))

(defn process-transition-event [event]
  (when event
    (let [event-set (keyword-or-coll->set event)]
      (->> event-set
        (map
          (fn [e]
            (if (= :* e)
              {:length -1 :matcher (constantly true)}
              (let [e-name  (name e)
                    e-parts (-> e-name (str/split split-at-dot-re) drop-last-wildcard)
                    regex   (-> (concat
                                  [:cat :start]
                                  (->> e-parts
                                    (map (fn [p] (if (= "*" p) [:+ :any] p)))
                                    (interpose "."))
                                  [[:* "." [:+ :any]]
                                   :end])
                              regal/regex)]
                {:length (count e-parts) :matcher #(and % (re-matches regex %))}))))
        (sort-by :length)
        (map :matcher)))))

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

(defn process-transition-type [transition-type is-targetless]
  (if is-targetless
    :internal
    (or transition-type :external)))

(defn process-transition [attrs context]
  (let [is-targetless (nil? (:fsm.transition/target attrs))]
    (-> attrs
      (update :fsm.transition/event process-transition-event)
      (update :fsm.transition/target #(when % (keyword-or-coll->set %)))
      (update :fsm.transition/type #(or % :external)                         ;;process-transition-type is-targetless
        )
      (update :fsm.transition/cond process-transition-cond context)
      (update :fsm/on process-handler context))))

(defn process-state-handlers [attrs context]
  (-> attrs
    (update :fsm.on/enter process-handler context)
    (update :fsm.on/exit process-handler context)))

(defn expand-partials [child-nodes]
  (mapcat
    (fn [node]
      (if (= :<> (first node))
        (rest node)
        [node]))
    child-nodes))

(defn expand-node [context [node-name-id & children]]
  (let [[node-name inline-id] (get-node-name-and-id node-name-id)
        [first-child & rest-children] children
        attrs             (if (map? first-child) first-child {})
        id                (or (:fsm/id attrs) inline-id (keyword (gensym "fsm/id-")))
        path              (get-in context [:fsm/cursor :path])
        is-state-node     (contains? state-nodes node-name)
        child-context     (cond-> context
                            is-state-node
                            (assoc-in [:fsm/cursor :parent-state] {:fsm/id id :fsm/path path}))
        child-nodes       (->> (if (map? first-child) rest-children children)
                            expand-partials
                            (map-indexed (fn [idx s]
                                           (expand-node (update-in child-context [:fsm/cursor :path] conj idx) s)))
                            vec)
        child-states      (filterv #(contains? state-nodes (:fsm/type %)) child-nodes)
        child-transitions (filterv #(= :fsm/transition (:fsm/type %)) child-nodes)]

    (cond->
      (merge
        attrs
        {:fsm/type node-name
         :fsm/id id
         :fsm/parent-state (get-in context [:fsm/cursor :parent-state])
         :fsm/path (get-in context [:fsm/cursor :path])})

      (and (= :fsm/state node-name) (seq child-states))
      (assoc :fsm.state/type :compound
             :fsm/initial (get-initial-child-state-id attrs child-states))

      (or (and (= :fsm/state node-name) (not (seq child-states)))
        (= :fsm/final node-name))
      (assoc :fsm.state/type :atomic)

      (and (= :fsm/transition node-name))
      (process-transition context)

      is-state-node
      (process-state-handlers context)

      (seq child-nodes)
      (assoc :fsm/children child-nodes)

      (seq child-states)
      (assoc :fsm.children/states child-states)

      (seq child-transitions)
      (assoc :fsm.children/transitions child-transitions))))

(defn build-index
  ([node] (build-index {} node))
  ([index node]
   (reduce
     (fn [acc s]
       (build-index acc s))
     (-> index
       (assoc-in [:by-path (:fsm/path node)] node)
       (assoc-in [:by-id (:fsm/id node)] node))
     (:fsm.children/states node))))

(>defn compile
  ([node]
   [::root-node => map?]
   (compile node {}))
  ([node context]
   [::root-node map? => map?]
   (let [expanded (expand-node (assoc context :fsm/cursor {:path []}) node)]
     {:fsm/tree expanded
      :fsm/index (build-index expanded)
      :fsm/state {}})))

(comment
  (clojure.pprint/pprint
    (compile [:fsm/root {:fsm/initial :a}
              [:fsm/state {:fsm/id :a}
               [:fsm/transition {:fsm.transition/target :b :fsm.transition/event :t}]]
              [:fsm/state {:fsm/id :b}]]
      {})))