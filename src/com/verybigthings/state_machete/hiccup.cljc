(ns com.verybigthings.state-machete.hiccup
  (:refer-clojure :exclude [compile])
  (:require [com.verybigthings.state-machete.spec :as sm-spec]
            [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn >def | ? =>]]
            [clojure.string :as str]))

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

(>def :fsm/on
  keyword?)

(>def :fsm.on/enter
  keyword?)

(>def :fsm.on/exit
  keyword?)

(>def :fsm.on/initial
  keyword?)

(>def :fsm/id
  any?)

(>def :fsm.transition/cond
  keyword?)

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

(defn keyword-or-coll->set [v]
  (if (keyword? v) #{v} (set v)))

(defn get-node-name-and-id [node-name-id]
  (let [ns (namespace node-name-id)
        [node-name id] (-> node-name-id name (str/split #"#"))
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

(defn process-transition [attrs context]
  (-> attrs
    (update :fsm.transition/event #(when (and % (not= :* %)) (keyword-or-coll->set %)))
    (update :fsm.transition/target #(when % (keyword-or-coll->set %)))
    (update :fsm.transition/type #(or % :external))
    (update :fsm.transition/cond #(get-in context [:fsm/guards %] (constantly true)))))

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
        child-context     (cond-> context
                            (contains? state-nodes node-name)
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

      (and (= :fsm/state node-name) (not (seq child-states)))
      (assoc :fsm.state/type :atomic)

      (and (= :fsm/transition node-name))
      (process-transition context)

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