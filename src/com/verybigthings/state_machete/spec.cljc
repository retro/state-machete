(ns com.verybigthings.state-machete.spec
  (:require [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn >def | ? =>]]
            [clojure.string :as str]))

(def void-elements
  #{:fsm/transition :fsm/history :fsm/final})

(def non-void-elements
  #{:fsm/root :fsm/state :fsm/parallel})

(defn valid-element? [valid-elements el]
  (let [el-ns   (namespace el)
        el-name (-> el name (str/split #"#") first)]
    (contains? valid-elements (keyword el-ns el-name))))

(>def ::params (s/map-of keyword? any?))

(>def ::non-void-element
  (s/cat
    :name (s/and keyword? (partial valid-element? non-void-elements))
    :args (s/? ::params)
    :body (s/* ::node)))

(>def ::void-element
  (s/cat
    :name (s/and keyword? (partial valid-element? void-elements))
    :args (s/? ::params)))

(>def ::node
  (s/or
    :void-element ::void-element
    :non-void-element ::non-void-element))

(>def ::fsm
  (s/cat
    :name (s/and keyword? (partial valid-element? #{:fsm/root}))
    :args (s/? ::params)
    :body (s/* ::node)))

(comment
  )