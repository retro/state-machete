(defproject com.verybigthings/state-machete "0.0.1-alpha1"
  :description "Functional statecharts implementation for Clojure and ClojureScript."
  :url "https://github.com/keechma/keechma-next"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :source-paths ["src"]
  :dependencies [lambdaisland/regal {:mvn/version "0.0.89"}
                 [com.fulcrologic/guardrails "0.0.12"]])