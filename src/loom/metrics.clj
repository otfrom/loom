(ns ^{:doc "Graph metrics"
      :author "Bruno Kim Medeiros Cesar"}
  loom.metrics
  (:use [loom.graph :only [in-degree out-degree weighted? directed? nodes edges weight successors]]))

(defn degrees
  "Returns a map from each graph's node to its degree.
  Graph: {node <degree>}
  Digraph: {node [<in-deg> <out-deg>]}"
  [g]
  (into {} (for [node (nodes g)]
             [node (if (directed? g)
                     ((juxt in-degree out-degree) g node)
                     (out-degree g node))])))

(defn strengths
  "Returns a map from each weighted graph's node to its strenght (sum of weights)
  Graph: {node <strength>}
  Digraph: {node [<in-strength> <out-strength>]}"
  [g]
  (when (not (weighted? g)) (throw (IllegalArgumentException. "Graph is not weighted")))
  (if (directed? g)
    (reduce (fn [strengths [u v]]
              (let [w (weight g u v)]
                (-> strengths
                  (update-in [u 1] + w)
                  (update-in [v 0] + w))))
            (into {} (map #(vector % [0 0]) (nodes g)))
            (edges g))
    (reduce (fn [strengths [u v]]
              (let [w (weight g u v)]
                (update-in strengths [u] + w)))
            (into {} (map #(vector % 0) (nodes g)))
            (for [u (nodes g), v (successors g u)] [u v]))))
