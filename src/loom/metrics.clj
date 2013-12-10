(ns ^{:doc "Graph metrics"
      :author "Bruno Kim Medeiros Cesar"}
  loom.metrics
  (:use [loom.graph :only [in-degree out-degree weighted? directed? nodes edges weight successors has-edge?]]))

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

(defn clustering
  "Returns a map from each node to its clustering.
  
  The clustering of the i-th node is the ratio N_i/(k_i 2), where k_i is the number of neighbors to k_i, N_i is the number of edges between this node's neighbors and (k_i 2) = k_i * (k_i - 1)/2 is the number of pairs of k_i elements.
  
  Clustering is only defined for undirected graphs."
  [g]
  (when (directed? g) (throw (IllegalArgumentException. "Graph is directed")))
  (let [num-edges (fn [node]
                    (let [neighbors (successors g node)
                          deg (count neighbors)]
                      (reduce (fn [s [n1 n2]]
                                (if (has-edge? g n1 n2)
                                  (+ s 1)
                                  s))
                              0
                              (for [n1 neighbors
                                    n2 neighbors]
                                [n1 n2]))))]
    (zipmap (nodes g)
            (map (fn [[node deg]]
                   (if (<= deg 1) 
                     0
                     (/ (num-edges node) (* deg (- deg 1)))))
                 (degrees g)))))
