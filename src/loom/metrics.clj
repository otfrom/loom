(ns ^{:doc "Graph metrics"
      :author "Bruno Kim Medeiros Cesar"}
  loom.metrics
  (:use [loom.graph :only [in-degree out-degree weighted? directed? nodes edges weight predecessors successors has-edge?]]))

(defn degrees
  "Returns a map from each graph's node to its degree.
  Graph: {node <degree>}
  Digraph: {node [<in-deg> <out-deg>]}"
  [g]
  (into {} (for [node (nodes g)]
             [node (if (directed? g)
                     ((juxt in-degree out-degree) g node)
                     (out-degree g node))])))

(defn strength
  "Returns a node's strength (sum of weights).
  Graph: <strenght>
  Digraph: [<in-strength> <out-strength>]
  
  For unweighted graphs, every edge has weight 1."
  [g node]
  (if (not (weighted? g))
    (if (directed? g)
      ((juxt in-degree out-degree) g node)
      (out-degree g node))
    (if (directed? g)
      [(apply + 0 (map #(weight g % node) (predecessors g node)))
       (apply + 0 (map #(weight g node %) (successors g node)))]
      (apply + 0 (map #(weight g node %) (successors g node))))))

(defn strengths
  "Returns a map from each weighted graph's node to its strenght (sum of weights)
  Graph: {node <strength>}
  Digraph: {node [<in-strength> <out-strength>]}"
  [g]
  (into {} (for [node (nodes g)]
             [node (strength g node)])))

(defn clustering
  "Returns a map from each node to its clustering.
  
  The clustering of the i-th node is the ratio N_i/(k_i 2), where 
    k_i is the number of neighbors to i, 
    N_i is the number of edges between i's neighbors and 
    (k_i 2) = k_i * (k_i - 1)/2 is the number of pairs of k_i elements.
  
  Clustering is only defined for undirected graphs."
  [g]
  (when (directed? g) (throw (IllegalArgumentException. "Graph is directed")))
  (zipmap (nodes g)
          (for [[node deg] (degrees g)]
            (if (<= deg 1) 
              0
              (let [neighbors (successors g node)
                    num-edges*2 (->> (for [n1 neighbors, n2 neighbors] [n1 n2]) ; we walk each edge twice:
                                  (filter (fn [[n1 n2]] (has-edge? g n1 n2)))   ; both [n1 n2] and [n2 n1]
                                  count)]
                (/ num-edges*2 (* deg (- deg 1)))))))) ; so here we don't need to multiply by 2

(defn transitivity
  "Transitivity measures the ratio of number of triangles to the number of possible triangles.
  
  Transitivity is only defined for undirected graphs."
  [g]
  (when (directed? g) (throw (IllegalArgumentException. "Graph is directed")))
  (let [closed? (for [node (nodes g)
                      n1 (successors g node)
                      n2 (successors g node)]
                 (has-edge? g n1 n2))]
    (/ (count (filter identity closed?))
       (count closed?))))
