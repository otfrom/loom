(ns ^{:doc "Graph metrics"
      :author "Bruno Kim Medeiros Cesar"}
  loom.metrics
  (:use [loom.graph 
         :only [nodes edges weighted? directed? predecessors successors 
                in-degree out-degree weight has-edge?]])
  (:require [loom.alg :as alg]))

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
  Graph: <strength>
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
                      n2 (successors g node)
                      :when (not= n1 n2)]
                  (has-edge? g n1 n2))]
    (/ (count (filter identity closed?))
       (count closed?))))

(defn distances
  "Returns histogram of geodesic distances in component, given as a sequence of vertices.
   If component is not given, uses all nodes in graph.
   If the component is not connected, returns number of unreachable paths in Double/POSITIVE_INFINITY."
  ([g]
     (distances g (nodes g)))
  ([g comp]
     (let [hist (frequencies
                  (for [u comp, v comp] ; TODO calculate over the spanning tree of each vertex
                    (cond
                      (= u v) 0
                      (weighted? g) (second (alg/dijkstra-path-dist g u v))
                      :else         (when-let [path (alg/bf-path g u v)]
                                      (dec (count path))))))
           num-unreachable (get hist nil)]
       (if num-unreachable
         (-> hist (dissoc nil) (assoc Double/POSITIVE_INFINITY num-unreachable))
         hist))))
