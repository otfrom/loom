(ns loom.test.metrics
  (:use [loom graph metrics] :reload)
  (:use [clojure.test]))

;; (g1)      (g2)     (g3)     (g4)
;;                      1       1/2
;;  a -- b   a<-->b   a -- b   a<-->b
;;  | \  |   |`\  |   | \  |   |`\1 |
;;  |  \ |   v  \.v  3| 2\ |2 3v 2\.v2
;;  c    d   c    d   c    d   c    d

(deftest test-degrees
  (let [g1 (graph [:a :b] [:a :c] [:a :d] [:b :d])
        g2 (digraph [:a :b] [:a :c] [:a :d] [:b :a] [:b :d] [:d :a])
        g3 (weighted-graph [:a :b 1] [:a :c 3] [:a :d 2] [:b :d 2])
        g4 (weighted-digraph [:a :b 1] [:a :c 3] [:a :d 2] [:b :a 2] [:b :d 2] [:d :a 1])] 
    (testing "Degree in common graphs"
      (are [expected got] (= expected got)
        {:a 3, :b 2, :c 1, :d 2}                 (degrees g1)
        {:a [2 3], :b [1 2], :c [1 0], :d [2 1]} (degrees g2)
        {:a 3, :b 2, :c 1, :d 2}                 (degrees g3)
        {:a [2 3], :b [1 2], :c [1 0], :d [2 1]} (degrees g4)))
    (testing "Degree in edge cases"
      (are [expected got] (= expected got)
        {}         (degrees (graph))
        {:a 0}     (degrees (-> (graph) (add-nodes :a)))
        {:a [0 0]} (degrees (-> (digraph) (add-nodes :a)))))
    (testing "Strength in common graphs"
      (are [expected got] (= expected got)
        {:a 6, :b 3, :c 3, :d 4}                 (strengths g3)
        {:a [3 6], :b [1 4], :c [3 0], :d [4 1]} (strengths g4)))
    (testing "Strength in edge cases"
      (are [expected got] (= expected got)
        {:a 0}     (strengths (-> (weighted-graph) (add-nodes :a)))
        {:a [0 0]} (strengths (-> (weighted-digraph) (add-nodes :a)))))))

(deftest test-clustering
  (let [g (add-nodes (graph [:a :b] [:a :d] [:a :e] [:a :g] 
                             [:b :c] [:b :d] [:b :e] [:b :g]
                             [:c :d] [:d :e] [:e :f] [:f :g] [:g :h])
                     :i)]
    (testing "Clustering coefficient"
      (are [expected got] (= expected got)
        {:a 4/6 :b 5/10 :c 1/1 :d 4/6 :e 3/6 :f 0/1 :g 1/6 :h 0 :i 0} (clustering g)))))
