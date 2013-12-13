(ns loom.test.metrics
  (:use [loom graph metrics] :reload)
  (:use [clojure.test]))

;; (g1)      (g2)     (g3)     (g4)
;;                      1       1/2
;;  a -- b   a<-->b   a -- b   a<-->b
;;  | \  |   |`\  |   | \  |   |`\1 |
;;  |  \ |   v  \.v  3| 2\ |2 3v 2\.v2
;;  c    d   c    d   c    d   c    d

(let [g1 (graph [:a :b] [:a :c] [:a :d] [:b :d])
      g2 (digraph [:a :b] [:a :c] [:a :d] [:b :a] [:b :d] [:d :a])
      g3 (weighted-graph [:a :b 1] [:a :c 3] [:a :d 2] [:b :d 2])
      g4 (weighted-digraph [:a :b 1] [:a :c 3] [:a :d 2] [:b :a 2] [:b :d 2] [:d :a 1])] 
  (deftest test-strength
    (testing "Strength in common graphs"
      (are [expected got] (= expected got)
        6 (strength g3 :a)
        3 (strength g3 :b)
        3 (strength g3 :c)
        4 (strength g3 :d)
        [3 6] (strength g4 :a)
        [1 4] (strength g4 :b)
        [3 0] (strength g4 :c)
        [4 1] (strength g4 :d)))
    (testing "Strength in edge cases"
      (are [expected got] (= expected got)
        0 (strength (-> (weighted-graph) (add-nodes :a)) :a)
        [0 0] (strength (-> (weighted-digraph) (add-nodes :a)) :a))))
  (deftest test-degrees
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
    (testing "Strengths in common graphs"
      (are [expected got] (= expected got)
        {:a 6, :b 3, :c 3, :d 4}                 (strengths g3)
        {:a [3 6], :b [1 4], :c [3 0], :d [4 1]} (strengths g4)))
    (testing "Strengths in edge cases"
      (are [expected got] (= expected got)
        {:a 0}     (strengths (-> (weighted-graph) (add-nodes :a)))
        {:a [0 0]} (strengths (-> (weighted-digraph) (add-nodes :a)))))))

(deftest test-clustering
  (let [g (-> (graph [:a :b] [:a :d] [:a :e] [:a :g] 
                     [:b :c] [:b :d] [:b :e] [:b :g]
                     [:c :d] [:d :e] [:e :f] [:f :g] [:g :h])
            (add-nodes :i))]
    (testing "Clustering coefficient"
      (is (= {:a 4/6 :b 5/10 :c 1/1 :d 4/6 :e 3/6 :f 0/1 :g 1/6 :h 0 :i 0} (clustering g))))
    (testing "Transitivity"
      (is (= 36/72 (transitivity g))))))

(deftest test-distances
  (let [g (-> (graph [:a :b] [:b :c]
                     [:d :e] [:e :f] [:e :g] [:f :g]
                     [:h :i])
            (add-nodes :j))
        wg (-> (weighted-graph [:a :b 1] [:b :c 2]
                               [:d :e 2] [:e :f 3] [:e :g 1] [:f :g 1]
                               [:h :i 2])
            (add-nodes :j))]
    (testing "Distance histogram in simple graph"
      (are [expected got] (= expected got) 
        {0 3, 1 4, 2 2} (distances g [:a :b :c])
        {0 4, 1 8, 2 4} (distances g [:d :e :f :g])
        {0 2, 1 2}      (distances g [:h :i])
        {0 1}           (distances g [:j])
        {0 7, 1 12, 2 6, Double/POSITIVE_INFINITY 24} (distances g [:a :b :c :d :e :f :g])))
    (testing "Distance histogram in weighted graph"
      (are [expected got] (= expected got)
        {0 3, 1 2, 2 2, 3 2}      (distances wg [:a :b :c])
        {0 4, 1 4, 2 4, 3 2, 4 2} (distances wg [:d :e :f :g])))))
