;; Dai's Benchmark accelerate
(interpolant
 (and
  (< vc (/ 4961 100))
  )
 (and
  (= fa (* (/ 5418 10000) vc vc))
  (= fr (- 1000 fa))
  (= ac (* (/ 5 10000) fr))
  (= vc1 (+ vc ac))
  (>= vc (/ 4961 100))
  )
 2)
