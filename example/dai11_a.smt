;; Dai's Benchmark Subproblem 1 of ex1
(interpolant
 (and
  (< (+ xa1 (* 2 ya1)) 0)
  )
 (and
  (>= (+ xa (* 2 ya)) 0)
  (= x (+ xa (* 2 ya)))
  (= y (+ (* (- 2) xa) ya))
  (= x1 (+ x 1))
  (= y1 (+ y x1))
  (= xa1 (- x1 (* 2 y1)))
  (= ya1 (+ (* 2 x1) y1))
  )
 0)
