(interpolant
 (and
  (= x1 0)
  (= v1 0)
  )
 (and
  (= x2 (+ x1 (* v1 2)))
  (= v2 (+ v1 2))
  (< x2 0)
 )
 1)
