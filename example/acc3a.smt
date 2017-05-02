(interpolant
 (and
  (= x1 0)
  (= v1 0)
  (= x2 (+ x1 (* v1 2)))
  (= v2 (+ v1 2))
  )
 (and
  (= x3 (+ x2 (* v2 2)))
  (= v3 (+ v2 2))
  (< x3 0)
 )
 0)
