(interpolant
 (and
  (= x1 0)
  (= v1 v0)  
  (> v0 0)
  (= x2 (+ x1 (* v1 2)))
  (< x2 0)
  )
 (and
  (= v2 (+ v1 2))
 )
 1)
