(interpolant
  (and		
  (= v1 0)
  (= x1 0)
  (= v2 (+ v1 eps))
  )
 (and
  (= x2 (+ x1 (* v1 eps)))
  (> eps 0)
  (< x2 0)
  (> eps 0)
 )
 1)
