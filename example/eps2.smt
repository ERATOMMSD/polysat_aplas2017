(interpolant
  (and		
  (= v1 0)
  (> eps 1)
  )

 (and
  (= x1 0)
  (= v2 (+ v1 eps))
  (= x2 (+ x1 (* v1 eps)))
  (< x2 0)
  (> eps 1)
 )

 0)
