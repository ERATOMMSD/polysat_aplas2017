(interpolant
  (and		
  (= v1 0)
  )

 (and
  (= x1 0)
  (= v2 (+ v1 eps))
  (= x2 (+ x1 (* v1 eps)))
  (< x2 0)
  (> eps 0)
 )

 2)
