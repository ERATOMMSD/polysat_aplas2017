(interpolant
 (and
  (>= n 0)
  (= i2 (+ i1 1))  
  (< i1 n)
  (< (* 2 s2) (* n (+ n 1)))
 )
 (and
  (= s2 (+ s1 i2))
  (= s1 0)
  (= i1 0)
 )
 3)
