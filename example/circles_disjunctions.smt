(interpolant
 (or (>= 1 (+ (* (- x 1) (- x 1)) (* y y))) (>= 1 (+ (* x x) (* y y))))
 (or  (>= 2 (+ (* (- x 1) (- x 1)) (* (- y 4) (- y 4)))) (>= 1 (+ (* x x) (* (- y 4) (- y 4)))))
 2)
