;; (+ (* x x) (* y y))

;; (simplify (not (=> (and (>= fact 1)
;;                         (> i 0)
;;                         (> i 1))
;;                    (and (>= (* fact i) 1)
;;                         (> (- i 1) 0)))))

;; (simplify (and (and (>= fact 1)
;;                     (> i 0)
;;                     (> i 1))
;;                (not (and (>= (* fact i) 1)
;;                          (> (- i 1) 0)))))

(simplify (>= x 1))
(simplify (<= x (- 1)))

;; (simplify (not (=> (> x 0) (> y 0))))
;; (simplify (and (> x 0) (not (> y 0))))
