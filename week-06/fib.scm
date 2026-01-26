


;; (define fib
;;   (lambda (n)
;;     (cond
;;      ((= n 0) 0)
;;      ((= n 1) 1)
;;      (#t (+ (fib (- n 1))
;; 	    (fib (- n 2)))))))

;; (define fib-sum
;;   (lambda (n)
;;     (let loop ((a 0)(b 1)(k n)(sum (+ a b)))
;;       (cond
;;        ((> k 0) (loop 
      
;;     (cond
;;      ((= n 0) 0)
;;      ((= n 1) 1)
;;      (#t (+ (fib (- n 1))
;; 	    (fib (- n 2)))))))

#|
n   : 0 1 2 3 4 5 6 7   8  9 10
sum : 0 1 1 2 3 5 8 13 21 34 55
      * * ---- two seed values 
|#

(define fib-sum
  (lambda (n)
    (let loop ((a 0)(b 1)(k n))
      
	       
	       (sum (+ a b)))
      (cond
       ((> k 0) (loop 



