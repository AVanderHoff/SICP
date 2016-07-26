(define (square x)(* x x))
(define (cube x)(* x x x))

(define (sum term a next b)(if (> a b)0(+ (term a) (sum term (next a) next b))))

(define (inc n)(+ n 1))
(define (sum-cube a b)(sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1 (* x (+ x 2))))
      (define (pi-next x)
        (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)(+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)dx))

(define (itercube term a next b result) (if (> a b) result (itercube term (next a) next b (+ result (term a)))))

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity x)x)
(define (product term a next b)(if (> a b)1 (* (term a) (product term (next a) next b))))

(define (factorial x)(product identity 1 inc x))

(define(wallis x)( * (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))

(define (term x)(/ (* 4.0 (square x)) (- (* 4.0 (square x)) 1)))

(define (product2 term a next b)
  (define (result2 a result)
    (if (> a b)
        result
        (result2 (next a) (* result (term a)))))
      (result2 a 1))

(define (accumulate combiner null-value term a next b) (if (> a b) null-value (combiner (term a) (accumulate combiner null-value term (next a) next b))))
                                                           
(define (itaccum combiner null-value term a next b)
  (define (itaccumhelp a result)(if (> a b) result (itaccumhelp (next a) (combiner result null-value (term a)))))
  (itaccumhelp a 0))

(define (divides? a b)(= (remainder b a) 0))

(define (find-divisor n test-divisor)(cond((> (square test-divisor) n) n)
                                          ((divides? test-divisor n)test-divisor)
                                          (else(find-divisor n (next test-divisor)))))

(define (next x)(if(equal? x 2) 3 (+ x 2)))


(define (smallest-divisor n)(find-divisor n 2))

(define (prime? n)(= n (smallest-divisor n)))





(define (GCD a b)(if (= b 0) a (GCD b (remainder a b))))




(define (filteredaccum combiner null-value term a next b filter)(if (> a b) null-value (combiner (question a term filter null-value b) 
                                                                                                 (filteredaccum combiner null-value term (next a) next b filter))))
(define (question a term filter null-value b)(if (equal? (filter a b) null-value) (term a) null-value))

(define (average a b)(/ (+ a b) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess )
  (define (close-enough? v1 v2)(< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let((next (f guess)))
      (if (close-enough? guess next)
          next
       (try next))))
         (try first-guess ))

(define (cont-frac n d k)
  (define (frac i)
     (if (< i k)
         (/ (n i) (+ (d i) (frac (+ i 1))))
         (/ (n i) (d i))))
  (frac 1))


  



(define (D i)(if (not (= (remainder (+ i 1) 3) 0)) 1 (* 2(/ (+ i 1) 3))))
(define (D2 i) (- (* 2 i) 1))

(define (N2 i x)(cond((equal? i 1) x)
                     (else (square x))))

(define (cont-frac2  n d k)
  (define (frac i)
     (if (< i k)
         (/ (n i ) (- (d i) (frac (+ i 1))))
         (/ (n i ) (d i))))
  (frac 1))


(define (newtang x k) (cont-frac2 (lambda (i) (if (equal? i 1) x (square x))) D2 k))

(define (average-damp f)(lambda (x) (average x (f x))))

(define (new-sqrt x)(fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (new-cube x) (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(define (tothe4th x)(fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y))))) 1.0))
(define (tothe? x ad plus1)(fixed-point ((repeated average-damp ad)(lambda (y) (/ x (plus1 y)))) 1.0))
(define (?power x n)(if (= n 1)x(* x (?power x (- n 1)))))

(define (tothehelp x n ad)(fixed-point ((repeated average-damp ad)(lambda (y) (/ x (?power y n)))) 1.0))
(define(tothe? x n ad)(tothehelp x (- n 1) ad))



(define(deriv g)(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)(lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess) (fixed-point (newton-transform g) guess))
(define (squareroot x)(newtons-method (lambda (y) (- (square y) x)) 1.0))
(define (cube x)(* x x x))

(define (cubichelp a b c x)(newtons-method  (lambda (y)(- (+ (cube y) (* a (square y)) (* b y) c) x)) 1.0))

(define (cubic a b c) (cubichelp a b c 0))

(define (billcubic a b c)(lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double g)(lambda (x) (g (g x))))

(define (compose f g) (lambda (x) (f (g x))))


(define (repeated f n)(if (= n 1) f (compose f (repeated f (- n 1)))))


(define (smooth f dx)(lambda (x) (/   (+ ( f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (n-fold f dx n)(repeated (smooth f dx) n))


(define (new-fixed-point f first-guess close-enough? )
  
  (define (try guess)
    (let((next (f guess)))
      (if (close-enough? guess next)
          next
       (try next))))
         (try first-guess )) 

(define (close? x y)(< (abs (- x y)) 0.0001))

(define (iterative-improve1 close-enough? improve)(lambda (x y) (improve x y close-enough?)))

(define (iterative-improve close? improve)
  (define (imp guess) (if (close? guess ) guess (imp (improve guess)))) imp)

(define (new-sqrt x)((iterative-improve (lambda (y) (< (abs (- (square y) x)) .0001))
                                        (lambda (y) (average y (/ x y))))1.0))

(define (new-fixed-point2 f first-guess)((iterative-improve (lambda (y) (< (abs (- (f y) y)) .0001))
                                                            (lambda (y) (f y)))first-guess))

                                                            




















