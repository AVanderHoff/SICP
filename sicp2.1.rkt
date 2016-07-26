(define nil '())
(define (square x)(* x x))
(define (cube x)(* x x x))
(define (numer x)(car x))
(define (denom x) (cdr x))
(define (print-rat x)(newline)(display (numer x))(display "/")(display (denom x)))

(define (make-rat n d)(let((g(gcd n d)))(if (> 0 d) (cons (/ (- n) g) (/ (- d) g))(cons (/ n g) (/ d g)))))

(define (make-point x-point y-point) (cons x-point y-point))
(define (x-point x)(car x))
(define (y-point x) (cdr x))

(define (start-segment point)(car point))
(define (end-segment point)(cdr point))
(define (make-segment start end) (cons start  end))

(define (mid-point x)( cons (/(+ (x-point(start-segment x)) (x-point(end-segment x))) 2) (/ (+ (y-point(start-segment x)) (y-point(end-segment x))) 2)))

(define (print-point p)(newline)(display "(")(display (x-point p))(display ",")(display (y-point p))(display ")"))

(define a (make-point 1 1))
(define b (make-point 1 5))
(define c (make-point 4 5))
(define d (make-point 4 1))
(define (checkrec a b c d) (and (= (x-point a) (x-point b)) (= (x-point c)(x-point d)) (= (y-point b) (y-point c)) (= (y-point a) (y-point d))))
(define (area a b c d)( * (- (y-point b) (y-point a)) (- (x-point d) (x-point a))))
(define (perimeter a b c d) (+ (* (- (y-point b) (y-point a)) 2) (* (- (x-point d) (x-point a)) 2)))

(define (rectangle a b c d function)(if (checkrec a b c d) (function a b c d) 'not-a-rectangle))


;(define (kons x y)
;  (define (dispatch m)
;    (cond((= m 0)x)
;         ((= m 1) y)
;         (else (error "arguement not 0 or 1" m))))dispatch)
;(define (kar z) (z 0))
;(define (kdr z) (z 1))

;(define (kons x y)(lambda (m) (m x y)))
;(define (kar z)(z (lambda (p q) p)))
;(define (kdr z)(z (lambda (p q ) q)))
;(kdr(kons 1 2))


;(define (inner-pair a b)(* (expt 2 a) (expt 3 b)))
;
;(define (findmaxdivide div num count)(if (not (= (remainder num div)0) ) count (findmaxdivide div (/ num div) (+ count 1))))
;
;
;(define (findpower pow num count)(if (= (expt pow count) num) count (findpower pow num (+ count 1))))
;
;(define (FindAorB AorB x)(let((opp(if (= AorB 2) 3 2))(pow(findmaxdivide AorB x 0)))(findpower opp (/ x (expt AorB pow)) 1)))
;
;
;(define (A f)(FindAorB 3 f))
;(define (B f)(FindAorB 2 f))
;
;(define (a x)(findmaxdivide 2 x 0))
;(define (b x)(findmaxdivide 3 x 0))

;(define zero (lambda (f)(lambda (x) x)))
;(define (add-1 n)(lambda(f) (lambda (x) (f ((n f) x)))))
;
;(define (inc n)(+ 1 n))
;
;
;
;(define one
;   (lambda (f) (lambda (x) (f x))))
;(define two
;   (lambda (f) (lambda (x) (f (f x)))))

(define (make-interval a b)(cons a b))
(define (upper-bound x)(cdr x))
(define (lower-bound x)(car x))



(define (add-interval x y)(make-interval (+ (lower-bound x) (lower-bound y))(+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
   (make-interval (- (lower-bound x) (upper-bound y))
                  (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)(let((p1 (* (lower-bound x) (lower-bound y)))
                               (p2 (* (lower-bound x) (upper-bound y)))
                               (p3 (* (upper-bound x) (lower-bound y)))
                               (p4 (* (upper-bound x) (upper-bound y))))
                            
                            
                            (make-interval (min p1 p2 p3 p4)(max p1 p2 p3 p4))))



(define (div-interval x y)(if (and (>= (upper-bound y) 0)(<= (lower-bound y)0)) "y can not span a zero"
                              (mul-interval x (make-interval (/ 1.0 (upper-bound y))(/ 1.0 (lower-bound y))))))

(define dean '(1 . 3))
(define kc '(3 . 4))



(define (make-center-width c w)(make-interval (- c w) (+ c w)))
(define (center i) (/ (+ (lower-bound i) (upper-bound i))2))
(define (width i)(/ (-(upper-bound i) (lower-bound i)) 2))


(define (make-center-percent c p)(make-interval (* c (- 1 p)) (* c (+ 1 p))))


(define (percent i) (/(width i)(center i)))


(define a(make-center-width 5 .6))
(define b (make-center-width 6 .11))

(define(par1 r1 r2)(div-interval (mul-interval r1 r2)(add-interval r1 r2)))

(define(par2 r1 r2)(let((one(make-interval 1 1)))(div-interval one (add-interval (div-interval one r1)(div-interval one r2)))))

(define (appendd list1 list2)(if(null? list1)list2 (cons (car list1)(appendd (cdr list1) list2))))

(define (last-pair lst)(if (null? (cdr lst)) lst (last-pair (cdr lst))))

(define (reverselist lst )
  (define (revhelp empty lst )(if (null? lst) empty  (revhelp (cons (car lst) empty) (cdr lst))))
  (revhelp null lst))


(define (cc amount coin-values)(cond((= amount 0)1)
                                    ((or (< amount 0) (no-more? coin-values))0)
                                    (else(+ (cc amount (except-first-denomination coin-values))
                                            (cc (- amount (first-denomination coin-values)) coin-values)))))
(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)
(define us-coins (list 1 5 10 25 50))


;(define f (lambda (x y . z) <body>)
;(define g (lambda w <body>))
;remember that you use car of y and cdr y instead of whats before period

(define (same-parity  . y)
  (define (parity-helper n sign)(cond((null? n)'())
                                     ((sign (car n))(cons (car n) (parity-helper (cdr n) sign)))
                                     (else(parity-helper (cdr n) sign))))
                                     (if (even? (car y)) (parity-helper y even?)(parity-helper y odd?)))



(define (square-list items)(if (null? items) null (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (foor-each proc list) 
   (cond 
    ((null? list) #t) 
    (else (proc (car list)) 
          (foor-each proc (cdr list))))) 


(define (count-leaves x)(cond((null? x)0)
                             ((not (pair? x))1)
                             (else(+ (count-leaves (car x))
                                     (count-leaves (cdr x))))))


;(car(cdr(car(cdr(cdr '(1 3 (5 7) 9))))))
;(car (car '((7))))
;(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr '(1(2(3(4(5(6 7))))))))))))))))))


(define(deep-reverse x)
  (cond((null? x) '())
       ((not(pair? (car x)))(append (deep-reverse (cdr x)) (list(car x))))
       (else(append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))))

(define (fringe x)
  (cond((null? x)'())
       ((pair? (car x))(append (fringe (car x)) (fringe (cdr x))))
       (else(append (list(car x)) (fringe (cdr x))))))

(define (left-branch mobile)
   (car mobile))

(define (right-branch mobile)
   (car (cdr mobile)))

(define (branch-length branch)
   (car branch))

(define (branch-structure branch)
   (car (cdr branch)))

(define dean '((5 ((2 3) (2 3))) (3 ((2 3) (4 5)))))
(define kc '(5 ((2 3) (2 3))))
(define danny '((4 5)(5 6)))
(define (bubble x)(cond((null? x)0)
                       (else(+ (cadr(car x)) (bubble (cdr x))))))



(define (half-weight mobile)(cond((null? mobile)0)
                                 ((number? (car mobile))(half-weight (cadr mobile)))
                                 (else(+ (cadr(car mobile)) (half-weight (cdr mobile))))))



(define (scale-tree tree factor)(cond((null? tree)nil)
                                      ((not (pair? tree))(* tree factor))
                                      (else(cons (scale-tree (car tree) factor)
                                                 (scale-tree (cdr tree) factor)))))
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree) (if (pair? sub-tree)(scale-tree sub-tree factor)(* sub-tree factor)))tree))

(define (square-tree lst)
  (cond((null? lst)nil)
       ((number? lst)(square lst))
       (else(cons (square-tree (car lst))
                  (square-tree (cdr lst))))))
                                           
(define (tree-map func tree)
  (map (lambda (sub-tree)(if (pair? sub-tree)(tree-map func sub-tree) (func sub-tree)))tree))
(define (square-tree-2 tree)(tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let((rest(subsets(cdr s))))
        (append rest (map (lambda (x)  (cons(car s) x))rest)))))

(define (accumulate-recur op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate-recur op initial (cdr sequence)))))
(define (newmap p sequence)
  (accumulate-recur (lambda (x y) (cons (p x) y) nil sequence)))

(define (newappend seq1 seq2)
  (accumulate-recur cons seq2 seq1))



(define (newlength sequence)
  (accumulate-recur (lambda (x y) (+ 1 y)) 0 sequence))

(define (many-square number power)
  (if (= power 1)number
      (* number (many-square number (- power 1)))))

(define (list-gen x)
  (define (list-gen-help y lst)
    (if (> 0 y)lst
        (list-gen-help (- y 1) (cons y lst))))
  (list-gen-help (- x 1) nil))

(define (horner-eval x coefficient-sequence)
   (accumulate-recur (lambda (this-coeff higher-terms)
                 (+ (* x higher-terms) this-coeff))
               0
               coefficient-sequence))

(define (he x coeff-seq)
  (if (null? coeff-seq)
    0
    (+ (car coeff-seq)
       (* x (he x (cdr coeff-seq))))))
      
(define (accumulate-recur op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate-recur op initial (cdr sequence)))))





(define (count-leaves x)(cond((null? x)0)
                             ((not (pair? x))1)
                             (else(+ (count-leaves (car x))
                                     (count-leaves (cdr x))))))
(define (identity x) x)

(define (new-count-leaves t)(accumulate-recur + 0 (map (lambda (x) (if (pair? x) (new-count-leaves x) 1))    t)))

           
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil (cons (accumulate-recur op init (map car seqs)) (accumulate-n op init (map cdr seqs)))))
             


(define (dot-product v w)(accumulate-recur + 0 (map * v w)))
(define (matrixXvector m v)(map (lambda (x) (append (accumulate + (map * x v))))m))
(define (matrix-*-vector m v)
   (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)(accumulate-n cons '() mat))

(define (matrixXmatrix m n)(let((cols(transpose n)))(map (lambda (x) (matrixXvector cols x))m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
           (iter initial sequence))


(define (reverse1 seq)(accumulate-recur (lambda (x y)(append y (list x)) ) nil seq))
(define (reverse2 seq)(fold-left (lambda (x y) (append (list y) x)) nil seq))

(define (find-divisor n test-divisor)(cond((> (square test-divisor) n) n)
                                          ((divides? test-divisor n)test-divisor)
                                          (else(find-divisor n (next test-divisor)))))

(define (next x)(if(equal? x 2) 3 (+ x 2)))

(define (divides? a b)(= (remainder b a) 0))
(define (smallest-divisor n)(find-divisor n 2))

(define (prime? n)(= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)(accumulate-recur append nil (map proc seq)))
(define (prime-sum? pair)(prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i) (map (lambda (j) (list i j))(enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))s)))

(define (unique-pairs n)
   (flatmap
    (lambda (i)
      (map (lambda (j) (list i  j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))


(define (add-to s)(lambda (x)(= s (+ (car x) (cadr x) (caddr x))))) 

(define (unique-pairs2 n)
   (flatmap
    (lambda (i)
      (map (lambda (j) (list i (- i 1) j))
           (enumerate-interval 1 (- i 2))))
    (enumerate-interval 1 n)))

  ;(map (lambda (x) (permutations x)) (unique-pairs2 5))
(define (add-sum x)(list (accumulate-recur + 0 x) x))
(define (add-sum-to-list n ) (map add-sum (unique-pairs2 n)))
(define (filter-out-sums n s )(filter (lambda (x) (= (car x) s)) n))
(define (remove-car n)(cadr n))
(define (ordered-triples n s)(map (lambda (x) (permutations x)) (map remove-car (filter-out-sums (add-sum-to-list n) s))))

(define (ordered-triples3 n)
   (flatmap (lambda (i)
      (flatmap (lambda (j)
         (map (lambda (k)
                (list i j k))
              (enumerate-interval 1 (- j 1))))
       (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))


(define (queens board-size)
   (define (queen-cols k) 
     (if (= k 0)
         (list empty-board)
         (filter
          (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
   (queen-cols board-size))
(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

(define empty-board null)

(define (adjoin-position row col positions)
   (append positions (list (make-position row col))))



(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (position-col q))))
                               positions)))
   (define (attacks? q1 q2)
     (or (= (position-row q1) (position-row q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))
                                  
 (require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
                                 


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let((top-left(beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let((smaller(up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-limit painter n)
  (let((quarter (corner-split painter n)))
    (let((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;(define right-split (split beside below))
;(define up-split (split below beside))

(define(split arg1 arg2)
  (lambda (painter n)(if (= n 0) painter (let((smaller((split arg1 arg2) painter (- n 1))))
                                  (arg1 painter (arg2 smaller smaller))))))
(define (right-split)(split beside below))
(define (up-split)(split below beside))

(define(frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect-v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge-frame2 frame))))))

(define (make-vector a b)(cons a b))
(define (xcor-vect a)(car a))
(define (ycor-vect a)(cdr a))
(define (add-vect x y)(cons (+ (x-cor-vect x)(x-cor-vect y)) (+ (y-cor-vect x)(y-cor-vect y))))
(define (sub-vect x y)(cons (- (x-cor-vect x) (x-cor-vect y)) (- (y-cor-vect x)(y-cor-vect y))))
(define (scale-vect s p)(cons (* s (x-cor-vect p)) (* s (y-cor-vect p))))
(define dean (make-vector 4 5))
