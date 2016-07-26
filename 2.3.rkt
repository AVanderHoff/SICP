(define (memq item x)
  (cond((null? x)#f)
       ((eq? item (car x))x)
       (else(memq item (cdr x)))))


(define (eql? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (eql? (car p1) (car p2))
              (eql? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))


(define (variable? x)(symbol? x))
(define (same-variable? v1 v2)(and (variable? v1)(variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond((=number? a1 0)a2)
       ((=number? a2 0)a1)
       ((and (number? a1) (number? a2))(+ a1 a2))
       (else (list '+ a1 a2))))
(define(=number? exp num)
  (and(number? exp) (= exp num)))
 
(define (make-product m1 m2)
  (cond((or (=number? m1 0)(=number? m2 0))0)
       ((=number? m1 1)m2)
       ((=number? m2 1)m1)
       ((and(number? m1)(number? m2))(* m1 m2))
       (else (list '* m1 m2))))
(define (simplify exp)
  (if (null? (cdr exp)) (car exp) exp))
(define (sum? x)(and (pair? x) (eq? (cadr x) '+)))
(define (addend s)(car s))
;(define (augend s)(caddr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))
 



(define (product? x)(and (pair? x)(eq? (cadr x) '*)))
(define (multiplier p)(car p))
;(define (multiplicand p)(caddr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))
(define (deriv exp var)
  (cond((number? exp)0)
       ((variable? exp)(if (same-variable? exp var) 1 0))
       ((sum? exp)
        (make-sum (deriv (addend exp)var)
                  (deriv (augend exp) var)))
       ((product? exp)
        (make-sum 
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp))))
       
       ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
       (else(error "unknown expression type--DERIV" exp))))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (element-of-set? x set)
  (cond((null? set ) #f)
       ((equal? x (car set))#t)
       (else(element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond((or (null? set1)(null? set2))'())
       ((element-of-set? (car set1) set2)
        (cons (car set1)
              (intersection-set (cdr set1) set2)))
       (else(intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond((null? set1)set2)
       ((element-of-set? (car set1) set2)
        (union-set (cdr set1) set2))
       (else(cons (car set1) (union-set (cdr set1) set2)))))

(define (new-union set1 set2)
  (append set1 set2))

(define (count-appear x lst)
  (define(count-help lst cnt)
    (cond((null? lst)cnt)
         ((equal? (car lst) x)(count-help (cdr lst) (+ cnt 1)))
         (else(count-help (cdr lst) cnt))))
      (count-help lst 0))
(define (gen-multi x num)
  (cond((= num 0)'())
       (else( cons x (gen-multi x (- num 1))))))

(define (shorten x)
  (define (shorten-help y empt)
    (cond((null? y)empt)
         ((member? (car y) (cdr y))(shorten-help (cdr y) empt))
         (else(shorten-help (cdr y) (cons (car y) empt)))))
       (shorten-help x '()))

(define(multi-list x num)
  (cond((= num 0)'())
       (else (cons x(multi-list x (- num 1))))))
(define (appears x lst)
  (cond((null? lst)0)
       ((equal? (car lst) x)
        (+ 1(appears x (cdr lst))))
       (else(appears x (cdr lst)))))

(define (new-intersection set1 set2)
  (define (gen-first-set set1 empt)
    (cond((null? set1)empt)
         ((element-of-set? (car set1) set2)
          (gen-first-set (cdr set1) (cons (car set1) empt)))
         (else(gen-first-set (cdr set1) empt))))
        (define (gen-final-set newset set2 empt2)
            (let((short(shorten newset)))
          (cond((null? short)empt2)
                 (else(gen-final-set (cdr short) set2 (append (multi-list (car short) (appears (car short) set2)) empt2))))))
  (gen-final-set (gen-first-set set1 '()) set2 (gen-first-set set1 '())))

                 
 (define (element-of-set2 x set)
   (cond((null? set)#f)
        ((= (car set) x)#t)
        ((< x (car set))#f)
        (else(element-of-set2 x (cdr set)))))

 (define (intersection-set2 set1 set2)
   (if(or (null? set1) (null? set2))
      '()
      (let((x1(car set1))(x2(car set2)))
        (cond((= x1 x2)
              (cons x1 (intersection-set2 (cdr set1)
                                          (cdr set2))))
             ((< x1 x2)
              (intersection-set2 (cdr set1) set2))
             ((< x2 x1)
              (intersection-set2 set1 (cdr set2)))))))

 
 (define (adjoin-set2 x set)
   (cond((null? set)(list x))
     ((= x (car set))set)
        ((< x (car set))(cons x set))
        (else(cons (car set) (adjoin-set2 x (cdr set))))))
;(1 3 5 7) (2 4 8 10 12) 
 
 (define (union-set2 set1 set2)
   (cond((null? set1)set2)
        ((null? set2)set1)
        ((< (car set1) (car set2))(cons (car set1) (union-set2 (cdr set1) set2)))
        ((< (car set2) (car set1))(cons (car set2) (union-set2 set1 (cdr set2))))
        (else(cons (car set1) (union-set2 (cdr set1) (cdr set2))))))
 
(define (entry tree)(car tree))
(define (left-branch tree)(cadr tree))
(define (right-branch tree)(caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set3 x set)
  (cond((null? set)#f)
       ((= x (entry set))#t)
       ((< x (entry set))
        (element-of-set3 x (left-branch set)))
       (else
        (element-of-set3 x (right-branch set)))))

     
(define (adjoin-set3 x set)
  (cond((null? set)(make-tree x '() '()))
       ((= x (entry))set)
       ((< x (entry set))
        (make-tree (entry set)
                   (adjoin-set3 x (left-branch set))
                   (right-branch set)))
       ((> x (entry set))
        (make-tree (entry set)
                   (left-branch set)
                   (adjoin-set x (right-branch set))))))

                     
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
                         (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let((left-result (partial-tree elts left-size)))
          (let((left-tree (car left-result))
               (non-left-elts (cdr left-result))
               (right-size (- n (+ left-size 1))))
            (let((this-entry (car non-left-elts))
                 (right-result(partial-tree (cdr non-left-elts)
                                            right-size)))
              (let((right-tree (car right-result))
                   (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)remaining-elts))))))))

     
(define (union-set set1 set2)
  (let((s1(tree->list-2 set1))(s2(tree->list-2 set2)))
    (let((union(union-set2 s1 s2)))
      (list->tree union))))

(define tree1 '(5 (2 (1 () ())()) (9 (7 () ()) (10 () ()))))
(define tree2 '(8 (4 (2 () ()) ())(14 (9 () ()) (16 () ()))))

(define (intersection-set set1 set2)
  (let((s1 (tree->list-2 set1))
       (s2 (tree->list-2 set2)))
    (let((intersection
          (intersection-set2 s1 s2)))
         (list->tree intersection))))



(define (lookup given-key set-of-records)
  (cond((null? set-of-records)#f)
       ((equal? given-key (key (car set-of-records)))
        (car set-of-records))
       (else(lookup given-key (cdr set-of-records)))))


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    
                               (cadr pair))  
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append(encode-symbol (car message) tree)
             (encode (cdr message) tree))))
(define (encode-symbol sym tree)
  (define (element-of-symbols? x syms)
    (cond((null? syms)#f)
         ((equal? x (car syms))#t)
         (else(element-of-symbols? x (cdr syms)))))
       (define (encode-symbol-1 sym tree result)
         (let*((left(left-branch tree))
               (left-symbols(symbols left)))
           (if (element-of-symbols? sym left-symbols)
               (if(leaf? left)
                  (append result (list 0))
                  (encode-symbol-1 sym left (append result (list 0))))
               (let*((right(right-branch tree))
                     (right-symbols (symbols right)))
                 (if (element-of-symbols? sym right-symbols)
                     (if (leaf? right)
                         (append result (list 1))
                         (encode-symbol-1 sym right (append result (list 1))))
                     (error "unknown sym" sym))))))
                 (encode-symbol-1 sym tree '()))
                 
         
    









(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
                        
 
  (define (successive-merge tree)
  (if (null? (cdr tree))
      (car tree)
      (let ((node-1 (car tree))
            (node-2 (cadr tree))
            (rest (cddr tree)))
        (let ((new-node (make-code-tree node-1 node-2)))
          (successive-merge (adjoin-set new-node rest))))))                      
                                              ;              ;                                   ;
(define littlesnoot (generate-huffman-tree '((A 2)(NA 16) (BOOM 1)(SHA 3)(GET 2)(YIP 9)(JOB 2)(WAH 1))))
 

