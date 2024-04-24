(define-who test-all-extra
  (lambda arg*
    (fluid-let ([tests tests-extra])
      (apply test-all arg*))
    (test-one `,(list 'quote tests))
    (test-one test-interpreter)))

(define tests-extra
  '('a
    'xyz
    '+a
    '-a
    '.a
    'λ
    '\x00;
    '\"
    '\#
    '\'
    '\(
    '\[
    '\\
    '\;
    '\@
    '\`
    '\{
    '\|
    '\,
    '\-1
    '\+0
    '\.0
    '\00
    '\\x0\;
    '(a 0 b 1 c 2)
    '(a a a b b b)
    (symbol? 'a)
    (symbol? 'λ)
    (symbol? 1)
    (symbol? '(a b c))
    (eq? 'q 'r)
    (eq? 't 't)
    #\a
    #\我
    #\x00
    #\;
    '(#\a #\b a b)
    (char? #\x)
    (char? 'a)
    (char? 42)
    (char->integer #\!)
    (integer->char 55)
    (char=? #\0 #\1)
    (char=? #\+ #\+)
    (quotient 6 3)
    (quotient 5 2)
    (quotient -7 3)
    (quotient -10 -4)
    (quotient 9 -4)
    (remainder 6 3)
    (remainder 5 2)
    (remainder -7 3)
    (remainder -10 -4)
    (remainder 9 -4)
    (cond [(= 1 2) '=]
          [(< 1 2) '<]
          [(> 1 2) '>])
    (cond [else #t])
    (cond [#t #f]
          [else #t])
    (cond [#f #f]
          [else #t])
    (let ([cond (lambda (x) x)])
      (cond 0))
    (let ([else #f])
      (cond [else #t]))
    `a
    `(a b c)
    (let ([a 'A])
      `,a)
    `(,(+ 1 2))
    `(,(car `(,(cons 'x '(y z)) b c)))
    (let ([quasiquote (lambda () 's)])
      (quasiquote))
    (let ([unquote 'x])
      `(,x))
    (let ([a 'A])
      ``,a)
    (let ([a 'A])
      ``,,a)
    `(a `(b ,(c ,(cons 1 2))))

    ;; #t in chez but #f here
    ;; in chez a continuation really is the stack
    ;; (call/cc
    ;;   (lambda (k1)
    ;;     (call/cc
    ;;       (lambda (k2)
    ;;         (eq? k1 k2)))))

    ;; Euclidean
    (letrec ([gcd
               (lambda (x y)
                 (cond [(> x y) (gcd y x)]
                       [(= x 0) y]
                       [else
                         (gcd (remainder y x) x)]))])
      `(,(gcd 423 84)
        ,(gcd 1 832)
        ,(gcd 0 7)
        ,(gcd 7 0)
        ,(gcd 7 7)
        ,(gcd 36 32)))
    ;; NbE
    (letrec
        ([val
           (letrec ([assq
                      (lambda (x l)
                        (if (null? l) #f
                            (if (eq? (car (car l)) x)
                                (car l)
                                (assq x (cdr l)))))]
                    [val1
                      (lambda (e env)
                        (cond [(symbol? e) (cdr (assq e env))]
                              [(eq? (car e) 'quote) (car (cdr e))]
                              [(eq? (car e) 'lambda)
                               (lambda (arg)
                                 (val1
                                   (car (cdr (cdr e)))
                                   (cons (cons (car (car (cdr e))) arg) env)))]
                              [else ((val1 (car e) env) (val1 (car (cdr e)) env))]))])
             (lambda (e) (val1 e '())))]
         [read-back
           (letrec ([uvars '(a b c d e f g h i j k l m n o p q r s t u v w x y z)]
                    [c 0]
                    [created-neutral '()]
                    [newvar
                      (lambda ()
                        (set! c (+ c 1))
                        (letrec ([locate
                                   (lambda (i l)
                                     (if (eq? i 1)
                                         (car l)
                                         (locate (- i 1) (cdr l))))])
                          (locate c uvars)))]
                    [neutral?1
                      (lambda (x l)
                        (cond [(null? l) #f]
                              [(eq? (car (car l)) x) (cdr (car l))]
                              [else (neutral?1 x (cdr l))]))]
                    [neutral?
                      (lambda (x) (neutral?1 x created-neutral))]
                    [create-neutral
                      (lambda (e)
                        (letrec ([ne (lambda (y)
                                       (create-neutral (cons ne y)))])
                          (set! created-neutral (cons (cons ne e) created-neutral))
                          ne))])
             (lambda (e)
               (cond [(neutral? e)
                      (let ([app (neutral? e)])
                        (if (symbol? app) app
                            (let ([rator (car app)]
                                  [rand (cdr app)])
                              `(,(read-back rator) ,(read-back rand)))))]
                     [(procedure? e)
                      (let ([v (newvar)])
                        (let ([ne (create-neutral v)])
                          `(lambda (,v) ,(read-back (e ne)))))]
                     [else `',e])))]
         [normalize (lambda (e) (read-back (val e)))])
      (normalize '(((lambda (x) (lambda (y) (x y)))
                    (lambda (x) (x x)))
                   (lambda (x)
                     (lambda (y) x)))))
    ;; yin-yang
    (begin
      (display #\;)
      (display #\space)
      (let ([count 50])
        (let ([yin
                ((lambda (cc)
                   (display '陰)
                   (set! count (- count 1))
                   (if (= count 0)
                       (lambda (x) (display #\newline))
                       cc))
                 (call/cc (lambda (c) c)))])
          (let ([yang
                  ((lambda (cc)
                     (display '陽)
                     (set! count (- count 1))
                     (if (= count 0)
                         (lambda (x) (display #\newline))
                         cc))
                   (call/cc (lambda (c) c)))])
            (yin yang)))))
    ;; easy collection
    (letrec ([malloc
               (lambda (n)
                 (if (= 0 n)
                     'success
                     (let ([trash (make-vector 80000)])
                       (vector-set! trash 0
                         (malloc (- n 1))))))])
      (malloc 5))))

(define test-trace
  '(letrec ([kk #f]
            [fact
              (lambda (n)
                (cond [(<= n 0) (call/cc (lambda (k) (set! kk k) 1))]
                      [else (* n (fact (- n 1)))]))])
     (begin
       (fact 10)
       (inspect kk))))

(define fake-interpreter
  '(letrec
       ([extend (lambda (x v e) (cons (cons x v) e))]
        [extend*
          (lambda (xs vs e)
            (if (null? xs) e
                (extend* (cdr xs) (cdr vs)
                  (extend (car xs) (car vs) e))))]
        [map
          (lambda (f l)
            (if (null? l) '()
                (cons (f (car l)) (map f (cdr l)))))]
        [find
          (lambda (x e)
            (cond [(null? e) #f]
                  [(eq? (car (car e)) x) (cdr (car e))]
                  [else (find x (cdr e))]))]
        [apply
          (lambda (clos vs)
            (if (eq? (car clos) 'lambda)
                ;; (xs env . e)
                (let ([xs (car (cdr clos))]
                      [env (car (cdr (cdr clos)))]
                      [e (cdr (cdr (cdr clos)))])
                  (eval1 e (extend* xs vs env)))
                ;; letrec bundle
                ;; (x ((x-1 . f-1) ... (x-n . f-n)) . env)
                (let ([x (car (cdr clos))]
                      [x-f* (car (cdr (cdr clos)))]
                      [env (cdr (cdr (cdr clos)))])
                  (let ([f (find x x-f*)]
                        [env^ (extend*
                                (map (lambda (x-f) (car x-f)) x-f*)
                                (map (lambda (x-f) (cons 'letrec (cons (car x-f) (cons x-f* env)))) x-f*)
                                env)])
                    (let ([xs (car (cdr f))]
                          [e (car (cdr (cdr f)))])
                      (eval1 e (extend* xs vs env^)))))))]
        [eval1*
          (lambda (es env)
            (if (null? es) '()
                (cons (eval1 (car es) env) (eval1* (cdr es) env))))]
        [eval-binary-operator
          (lambda (rator)
            (cond
              [(eq? rator '*) (lambda (x y) (* x y))]
              [(eq? rator '+) (lambda (x y) (+ x y))]
              [(eq? rator '-) (lambda (x y) (- x y))]
              [(eq? rator '<) (lambda (x y) (< x y))]
              [(eq? rator '<=) (lambda (x y) (<= x y))]
              [(eq? rator '=) (lambda (x y) (= x y))]
              [(eq? rator '>) (lambda (x y) (> x y))]
              [(eq? rator '>=) (lambda (x y) (>= x y))]
              [(eq? rator 'eq?) (lambda (x y) (eq? x y))]
              [(eq? rator 'cons) (lambda (x y) (cons x y))]
              [else #f]))]
        [eval-unary-operator
          (lambda (rator)
            (cond
              [(eq? rator 'boolean?) (lambda (x) (boolean? x))]
              [(eq? rator 'fixnum?) (lambda (x) (fixnum? x))]
              [(eq? rator 'null?) (lambda (x) (null? x))]
              [(eq? rator 'pair?) (lambda (x) (pair? x))]
              [(eq? rator 'procedure?) (lambda (x) (procedure? x))]
              [(eq? rator 'car) (lambda (x) (car x))]
              [(eq? rator 'cdr) (lambda (x) (cdr x))]
              [(eq? rator 'symbol?) (lambda (x) (symbol? x))]
              [else #f]))]
        [eval1
          (lambda (e env)
            (cond
              [(fixnum? e) e]
              [(boolean? e) e]
              [(symbol? e) (find e env)]
              [(eq? (car e) 'quote) (car (cdr e))]
              [(eq? (car e) 'if)
               (if (eval1 (car (cdr e)) env)
                   (eval1 (car (cdr (cdr e))) env)
                   (eval1 (car (cdr (cdr (cdr e)))) env))]
              [(eq? (car e) 'lambda)
               (cons 'lambda
                 (cons (car (cdr e))
                   (cons env (car (cdr (cdr e))))))]
              [(eq? (car e) 'let)
               (let ([x-e* (car (cdr e))])
                 (let ([e* (map (lambda (x-e) (car (cdr x-e))) x-e*)])
                   (let ([v* (eval1* e* env)])
                     (eval1 (car (cdr (cdr e)))
                       (extend*
                         (map (lambda (x-e) (car x-e)) x-e*)
                         v*
                         env)))))]
              [(eq? (car e) 'letrec)
               (let ([x--f* (car (cdr e))])
                 (let ([x-f* (map (lambda (x--f) (cons (car x--f) (car (cdr x--f)))) x--f*)]
                       [x* (map (lambda (x--f) (car x--f)) x--f*)])
                   (eval1 (car (cdr (cdr e)))
                     (extend*
                       x*
                       (map (lambda (x) (cons 'letrec (cons x (cons x-f* env)))) x*)
                       env))))]
              [(eval-binary-operator (car e))
               ((eval-binary-operator (car e))
                (eval1 (car (cdr e)) env)
                (eval1 (car (cdr (cdr e))) env))]
              [(eval-unary-operator (car e))
               ((eval-unary-operator (car e))
                (eval1 (car (cdr e)) env))]
              [else (apply (eval1 (car e) env) (eval1* (cdr e) env))]))])
     (lambda (e) (eval1 e '()))))

(define test-interpreter
  `(let ([eval ,fake-interpreter]
         [quote-eval (quote ,(parse-scheme fake-interpreter))])
     (letrec ((deep (lambda (n)
                      (if (<= n 0)
                          '(letrec ((fac (lambda (n)
                                           (if (<= n 0)
                                               1
                                               (* n (fac (- n 1)))))))
                             (fac 10))
                          (cons
                            quote-eval
                            (cons
                              (cons
                                'quote
                                (cons (deep (- n 1)) '()))
                              '()))))))
       (eval (deep 3)))))
