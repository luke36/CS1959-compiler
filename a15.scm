;; todo: add trap-return-point (not now); interaction between call/cc and gc; is optimize-global (mostly) subsumed by a strong enough partial evaluator?

;; sra is evil: it makes ptrs non-ptrs (of course some other operands, but I believe sra is the only
;; possible source in this compiler). however,
;;   1. sra always appears as the second operand of *;
;;   2. operands of * are computed in order.
;; so this evil non-ptr is gone immediately and have no chance to be live on the frame.

(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.scm")
(load "helpers.scm")
(load "driver.scm")
(load "fmts.pretty")
(load "a15-wrapper.scm")

(define *standard* 'r6rs)
(define *cp-1-enabled* #t)
(define *closure-optimization-enabled* #t)
(define *iterated-coalescing-enabled* #t)
(define *optimize-jumps-enabled* #t)
(define *optimize-global-enabled* #t)
(define *max-inline-literal-size* 0)
(define *collection-enabled* #f)
(define *max-conservative-range* #f) ; may ask for more space than actually needed

(define disp-root-globals 0)
(define stack-base-register 'r14)
(define end-of-allocation-register 'r13)
(define return-address-location 'fv0)
(define mask-symbol #b111)
(define tag-symbol  #b100)

(define flag-true $true)
(define flag-false $false)
(define flag-nil $nil)
(define flag-trivial 0)
(define flag-pair 1)
(define flag-vector 2)
(define flag-vector-end 3)
(define flag-empty-vector 4)

(define take
  (lambda (n lst)
    (cond [(null? lst) '()]
          [(= n 0) '()]
          [else (cons (car lst) (take (- n 1) (cdr lst)))])))
(define drop
  (lambda (n lst)
    (cond [(null? lst) '()]
          [(= n 0) lst]
          [else (drop (- n 1) (cdr lst))])))
(define id (lambda (x) x))

(define binops
  '(+ - * logand logor sra))
(define binop?
  (lambda (x)
    (memq x binops)))
(define commutative?
  (lambda (x)
    (memq x '(+ * logand logor))))
(define binop->assembly
  (lambda (op)
    (match op
      [+ 'addq]
      [- 'subq]
      [* 'imulq]
      [logand 'andq]
      [logor 'orq]
      [sra 'sarq])))

(define overflow?
  (lambda (x)
    (and (integer? x)
         (exact? x)
         (not (fixnum-range? x)))))

(define relops
  '(= < <= > >=))
(define relop?
  (lambda (x) (memq x relops)))
(define rel->assembly
  (lambda (rel)
    (match rel
      [= 'je]
      [< 'jl]
      [<= 'jle]
      [> 'jg]
      [>= 'jge])))
(define not-rel->assembly
  (lambda (rel)
    (match rel
      [= 'jne]
      [< 'jge]
      [<= 'jg]
      [> 'jle]
      [>= 'jl])))
(define flip-relop
  (lambda (x)
    (match x
      [= '=]
      [< '>]
      [> '<]
      [>= '<=]
      [<= '>=])))

(define lambda?
    (lambda (x)
      (and (pair? x) (eq? (car x) 'lambda))))

(define (make-nopless-begin x*)
  (let ([x* (remove '(nop) x*)])
    (if (null? x*)
        '(nop)
        (make-begin x*))))

(define make-let
  (lambda (binding body)
    (if (null? binding)
        body
        `(let ,binding ,body))))

(define make-letrec
  (lambda (binding body)
    (if (null? binding)
        body
        `(letrec ,binding ,body))))

(define make-let-assigned
  (lambda (binding assign body)
    (if (null? binding)
        body
        `(let ,binding (assigned ,assign ,body)))))

(define value-primitives
  '(+ - * car cdr cons make-vector vector-length vector-ref void make-procedure procedure-ref procedure-code global-ref))
(define predicate-primitives
  '(<= < = >= > boolean? eq? fixnum? null? pair? vector? procedure? symbol?))
(define effect-primitives
  '(set-car! set-cdr! vector-set! procedure-set! inspect write global-set!))
(define value-primitive?
  (lambda (x) (memq x value-primitives)))
(define predicate-primitive?
  (lambda (x) (memq x predicate-primitives)))
(define effect-primitive?
  (lambda (x) (memq x effect-primitives)))
(define primitive?
  (lambda (x) (or (value-primitive? x) (predicate-primitive? x) (effect-primitive? x))))

(define user-primitive
  '([+             . 2]
    [-             . 2]
    [*             . 2]
    [car           . 1]
    [cdr           . 1]
    [cons          . 2]
    [make-vector   . 1]
    [vector-length . 1]
    [vector-ref    . 2]
    [void          . 0]
    [<=            . 2]
    [<             . 2]
    [=             . 2]
    [>=            . 2]
    [>             . 2]
    [boolean?      . 1]
    [eq?           . 2]
    [fixnum?       . 1]
    [null?         . 1]
    [pair?         . 1]
    [procedure?    . 1]
    [vector?       . 1]
    [symbol?       . 1]
    [set-car!      . 2]
    [set-cdr!      . 2]
    [vector-set!   . 3]
    [inspect       . 1]
    [write         . 1]))
(define user-primitive?
  (lambda (x) (assq x user-primitive)))
(define user-primitive->arity
  (lambda (x) (cdr (assq x user-primitive))))

(define-who parse-scheme
  (define check-bind-variable
    (lambda (var* e)
      (cond [(not (for-all symbol? var*))
             (format-error who "invalid bound variable in ~s" e)]
            [(not (set? var*))
             (format-error who "duplicate bound variable in ~s" e)]
            [else (void)])))
  (define check-datum
    (lambda (d)
      (cond [(pair? d)
             (check-datum (car d))
             (check-datum (cdr d))]
            [(vector? d)
             (vector-for-each check-datum d)]
            [(integer? d)
             (unless (and (exact? d)
                          (fixnum-range? d))
               (format-error who "~s is not a fixnum" d))]
            [(boolean? d) (void)]
            [(null? d) (void)]
            [(symbol? d) (void)]
            [else (format-error who "invalid datum ~s" d)])))
  (define convert-and
    (lambda (rand*)
      (cond [(null? rand*) #t]
            [(null? (cdr rand*)) (car rand*)]
            [else `(if ,(car rand*)
                       ,(convert-and (cdr rand*))
                       (quote #f))])))
  (define convert-or
    (lambda (rand*)
      (cond [(null? rand*) #f]
            [(null? (cdr rand*)) (car rand*)]
            [else (let ([tmp (unique-name 'tmp)])
                    `(let ([,tmp ,(car rand*)])
                       (if ,tmp ,tmp ,(convert-or (cdr rand*)))))])))
  (define convert-cond
    (lambda (clause* env)
      (cond [(null? clause*) '(void)]
            [else (let* ([cls (car clause*)]
                         [cond (car cls)]
                         [body* (map (Expr env) (cdr cls))])
                    (if (and (eq? cond 'else)
                             (not (assq 'else env)))
                        `(begin ,@body*)
                        `(if ,((Expr env) cond)
                             (begin ,@body*)
                             ,(convert-cond (cdr clause*) env))))])))
  (define convert-quasiquote
    (lambda (qd env)
      (define unchanged
        (lambda (qd expr)
          (and (pair? expr)
               (eq? (car expr) 'quote)
               (eq? qd (cadr expr)))))
      (cond [(and (pair? qd)
                  (eq? (car qd) 'unquote)
                  (not (assq 'unquote env)))
             ((Expr env) (cadr qd))]
            [(pair? qd)
             (let ([a (car qd)]
                   [d (cdr qd)])
               (let ([a^ (convert-quasiquote a env)]
                     [d^ (convert-quasiquote d env)])
                 (if (and (unchanged a a^) (unchanged d d^))
                     `(quote ,qd)
                     `(cons ,a^ ,d^))))]
            [(vector? qd)
             (let ([qd^ (vector-map (lambda (qd) (convert-quasiquote qd env)) qd)]
                   [length (vector-length qd)])
               (if (let loop ([i 0])
                     (if (= i length)
                         #t
                         (and (unchanged (vector-ref qd i) (vector-ref qd^ i))
                              (loop (add1 i)))))
                   `(quote ,qd)
                   (let* ([tmp (unique-name 'tmp)]
                          [fill
                            (let loop ([i 0])
                              (if (< i length)
                                  (cons
                                    `(vector-set! ,tmp (quote ,i) ,(vector-ref qd^ i))
                                    (loop (add1 i)))
                                  '()))])
                     `(let ([,tmp (make-vector (quote ,length))])
                        (begin ,@fill ,tmp)))))]
            [else
              (check-datum qd)
              `(quote ,qd)])))
  (define Var
    (lambda (env)
      (lambda (var)
        (cond [(assq var env) => cdr]
              [(user-primitive? var) var]
              [else (format-error who "variable ~s is not bound" var)]))))
  (define Expr
    (lambda (env)
      (lambda (x)
        (match x
          [#t '(quote #t)]
          [#f '(quote #f)]
          [,n (guard (integer? x) (exact? x) (fixnum-range? x)) `(quote ,n)]
          [,var (guard (symbol? var)) ((Var env) var)]
          [(,proc ,[(Expr env) -> arg*] ...) (guard (assq proc env))
           `(,((Expr env) proc) ,arg* ...)]
          [(,prim ,[(Expr env) -> rand*] ...) (guard (user-primitive? prim))
           (if (= (user-primitive->arity prim) (length rand*))
               `(,prim ,rand* ...)
               (format-error who "incorrect argument count in call ~s" x))]
          [(quote ,datum)
           (check-datum datum)
           `(quote ,datum)]
          [(quasiquote ,quasidatum) (convert-quasiquote quasidatum env)]
          [(call/cc ,[(Expr env) -> expr]) `(call/cc ,expr)]
          [(if ,[(Expr env) -> cond] ,[(Expr env) -> conseq])
           `(if ,cond ,conseq (void))]
          [(if ,[(Expr env) -> cond] ,[(Expr env) -> conseq] ,[(Expr env) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(and ,[(Expr env) -> rand*] ...) (convert-and rand*)]
          [(or ,[(Expr env) -> rand*] ...) (convert-or rand*)]
          [(not ,[(Expr env) -> rand]) `(if ,rand (quote #f) (quote #t))]
          [(cond ,clause* ...) (convert-cond clause* env)]
          [(caar ,[(Expr env) -> expr]) `(car (car ,expr))]
          [(cadr ,[(Expr env) -> expr]) `(car (cdr ,expr))]
          [(cdar ,[(Expr env) -> expr]) `(cdr (car ,expr))]
          [(cddr ,[(Expr env) -> expr]) `(cdr (cdr ,expr))]
          [(caaar ,[(Expr env) -> expr]) `(car (car (car ,expr)))]
          [(caadr ,[(Expr env) -> expr]) `(car (car (cdr ,expr)))]
          [(cadar ,[(Expr env) -> expr]) `(car (cdr (car ,expr)))]
          [(cdaar ,[(Expr env) -> expr]) `(cdr (car (car ,expr)))]
          [(caddr ,[(Expr env) -> expr]) `(car (cdr (cdr ,expr)))]
          [(cdadr ,[(Expr env) -> expr]) `(cdr (car (cdr ,expr)))]
          [(cddar ,[(Expr env) -> expr]) `(cdr (cdr (car ,expr)))]
          [(cdddr ,[(Expr env) -> expr]) `(cdr (cdr (cdr ,expr)))]
          [(begin ,[(Expr env) -> expr*] ... ,[(Expr env) -> expr]) `(begin ,expr* ... ,expr)]
          [(set! ,var ,[(Expr env) -> expr])
           (if (not (symbol? var))
               (format-error who "invalid syntax ~s" x)
               `(set! ,((Var env) var) ,expr))]
          [(lambda (,formal* ...) ,expr* ...)
           (if (<= (length expr*) 0) (format-error who "invalid syntax ~s" x)) ; can improve
           (check-bind-variable formal* x)
           (let* ([uvar* (map unique-name formal*)]
                  [env^ (append (map cons formal* uvar*) env)]
                  [body* (map (Expr env^) expr*)])
             `(lambda (,uvar* ...) (begin ,body* ...)))]
          [(let ([,var* ,[(Expr env) -> expr*]] ...) ,body* ...)
           (if (<= (length body*) 0) (format-error who "invalid syntax ~s" x))
           (check-bind-variable var* x)
           (let* ([uvar* (map unique-name var*)]
                  [env^ (append (map cons var* uvar*) env)]
                  [body*^ (map (Expr env^) body*)])
             `(let ([,uvar* ,expr*] ...) (begin ,body*^ ...)))]
          [(letrec ([,var* ,expr*] ...) ,body* ...)
           (if (<= (length body*) 0) (format-error who "invalid syntax ~s" x))
           (check-bind-variable var* x)
           (let* ([uvar* (map unique-name var*)]
                  [env^ (append (map cons var* uvar*) env)]
                  [expr*^ (map (Expr env^) expr*)]
                  [body*^ (map (Expr env^) body*)])
             `(letrec ([,uvar* ,expr*^] ...) (begin ,body*^ ...)))]
          [(,[(Expr env) -> proc] ,[(Expr env) -> arg*] ...) `(,proc ,arg* ...)]
          [,x (format-error who "invalid expression ~s" x)]))))
  (lambda (p) ((Expr '()) p)))

(define-who proceduralize-primitive
  (define bindings)
  (define Expr
    (lambda (e)
      (match e
        [,prim (guard (or (user-primitive? prim) (eq? prim 'call/cc)))
         (when (not (assq prim bindings))
           (let ([new-p (unique-name prim)]
                 [new-fml* (let gen ([c (user-primitive->arity prim)])
                             (if (= c 0) '()
                                 (cons (unique-name 'x) (gen (sub1 c)))))])
             (set! bindings
               (cons (cons prim `(,new-p (lambda ,new-fml* (,prim . ,new-fml*))))
                 bindings))))
         (cadr (assq prim bindings))]
        [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
        [(let ([,uvar* ,[body*]] ...) ,[body]) `(let ([,uvar* ,body*] ...) ,body)]
        [(letrec ([,uvar* ,[body*]] ...) ,[body]) `(letrec ([,uvar* ,body*] ...) ,body)]
        [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
        [(lambda (,uvar* ...) ,[body]) `(lambda (,uvar* ...) ,body)]
        [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
        [(call/cc ,[expr]) `(call/cc ,expr)]
        [(quote ,imm) `(quote ,imm)]
        [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
        [,x (guard (uvar? x)) x])))
  (lambda (p)
    (set! bindings '())
    (let ([p^ (Expr p)])
      (make-letrec (map cdr bindings) p^))))

(define optimize-direct-call
  (lambda (e)
    (match e
      [(with-global-data ,data (outmost ,out ,[e])) `(with-global-data ,data (outmost ,out ,e))]
      [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
      [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
      [(let ([,uvar* ,[body*]] ...) ,[body]) `(let ([,uvar* ,body*] ...) ,body)]
      [(letrec ([,uvar* ,[body*]] ...) ,[body]) `(letrec ([,uvar* ,body*] ...) ,body)]
      [(lambda (,uvar* ...) ,[body]) `(lambda (,uvar* ...) ,body)]
      [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
      [(call/cc ,[expr]) `(call/cc ,expr)]
      [(quote ,imm) `(quote ,imm)]
      [((lambda (,uvar* ...) ,[body]) ,[arg*] ...)
       (make-let `([,uvar* ,arg*] ...) body)]
      [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
      [,x x])))

(define-who uncover-assigned
  (define Expr
    (lambda (e)
      (match e
        [(if ,[Expr -> cond a1] ,[Expr -> conseq a2] ,[Expr -> alter a3])
         (values `(if ,cond ,conseq ,alter) (union a1 a2 a3))]
        [(begin ,[Expr -> expr* a*] ... ,[Expr -> expr a])
         (values `(begin ,expr* ... ,expr) (apply union a a*))]
        [(let ([,uvar* ,[Expr -> body* a*]] ...) ,[Expr -> body a])
         (values
           `(let ([,uvar* ,body*] ...)
              (assigned ,(intersection a uvar*) ,body))
           (union (difference a uvar*) (apply union a*)))]
        [(letrec ([,uvar* ,[Expr -> body* a*]] ...) ,[Expr -> body a])
         (let ([a^ (apply union a a*)])
           (values
             `(letrec ([,uvar* ,body*] ...)
                (assigned ,(intersection a^ uvar*) ,body))
             (difference a^ uvar*)))]
        [(set! ,uvar ,[Expr -> expr a])
         (values `(set! ,uvar ,expr) (set-cons uvar a))]
        [(lambda (,uvar* ...) ,[Expr -> body a])
         (values
           `(lambda (,uvar* ...)
              (assigned ,(intersection a uvar*) ,body))
           (difference a uvar*))]
        [(,prim ,[Expr -> rand* a*] ...) (guard (primitive? prim))
         (values `(,prim ,rand* ...) (apply union a*))]
        [(call/cc ,[Expr -> expr a])
         (values `(call/cc ,expr) a)]
        [(quote ,imm) (values e '())]
        [(,[Expr -> proc a] ,[Expr -> arg* a*] ...)
         (values `(,proc ,arg* ...) (apply union a a*))]
        [,x (guard (uvar? x)) (values x '())])))
  (lambda (p)
    (match p
      [,[Expr -> e a] e])))

(define-who purify-letrec
  (define simple?
    (lambda (x* reduc-now?)
      (lambda (e)
        (match e
          [(if ,cond ,conseq ,alter)
           (and ((simple? x* reduc-now?) cond)
                ((simple? x* reduc-now?) conseq)
                ((simple? x* reduc-now?) alter))]
          [(begin ,expr* ... ,expr)
           (andmap (simple? x* reduc-now?) (cons expr expr*))]
          [(let ([,uvar* ,expr*] ...) (assigned (,as* ...) ,expr))
           (andmap (simple? x* reduc-now?) (cons expr expr*))]
          [(letrec ([,uvar* ,expr*] ...) ,body) ((simple? x* reduc-now?) body)]
          [(set! ,uvar ,expr) ((simple? x* reduc-now?) expr)]
          [(lambda (,uvar* ...) (assigned (,as* ...) ,expr)) ((simple? x* #f) expr)]
          [(,prim ,rand* ...) (guard (primitive? prim))
           (for-all (simple? x* reduc-now?) rand*)]
          [(call/cc ,expr) #f]
          [(quote ,imm) #t]
          [(,proc ,arg* ...)
           (if (eq? *standard* 'r6rs)
               (andmap (simple? x* reduc-now?) (cons proc arg*))
               (if reduc-now? #f (for-all (simple? x* #f) (cons proc arg*))))]
          [,x (guard (uvar? x)) (not (memq x x*))]))))
  (define partition
    (lambda (x* b* as*)
      (if (null? b*) (values '() '() '())
          (let* ([b (car b*)]
                 [x (car b)]
                 [e (cadr b)])
            (let-values ([(simple* lambda* complex*) (partition x* (cdr b*) as*)])
              (cond [(memq x as*) (values simple* lambda* (cons b complex*))]
                    [(lambda? e) (values simple* (cons b lambda*) complex*)]
                    [((simple? x* #t) e) (values (cons b simple*) lambda* complex*)]
                    [else (values simple* lambda* (cons b complex*))]))))))
  (lambda (e)
    (match e
      [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
      [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
      [(let ([,uvar* ,[expr*]] ...) (assigned (,as* ...) ,[expr]))
       `(let ([,uvar* ,expr*] ...) (assigned (,as* ...) ,expr))]
      [(letrec ([,uvar* ,[expr*]] ...) (assigned (,as* ...) ,[body]))
       (let-values ([(simple* lambda* complex*)
                     (partition uvar* `((,uvar* ,expr*) ...) as*)])
         (let* ([c-x* (map car complex*)]
                [c-e* (map cadr complex*)]
                [tmp* (map unique-name c-x*)]
                [innermost
                  (if (null? complex*) body
                      (make-begin
                        `((let ([,tmp* ,c-e*] ...)
                            (assigned ()
                              (begin (set! ,c-x* ,tmp*) ...)))
                          ,body)))])
           (make-let-assigned simple* '()
             (make-let-assigned `([,c-x* (void)] ...)
               c-x*
               (make-letrec lambda*
                 innermost)))))]
      [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
      [(lambda (,uvar* ...) (assigned (,as* ...) ,[expr]))
       `(lambda (,uvar* ...) (assigned (,as* ...) ,expr))]
      [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
      [(call/cc ,[expr]) `(call/cc ,expr)]
      [(quote ,imm) `(quote ,imm)]
      [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
      [,x (guard (uvar? x)) x])))

(define-who optimize-constant
  (define primitive->datum
    (lambda (prim)
      (match prim
        [+             +]
        [-             -]
        [*             *]
        [car           car]
        [cdr           cdr]
        [cons          cons]
        [vector-length vector-length]
        [<=            <=]
        [<             <]
        [=             =]
        [>=            >=]
        [>             >]
        [boolean?      boolean?]
        [eq?           eq?]
        [fixnum?       fixnum?]
        [null?         null?]
        [pair?         pair?]
        [procedure?    procedure?]
        [vector?       vector?]
        [symbol?       symbol?]
        [,x            #f])))
  (define complex?
    (lambda (im)
      (not (or (null? im)
               (symbol? im)
               (integer? im)
               (boolean? im)))))
  (define merge-value
    (lambda (v1 v2)
      (cond [(null? v1) '()]
            [(null? v2) '()]
            [(eq? (car v1) (car v2)) v1]
            [else '()])))
  (define Expr
    (lambda (env)
      (lambda (e)
        (match e
          [(if ,[(Expr env) -> cd-e cd-v] ,[(Expr env) -> cq-e cq-v] ,[(Expr env) -> at-e at-v])
           (cond [(null? cd-v) (values `(if ,cd-e ,cq-e ,at-e)
                                 (merge-value cq-v at-v))]
                 [(car cd-v) (values cq-e cq-v)]
                 [else (values at-e at-v)])]
          [(begin ,[(Expr env) -> e* v*] ... ,[(Expr env) -> e v])
           (values (make-begin `(,e* ... ,e)) v)]
          [(let ([,uvar* ,[(Expr env) -> e* v*]] ...)
             (assigned (,a* ...) ,body))
           (let ([env^ (append
                         (filter (lambda (x) (and (not (memq (car x) a*))
                                                  (not (null? (cdr x)))
                                                  (not (complex? (cadr x)))))
                           (map cons uvar* v*))
                         env)])
             (let-values ([(e v) ((Expr env^) body)])
               (values
                 `(let ([,uvar* ,e*] ...)
                    (assigned (,a* ...) ,e))
                 v)))]
          [(letrec ([,uvar* ,[(Expr env) -> e* v*]] ...) ,[(Expr env) -> e v])
           (values `(letrec ([,uvar* ,e*] ...) ,e) v)]
          [(set! ,uvar ,[(Expr env) -> expr value])
           (values `(set! ,uvar ,expr) (list (void)))]
          [(lambda (,uvar* ...) (assigned (,a* ...) ,[(Expr env) -> body _]))
           (values `(lambda (,uvar* ...) (assigned (,a* ...) ,body)) '())]
          [(,prim ,[(Expr env) -> rand* v*] ...) (guard (primitive? prim))
           (cond [(and (not (exists null? v*))
                       (primitive->datum prim)) =>
                  (lambda (d)
                    (let ([v (apply d (map car v*))])
                      (cond [(overflow? v) (values `(,prim ,rand* ...) '())]
                            [(complex? v) (values `(,prim ,rand* ...) (list v))]
                            [else (values `(quote ,v) (list v))])))]
                 [else (values `(,prim ,rand* ...) '())])]
          [(call/cc ,[(Expr env) -> expr _]) (values `(call/cc ,expr) '())]
          [(quote ,imm) (values `(quote ,imm) (list imm))]
          [(,[(Expr env) -> proc _] ,[(Expr env) -> arg* _*] ...)
           (values `(,proc ,arg* ...) '())]
          [,x (guard (uvar? x))
            (cond [(assq x env) =>
                   (lambda (p)
                     (let ([v (cdr p)])
                       (values `(quote ,(car v)) v)))]
                  [else (values x '())])]))))
  (lambda (p)
    (if *cp-1-enabled*
        (match p
          [,[(Expr '()) -> e v] e])
        p)))

(define-who optimize-useless
  (define Expr
    (lambda (ctx)
      (lambda (expr)
        (match expr
          [(if ,[(Expr 'value) -> cond u1] ,[conseq u2] ,[alter u3])
           (values `(if ,cond ,conseq ,alter) (union u1 u2 u3))]
          [(begin ,[(Expr 'effect) -> e* u*] ... ,[e u])
           (values
             (make-begin `(,e* ... ,e))
             (apply union u u*))]
          [(let ,bd*
             (assigned (,as* ...) ,[body u]))
           (let-values ([(bd-used* bd-unused*)
                         (partition (lambda (bd) (memq (car bd) u)) bd*)])
             (match (cons bd-used* bd-unused*)
               [(([,x* ,[(Expr 'value) -> expr* u1*]] ...) .
                 ([,y* ,[(Expr 'effect) -> effect* u2*]] ...))
                (values
                  (make-begin
                    `(,@effect*
                       ,(make-let-assigned `([,x* ,expr*] ...)
                          (intersection x* as*) body)))
                  (difference (union u (apply union u1*) (apply union u2*)) x*))]))]
          [(letrec ([,x* ,[(Expr 'value) -> expr* u*]] ...) ,[body u])
           (let ([bd* (map list x* expr* u*)])
             (let-values ([(usbd* u^)
                           (let partition ([uncertain bd*] [useful '()] [used u])
                             (let loop ([bd* uncertain] [useful useful] [uncertain '()] [used used]
                                        [same #t])
                               (if (null? bd*)
                                   (if same
                                       (values useful used)
                                       (partition uncertain useful used))
                                   (if (memq (caar bd*) used)
                                       (loop (cdr bd*) (cons (car bd*) useful) uncertain
                                         (union (caddar bd*) used) #f)
                                       (loop (cdr bd*) useful (cons (car bd*) uncertain)
                                         used same)))))])
               (values
                 (make-letrec (map (lambda (bd) (list (car bd) (cadr bd))) usbd*)
                   body)
                 (difference u^ x*))))]
          [(set! ,x ,[(Expr 'value) -> e u])
           (values
             `(set! ,x ,e)
             (set-cons x u))]
          [(lambda (,x* ...) (assigned (,as* ...) ,[(Expr 'value) -> body u]))
           (if (eq? ctx 'value)
               (values
                 `(lambda (,x* ...) (assigned (,as* ...) ,body))
                 (difference u x*))
               (values '(void) '()))]
          [(,prim ,[(Expr 'effect) -> rand* u*] ...)
           (guard (primitive? prim)
             (not (effect-primitive? prim))
             (eq? ctx 'effect))
           (values (if (null? rand*) '(void)
                       (make-begin rand*))
             (apply union u*))]
          [(,prim ,[(Expr 'value) -> rand* u*] ...) (guard (primitive? prim))
           (values `(,prim ,rand* ...) (apply union u*))]
          [(call/cc ,[(Expr 'value) -> expr u])
           (values `(call/cc ,expr) u)]
          [(quote ,imm)
           (if (eq? ctx 'value)
               (values `(quote ,imm) '())
               (values '(void) '()))]
          [(,[(Expr 'value) -> proc u] ,[(Expr 'value) -> arg* u*] ...)
           (values `(,proc ,arg* ...) (apply union u u*))]
          [,x (guard (uvar? x))
            (if (eq? ctx 'value)
                (values x (list x))
                (values '(void) '()))]))))
  (lambda (prog)
    (if *cp-1-enabled*
        (match prog
          [,[(Expr 'value) -> e u] e])
        prog)))

(define-who uncover-outmost
  (define non-capture?
    (lambda (e)
      (match e
        [(if ,cond ,conseq ,alter)
         (and (non-capture? cond)
              (non-capture? conseq)
              (non-capture? alter))]
        [(begin ,expr* ... ,expr)
         (andmap non-capture? (cons expr expr*))]
        [(let ([,uvar* ,expr*] ...) (assigned (,as* ...) ,expr))
         (andmap non-capture? (cons expr expr*))]
        [(letrec ([,uvar* ,expr*] ...) ,expr)
         (andmap non-capture? (cons expr expr*))]
        [(set! ,uvar ,expr) (non-capture? expr)]
        [(lambda (,uvar* ...) ,expr) #t]
        [(,prim ,rand* ...) (guard (primitive? prim))
         (for-all non-capture? rand*)]
        [(call/cc ,expr) #f]
        [(quote ,imm) #t]
        [(,proc ,arg* ...) #f]
        [,x (guard (uvar? x)) #t])))
  (define Expr
    (lambda (e)
      (match e
        [(if ,cond ,conseq ,alter)
         (if (non-capture? cond)
             (append (Expr cond) (Expr conseq) (Expr alter))
             (Expr cond))]
        [(begin ,e+ ...)
         (let loop ([e* e+])
           (cond [(null? e*) '()]
                 [(non-capture? (car e*))
                  (append (Expr (car e*)) (loop (cdr e*)))]
                 [else (Expr (car e*))]))]
        [(let ([,uvar* ,e*] ...) (assigned (,as* ...) ,e))
         (if (for-all non-capture? e*)
             (apply append (Expr e)
               (map (lambda (uvar e) (if (lambda? e) (Expr e) (cons uvar (Expr e)))) uvar* e*)) ; just a temporary solution
             '())]
        [(letrec ([,uvar* ,e*] ...) ,[o])
         (if (or (eq? *standard* 'r6rs) (for-all non-capture? e*))
             (apply append o
               (map (lambda (uvar e) (if (lambda? e) (Expr e) (cons uvar (Expr e)))) uvar* e*))
             (map car
               (filter (lambda (bd) (not (lambda? (cdr bd))))
                 `([,uvar* . ,e*] ...))))]
        [(set! ,uvar ,[o]) o]
        [(lambda (,uvar* ...) ,body) '()]
        [(,prim ,o* ...) (guard (primitive? prim)) '()]
        [(call/cc ,[o]) o]
        [(quote ,imm) '()]
        [(,o ,o* ...) '()]
        [,x (guard (uvar? x)) '()])))
  (lambda (p)
    `(outmost ,(if *optimize-global-enabled* (Expr p) '()) ,p)))

(define-who convert-assignments
  (define global-label)
  (define Expr
    (lambda (assigned outmost)
      (lambda (e)
        (match e
          [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
          [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
          [(let ([,uvar* ,[expr*]] ...) (assigned (,as* ...) ,expr))
           (let* ([global* (intersection as* outmost)]
                  [lab* (map (lambda (uv) (unique-label (string->symbol (extract-root uv)))) global*)]
                  [box* (difference as* outmost)]
                  [new-t* (map unique-name box*)]
                  [correspond (map cons box* new-t*)]
                  [subst (lambda (x) (cond [(assq x correspond) => cdr]
                                           [else x]))]
                  [new-bind* (map subst uvar*)])
             (set! global-label (append `([,global* . ,lab*] ...) global-label))
             (make-let `([,new-bind* ,expr*] ...)
               (make-let `([,box* (cons ,new-t* (void))] ...)
                 (make-begin
                   `((global-set! ,lab* ,global*) ...
                     ,((Expr (append as* assigned) outmost) expr))))))]
          [(letrec ([,uvar* ,[expr*]] ...) ,[expr]) `(letrec ([,uvar* ,expr*] ...) ,expr)]
          [(set! ,uvar ,[expr])
           (cond [(assq uvar global-label) => (lambda (l) `(global-set! ,(cdr l) ,expr))]
                 [(memq uvar assigned) `(set-car! ,uvar ,expr)]
                 [else `(set! ,uvar ,expr)])]
          [(lambda (,uvar* ...) (assigned (,as* ...) ,expr))
           (let* ([new-t* (map unique-name as*)]
                  [correspond (map cons as* new-t*)]
                  [subst (lambda (x) (cond [(assq x correspond) => cdr]
                                           [else x]))]
                  [new-bind* (map subst uvar*)])
             `(lambda (,new-bind* ...)
                ,(make-let `([,as* (cons ,new-t* (void))] ...)
                   ((Expr (append as* assigned) outmost) expr))))]
          [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
          [(call/cc ,[expr]) `(call/cc ,expr)]
          [(quote ,imm) `(quote ,imm)]
          [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
          [,x (guard (uvar? x))
            (cond [(assq x global-label) => (lambda (l) `(global-ref ,(cdr l)))]
                  [(memq x assigned) `(car ,x)]
                  [else x])]))))
  (lambda (p)
    (set! global-label '())
    (match p
      [(outmost ,out ,[(Expr '() out) -> e])
       `(with-global-data ,(map cdr global-label)
          (outmost ,out ,e))])))

(define-who convert-complex-datum
  (define size
    (lambda (d)
      (cond [(pair? d) (add1 (+ (size (car d)) (size (cdr d))))]
            [(vector? d) (add1 (apply + (vector->list (vector-map size d))))]
            [else 1])))
  (define bindings)
  (define fillings)
  (define do-conversion
    (lambda (const)
      (cond
        [(and (pair? const))
         `(cons
            ,(do-conversion (car const))
            ,(do-conversion (cdr const)))]
        [(vector? const)
         (let* ([tmp (unique-name 'tmp)]
                [length (vector-length const)]
                [fill (let loop ([i 0])
                        (cond [(< i length)
                               (cons
                                 `(vector-set! ,tmp (quote ,i) ,(do-conversion (vector-ref const i)))
                                 (loop (add1 i)))]
                              [else '()]))])
           `(let ([,tmp (make-vector (quote ,length))])
              ,(make-begin `(,@fill ,tmp))))]
        [else `(quote ,const)])))
  (define remove-last
    (lambda (l)
      (cond [(null? (cdr l)) '()]
            [else (cons (car l) (remove-last (cdr l)))])))
  (define Expr
    (lambda (e)
      (match e
        [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
        [(let ([,uvar* ,[body*]] ...) ,[body]) `(let ([,uvar* ,body*] ...) ,body)]
        [(letrec ([,uvar* ,[body*]] ...) ,[body]) `(letrec ([,uvar* ,body*] ...) ,body)]
        [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
        [(lambda (,uvar* ...) ,[body]) `(lambda (,uvar* ...) ,body)]
        [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
        [(call/cc ,[expr]) `(call/cc ,expr)]
        [(quote ,imm)
         (if (and (or (pair? imm) (vector? imm))
                  (or (not *max-inline-literal-size*) (< (size imm) *max-inline-literal-size*)))
             (let ([imm^ (do-conversion imm)])
               (cond [(and (pair? imm))
                      (let ([tmp (unique-name 'tmp)])
                        (set! bindings (cons `(,tmp ,imm^) bindings))
                        tmp)]
                     [(vector? imm)
                      (set! bindings (cons (caadr imm^) bindings))
                      (set! fillings (append (remove-last (cdaddr imm^)) fillings))
                      (caaadr imm^)]))
             `(quote ,imm))]
        [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
        [,x (guard (uvar? x)) x]
        [,lab (guard (label? lab)) lab])))
  (lambda (p)
    (set! bindings '())
    (set! fillings '())
    (match p
      [(with-global-data ,data
         (outmost ,out ,[Expr -> e]))
       `(with-global-data ,data
          (outmost ,out
            ,(make-let bindings
               (make-begin `(,fillings ... ,e)))))])))

(define-who remove-anonymous-lambda
  (define let-rhs
    (lambda (rhs)
      (match rhs
        [(lambda (,uvar* ...) ,[Expr -> body])
         `(lambda (,uvar* ...) ,body)]
        [,[Expr -> e] e])))
  (define Expr
    (lambda (e)
      (match e
        [(if ,[Expr -> cond] ,[Expr -> conseq] ,[Expr -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Expr -> expr*] ... ,[Expr -> expr]) `(begin ,expr* ... ,expr)]
        [(let ([,uvar* ,[let-rhs -> body*]] ...) ,[Expr -> body]) `(let ([,uvar* ,body*] ...) ,body)]
        [(letrec ([,uvar* (lambda (,formal** ...) ,[Expr -> body*])] ...) ,[Expr -> body])
         `(letrec ([,uvar* (lambda (,formal** ...) ,body*)] ...) ,body)]
        [(lambda (,uvar* ...) ,[Expr -> body])
         (let ([anon (unique-name 'anon)])
           `(letrec ([,anon (lambda (,uvar* ...) ,body)])
              ,anon))]
        [(,prim ,[Expr -> rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
        [(call/cc ,[Expr -> expr]) `(call/cc ,expr)]
        [(quote ,imm) `(quote ,imm)]
        [(,[Expr -> proc] ,[Expr -> arg*] ...) `(,proc ,arg* ...)]
        [,x x])))
  (lambda (p)
    (match p
      [(with-global-data ,data
         (outmost ,out ,[Expr -> e]))
       `(with-global-data ,data
          (outmost ,out ,e))])))

(define sanitize-binding-forms
  (lambda (e)
    (match e
      [(with-global-data ,data (outmost ,out ,[e])) `(with-global-data ,data (outmost ,out ,e))]
      [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
      [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
      [(let ([,uvar* ,[body*]] ...) ,[body])
       (let* ([bindings `([,uvar* ,body*] ...)])
         (let-values ([(lambdas others)
                       (partition (lambda (x) (lambda? (cadr x))) bindings)])
           (make-letrec lambdas
             (make-let others body))))]
      [(letrec ([,uvar* (lambda (,formal** ...) ,[body*])] ...) ,[body])
       `(letrec ([,uvar* (lambda (,formal** ...) ,body*)] ...) ,body)]
      [(lambda (,uvar* ...) ,[body]) `(lambda (,uvar* ...) ,body)]
      [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
      [(call/cc ,[expr]) `(call/cc ,expr)]
      [(quote ,imm) `(quote ,imm)]
      [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
      [,x x])))

(define-who uncover-free
  (define Expr
    (lambda (e)
      (match e
        [(if ,[Expr -> cond f1] ,[Expr -> conseq f2] ,[Expr -> alter f3])
         (values `(if ,cond ,conseq ,alter) (union f1 f2 f3))]
        [(begin ,[Expr -> expr* f*] ... ,[Expr -> expr f])
         (values `(begin ,expr* ... ,expr) (apply union f f*))]
        [(let ([,uvar* ,[Expr -> body* f*]] ...) ,[Expr -> body f])
         (values `(let ([,uvar* ,body*] ...) ,body) (union (difference f uvar*) (apply union f*)))]
        [(letrec ([,uvar* (lambda (,formal** ...) ,[Expr -> body* f*])] ...) ,[Expr -> body f])
         (let ([f*^ (map difference f* formal**)])
           (values
             `(letrec ([,uvar* (lambda (,formal** ...) (free ,f*^ ,body*))] ...) ,body)
             (difference (apply union f f*^) uvar*)))]
        [(,prim ,[Expr -> rand* f*] ...) (guard (primitive? prim))
         (values `(,prim ,rand* ...) (apply union f*))]
        [(call/cc ,[Expr -> expr f])
         (values `(call/cc ,expr) f)]
        [(quote ,imm) (values e '())]
        [(,[Expr -> proc f] ,[Expr -> arg* f*] ...)
         (values `(,proc ,arg* ...) (apply union f f*))]
        [,x (guard (uvar? x)) (values x (list x))]
        [,lab (guard (label? lab)) (values lab '())])))
  (lambda (p)
    (match p
      [(with-global-data ,data (outmost ,out ,[Expr -> e fv]))
       `(with-global-data ,data (outmost ,out ,e))])))

(define-who convert-closures
  (define global-label*)
  (define Expr
    (lambda (outmost)
      (lambda (e)
        (match e
          [(if ,[(Expr outmost) -> cond] ,[(Expr outmost) -> conseq] ,[(Expr outmost) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Expr outmost) -> expr*] ... ,[(Expr outmost) -> expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,uvar* ,[(Expr outmost) -> body*]] ...) ,[(Expr outmost) -> body])
           (let* ([global* (intersection uvar* (map car global-label*))]
                  [lab* (map (lambda (v) (cdr (assq v global-label*))) global*)]) ; slow
             `(let ([,uvar* ,body*] ...)
                ,(make-begin `((global-set! ,lab* ,global*) ...
                               ,body))))]
          [(letrec ([,uvar* (lambda (,formal** ...)
                              (free (,fv** ...) ,body*))] ...)
             ,[(Expr outmost) -> body])
           (let* ([fv**^ (map (lambda (fv*) (difference fv* outmost)) fv**)]
                  [new-global* (difference (intersection outmost (apply append fv**)) (map car global-label*))]
                  [new-lab* (map (lambda (uv) (unique-label (string->symbol (extract-root uv)))) new-global*)]
                  [cp* (map (lambda (x) (unique-name 'cp)) uvar*)]
                  [lab* (map unique-label uvar*)])
             (set! global-label* (append `([,new-global* . ,new-lab*] ...) global-label*))
             `(letrec ([,lab* (lambda (,cp* ,formal** ...)
                                (bind-free (,cp* ,fv**^ ...) ,(map (Expr outmost) body*)))] ...)
                (closures ([,uvar* ,lab* ,fv**^ ...] ...) ,body)))]
          [(,prim ,[(Expr outmost) -> rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(call/cc ,[(Expr outmost) -> expr]) `(call/cc ,expr)]
          [(quote ,imm) e]
          [(,[(Expr outmost) -> proc] ,[(Expr outmost) -> arg*] ...)
           (if (uvar? proc)
               `(,proc ,proc ,arg* ...)
               (let ([tmp (unique-name 'tmp)])
                 `(let ([,tmp ,proc])
                    (,tmp ,tmp ,arg* ...))))]
          [,x (guard (uvar? x))
            (cond [(assq x global-label*) => (lambda (l) `(global-ref ,(cdr l)))]
                  [else x])]
          [,lab (guard (label? lab)) lab]))))
  (lambda (p)
    (set! global-label* '())
    (match p
      [(with-global-data (,lab* ...)
         (outmost ,out ,[(Expr out) -> e]))
       `(with-global-data (,@(map cdr global-label*) ,lab* ...)
          (outmost ,out ,e))])))

(define-who optimize-known-call
  (define Expr
    (lambda (known)
      (lambda (e)
        (match e
          [(if ,[(Expr known) -> cond] ,[(Expr known) -> conseq] ,[(Expr known) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Expr known) -> expr*] ... ,[(Expr known) -> expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,uvar* ,[(Expr known) -> body*]] ...) ,[(Expr known) -> body])
           `(let ([,uvar* ,body*] ...) ,body)]
          [(letrec ([,lab* (lambda (,formal** ...)
                             (bind-free (,cp* ,fv** ...) ,body*))] ...)
             (closures ([,uvar* ,data** ...] ...) ,body))
           (let* ([known^ (append uvar* known)]
                  [body*^ (map (Expr known^) body*)])
             `(letrec ([,lab* (lambda (,formal** ...)
                                (bind-free (,cp* ,fv** ...) ,body*^))] ...)
                (closures ([,uvar* ,data** ...] ...) ,((Expr known^) body))))]
          [(,prim ,[(Expr known) -> rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(call/cc ,[(Expr known) -> expr])
           `(call/cc ,expr)]
          [(quote ,imm) e]
          [(,proc ,[(Expr known) -> arg*] ...)
           (if (memq proc known)
               `(,(unique-label proc) ,arg* ...)
               `(,proc ,arg* ...))]
          [,x (guard (uvar? x)) x]
          [,lab (guard (label? lab)) lab]))))
  (lambda (p)
    (if *closure-optimization-enabled*
        (match p
          [(with-global-data ,data
             (outmost ,out ,[(Expr '()) -> e]))
           `(with-global-data ,data
              (outmost ,out ,e))])
        p)))

(define-who uncover-well-known
  (define Expr
    (lambda (e)
      (match e
        [(if ,[cond u1] ,[conseq u2] ,[alter u3])
         (values `(if ,cond ,conseq ,alter) (union u1 u2 u3))]
        [(begin ,[e* u*] ... ,[e u])
         (values `(begin ,e* ... ,e) (apply union u u*))]
        [(let ([,x* ,[e* u*]] ...) ,[body u])
         (values `(let ([,x* ,e*] ...) ,body) (apply union u u*))]
        [(letrec ([,lab* (lambda (,fml** ...)
                           (bind-free (,cp* ,fv** ...) ,[body* u*]))] ...)
           (closures ([,f* ,data** ...] ...) ,[body u]))
         (let ([u^ (apply union u u*)])
           (values
             `(letrec ([,lab* (lambda (,fml** ...)
                                (bind-free (,cp* ,fv** ...) ,body*))] ...)
                (closures ([,f* ,data** ...] ...)
                  (well-known ,(difference f* u^) ,body)))
             (values (difference u^ f*))))]
        [(,prim ,[rand* u*] ...) (guard (primitive? prim))
         (values `(,prim ,rand* ...) (apply union u*))]
        [(call/cc ,[expr u]) (values `(call/cc ,expr) u)]
        [(quote ,imm) (values `(quote ,imm) '())]
        [(,[proc u] ,cp ,[arg* u*] ...)
         (values `(,proc ,cp ,arg* ...) (apply union u u*))]
        [,x (guard (uvar? x)) (values x (list x))]
        [,lab (guard (label? lab)) (values lab '())])))
  (lambda (p)
    (if *closure-optimization-enabled*
        (match p
          [(with-global-data ,data (outmost ,out ,[Expr -> e u]))
           `(with-global-data ,data (outmost ,out ,e))])
        p)))

(define-who optimize-free
  (define partition
    (lambda (wk-cls esc-cls wk-uvar)
      (let loop ([c* wk-cls] [wk-c '()] [esc-c esc-cls] [wk-v wk-uvar] [same #t])
        (if (null? c*)
            (if same
                (values wk-c esc-c wk-v)
                (partition wk-c esc-c wk-v))
            (let ([cls (car c*)])
              (cond [(not (memq (car cls) wk-v))
                     (loop (cdr c*) wk-c (cons cls esc-c) wk-v #f)]
                    [(not (for-all (lambda (fv) (memq fv wk-v)) (cddr cls)))
                     (loop (cdr c*) wk-c (cons cls esc-c) (remq (car cls) wk-v) #f)]
                    [else (loop (cdr c*) (cons cls wk-c) esc-c wk-v same)]))))))
  (define Expr
    (lambda (wk-uvar wk-lab)
      (lambda (e)
        (match e
          [(letrec ([,lab* ,lam*] ...)
             (closures (,cls* ...)
               (well-known (,wk* ...) ,body)))
           (let*-values ([(wk-cls* oth-cls*0 wk-uvar^)
                          (partition cls* '() (append wk* wk-uvar))]
                         [(wk-lab^)
                          (append (map (lambda (cls) (cadr cls)) wk-cls*)
                            wk-lab)]
                         [(oth-cls*)
                          (map (lambda (cls)
                                 (cons (car cls)
                                   (cons (cadr cls)
                                     (difference (cddr cls) wk-uvar^))))
                            oth-cls*0)]
                         [(lam*^) (map (Lambda wk-uvar^ wk-lab^) lab* lam*)])
             `(letrec ([,lab* ,lam*^] ...)
                (closures (,oth-cls* ...)
                  ,((Expr wk-uvar^ wk-lab^) body))))]
          [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
          [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
          [(let ([,x* ,[e*]] ...) ,[body]) `(let ([,x* ,e*] ...) ,body)]
          [(,prim ,[rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(call/cc ,[expr]) `(call/cc ,expr)]
          [(quote ,imm) `(quote ,imm)]
          [(,[proc] ,cp ,[arg*] ...)
           (if (memq proc wk-lab)
               `(,proc ,arg* ...)
               `(,proc ,cp ,arg* ...))]
          [,x (guard (uvar? x)) x]
          [,lab (guard (label? lab)) lab]))))
  (define Lambda
    (lambda (wk-uvar wk-lab)
      (lambda (lab lam)
        (match lam
          [(lambda (,cp ,fml* ...)
             (bind-free (,cp ,fv* ...) ,body))
           (let ([fml*^ (if (memq lab wk-lab)
                            fml*
                            (cons cp fml*))]
                 [bd* (if (memq lab wk-lab)
                          '(dummy)
                          (cons cp
                            (difference fv* wk-uvar)))])
             `(lambda (,fml*^ ...)
                (bind-free (,bd* ...)
                  ,((Expr wk-uvar wk-lab) body))))]))))
  (lambda (p)
    (if *closure-optimization-enabled*
        (match p
          [(with-global-data ,data
             (outmost ,out ,[(Expr '() '()) -> e]))
           `(with-global-data ,data
              (outmost ,out ,e))])
        p)))

(define-who optimize-self-reference
  (define Expr
    (lambda (self cp)
      (lambda (expr)
        (match expr
          [(letrec ([,lab* ,lam*] ...)
             (closures ([,f* ,code* ,[fv**] ...] ...)
               ,[body]))
           (let* ([lab->self `((,code* . ,f*) ...)]
                  [fv**^ (map (lambda (f fv*) (remq f fv*)) f* fv**)]
                  [lam*^ (map (lambda (lab lam)
                                (cond [(assq lab lab->self) =>
                                       (lambda (self) ((Lambda (cdr self)) lam))]
                                      [else ((Lambda #f) lam)]))
                           lab* lam*)])
             `(letrec ([,lab* ,lam*^] ...)
                (closures ([,f* ,code* ,fv**^ ...] ...)
                  ,body)))]
          [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
          [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
          [(let ([,x* ,[e*]] ...) ,[body]) `(let ([,x* ,e*] ...) ,body)]
          [(,prim ,[rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(call/cc ,[expr]) `(call/cc ,expr)]
          [(quote ,imm) `(quote ,imm)]
          [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
          [,x (guard (uvar? x))
            (if (eq? x self) cp x)]
          [,lab (guard (label? lab)) lab]))))
  (define Lambda
    (lambda (self)
      (lambda (lam)
        (match lam
          [(lambda (,fml* ...)
             (bind-free (dummy)
               ,[(Expr #f #f) -> body]))
           `(lambda (,fml* ...)
              (bind-free (dummy)
                ,body))]
          [(lambda (,cp ,fml* ...)
             (bind-free (,cp ,fv* ...) ,body))
           `(lambda (,cp ,fml* ...)
              (bind-free (,cp ,@(remq self fv*))
                ,((Expr self cp) body)))]))))
  (lambda (p)
    (if *closure-optimization-enabled*
        (match p
          [(with-global-data ,data
             (outmost ,out ,[(Expr #f #f) -> e]))
           `(with-global-data ,data
              (outmost ,out ,e))])
        p)))

(define-who introduce-procedure-primitives
  (define closure-length) ; actually free-variable length
  (define index-of
    (lambda (x fvs)
      (let loop ([fv fvs] [i 0])
        (cond [(null? fv) #f]
              [(eq? (car fv) x) i]
              [else (loop (cdr fv) (add1 i))]))))
  (define Lambda
    (lambda (l)
      (match l
        [(lambda (,formal* ...)
           (bind-free (,cp ,fv* ...) ,body))
         `(lambda (,formal* ...) ,((Expr cp fv*) body))])))
  (define Closure
    (lambda (cp fvs)
      (lambda (c)
        (match c
          [(,uvar ,lab ,[(Expr cp fvs) -> fv*] ...)
           (set! closure-length (cons (list lab (ash (length fv*) align-shift)) closure-length))
           (let ([set-free (let loop ([fv fv*] [i 0])
                             (cond [(null? fv) '()]
                                   [else (cons `(procedure-set! ,uvar (quote ,i) ,(car fv))
                                           (loop (cdr fv) (add1 i)))]))])
             (values `(,uvar (make-procedure ,lab (quote ,(length fv*))))
               (if (null? set-free) '(void) `(begin ,@set-free))))]))))
  (define Expr
    (lambda (cp fvs)
      (lambda (e)
        (match e
          [(if ,[(Expr cp fvs) -> cond] ,[(Expr cp fvs) -> conseq] ,[(Expr cp fvs) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Expr cp fvs) -> expr*] ... ,[(Expr cp fvs) -> expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,uvar* ,[(Expr cp fvs) -> body*]] ...) ,[(Expr cp fvs) -> body])
           `(let ([,uvar* ,body*] ...) ,body)]
          [(letrec ([,lab* ,[Lambda -> lambda*]] ...)
             (closures (,[(Closure cp fvs) -> bind* fill*] ...) ,[(Expr cp fvs) -> body]))
           `(letrec ([,lab* ,lambda*] ...)
              ,(make-let bind*
                 (make-begin `(,fill* ... ,body))))]
          [(,prim ,[(Expr cp fvs) -> rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(call/cc ,[(Expr cp fvs) -> expr]) `(call/cc ,expr)]
          [(quote ,imm) e]
          [(outmost ,out ,[(Expr cp fvs) -> e]) `(outmost ,out ,e)]
          [(,[(Expr cp fvs) -> proc] ,[(Expr cp fvs) -> arg*] ...)
           (if (label? proc)            ; in the presence of optimize-known-call
               `(,proc ,arg* ...)
               `((procedure-code ,proc) ,arg* ...))]
          [,x (guard (uvar? x))
            (cond [(index-of x fvs) =>
                   (lambda (i) `(procedure-ref ,cp (quote ,i)))]
                  [else x])]
          [,lab (guard (label? lab)) lab]))))
  (lambda (x)
    (set! closure-length '())
    (match x
      [(with-global-data ,data
         (outmost ,out ,[(Expr #f '()) -> e]))
       `(with-closure-length ,(if *collection-enabled* closure-length '())
          (with-global-data ,data
            (outmost ,out ,e)))])))

(define-who lift-letrec
  (define Expr
    (lambda (e)
      (match e
        [(if ,[Expr -> cond b1] ,[Expr -> conseq b2] ,[Expr -> alter b3])
         (values `(if ,cond ,conseq ,alter) (append b1 b2 b3))]
        [(begin ,[Expr -> expr* b*] ... ,[Expr -> expr b])
         (values `(begin ,expr* ... ,expr) (apply append b b*))]
        [(let ([,uvar* ,[Expr -> expr* b*]] ...) ,[Expr -> expr b])
         (values `(let ([,uvar* ,expr*] ...) ,expr) (apply append b b*))]
        [(letrec ([,lab* (lambda ,uvars* ,[Expr -> body* b*])] ...) ,[Expr -> body b])
         (values body (apply append b (map cons `([,lab* (lambda ,uvars* ,body*)] ...) b*)))]
        [(,prim ,[Expr -> rand* b*] ...) (guard (primitive? prim))
         (values `(,prim ,rand* ...) (apply append b*))]
        [(call/cc ,[Expr -> expr b]) (values `(call/cc ,expr) b)]
        [(quote ,imm) (values `(quote ,imm) '())]
        [(,[Expr -> proc b] ,[Expr -> arg* b*] ...)
         (values `(,proc ,arg* ...) (apply append b b*))]
        [,x (values x '())])))
  (lambda (p)
    (match p
      [(with-closure-length ,closure-length
         (with-global-data ,data
           (outmost ,out ,[Expr -> body binds])))
       `(with-closure-length ,closure-length
          (with-global-data ,data
            (outmost ,out
              (letrec ,binds ,body))))])))

(define-who normalize-context
  (define Value
    (lambda (e)
      (match e
        [(quote ,immediate) e]
        [(if ,[Pred -> cond] ,[Value -> conseq] ,[Value -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Value -> value])
         (make-begin `(,effect* ... ,value))]
        [(let ([,uvar* ,[Value -> body*]] ...) ,[Value -> body])
         `(let ([,uvar* ,body*] ...) ,body)]
        [(,prim ,[Value -> rand*] ...) (guard (value-primitive? prim))
         `(,prim ,rand* ...)]
        [(,prim ,[Value -> rand*] ...) (guard (predicate-primitive? prim))
         `(if (,prim ,rand* ...) '#t '#f)]
        [(,prim ,[Value -> rand*] ...) (guard (effect-primitive? prim))
         `(begin (,prim ,rand* ...) (void))]
        [(call/cc ,[Value -> expr]) `(call/cc ,expr)]
        [(,[Value -> proc] ,[Value -> arg*] ...)
         `(,proc ,arg* ...)]
        [,lab (guard (label? lab)) lab]
        [,uvar (guard (uvar? uvar)) uvar]
        [,x (format-error who "invalid expression ~s in value context" x)])))
  (define Pred
    (lambda (p)
      (match p
        [(quote ,immediate) (if (not immediate) '(false) '(true))]
        [(if ,[Pred -> cond] ,[Pred -> conseq] ,[Pred -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred])
         (make-begin `(,effect* ... ,pred))]
        [(let ([,uvar* ,[Value -> body*]] ...) ,[Pred -> body])
         `(let ([,uvar* ,body*] ...) ,body)]
        [(,prim ,[Value -> rand*] ...) (guard (value-primitive? prim))
         `(if (eq? (,prim ,rand* ...) '#f) (false) (true))]
        [(,prim ,[Value -> rand*] ...) (guard (predicate-primitive? prim))
         `(,prim ,rand* ...)]
        [(,prim ,[Value -> rand*] ...) (guard (effect-primitive? prim))
         `(begin (,prim ,rand* ...) (true))]
        [(call/cc ,[Value -> expr])
         `(if (eq? (call/cc ,expr) '#f) (false) (true))]
        [(,[Value -> proc] ,[Value -> arg*] ...)
         `(if (eq? (,proc ,arg* ...) '#f) (false) (true))]
        [,lab (guard (label? lab)) '(true)]
        [,uvar (guard (uvar? uvar))
          `(if (eq? ,uvar '#f) (false) (true))]
        [,x (format-error who "invalid expression ~s in predicate context" x)])))
  (define Effect
    (lambda (e)
      (match e
        [(quote ,immediate) '(nop)]
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect])
         (make-begin `(,effect* ... ,effect))]
        [(let ([,uvar* ,[Value -> body*]] ...) ,[Effect -> body])
         `(let ([,uvar* ,body*] ...) ,body)]
        [(,prim ,[Effect -> rand*] ...) (guard (value-primitive? prim))
         (make-nopless-begin `(,rand* ...))] ; call/cc??
        [(,prim ,[Effect -> rand*] ...) (guard (predicate-primitive? prim))
         (make-nopless-begin `(,rand* ...))]
        [(,prim ,[Value -> rand*] ...) (guard (effect-primitive? prim))
         `(,prim ,rand* ...)]
        [(call/cc ,[Value -> expr]) `(call/cc ,expr)]
        [(,[Value -> proc] ,[Value -> arg*] ...)
         `(,proc ,arg* ...)]
        [,lab (guard (label? lab)) '(nop)]
        [,uvar (guard (uvar? uvar)) '(nop)]
        [,x (format-error who "invalid expression ~s in effect context" x)])))
  (lambda (p)
    (match p
      [(with-closure-length ,closure-length
         (with-global-data ,data
           (outmost ,out
             (letrec ([,lab* (lambda (,uvar* ...) ,[Value -> body*])] ...)
               ,[Value -> body]))))
       `(with-closure-length ,closure-length
          (with-global-data ,data
            (outmost ,out
              (letrec ([,lab* (lambda (,uvar* ...) ,body*)] ...)
                ,body))))])))

(define-who specify-representation
  (define call/cc-label)
  (define write-label)
  (define inspect-label)

  (define current-dump-length)
  (define symbol->index)

  (define complex-datum-label*)

  (define specify-complex
    (lambda (?complex)
      (cond [(pair? ?complex)
             (cons (specify-complex (car ?complex))
               (specify-complex (cdr ?complex)))]
            [(vector? ?complex)
             (vector-map specify-complex ?complex)]
            [else (Immediate ?complex)])))

  (define offset-car (- disp-car tag-pair))
  (define offset-cdr (- disp-cdr tag-pair))
  (define offset-vector-length (- disp-vector-length tag-vector))
  (define offset-vector-data (- disp-vector-data tag-vector))
  (define offset-procedure-code (- disp-procedure-code tag-procedure))
  (define offset-procedure-data (- disp-procedure-data tag-procedure))
  (define Value
    (lambda (v)
      (match v
        [(if ,[Pred -> cond] ,[Value -> conseq] ,[Value -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Value -> value])
         `(begin ,effect* ... ,value)]
        [(let ([,uvar* ,[Value -> value*]] ...) ,[Value -> value])
         `(let ([,uvar* ,value*] ...) ,value)]
        [(quote ,[Immediate -> imm]) imm]
        [(call/cc ,[Value -> expr]) `(,call/cc-label ,expr)]
        [(+ ,[Value -> rand1] ,[Value -> rand2]) `(+ ,rand1 ,rand2)]
        [(- ,[Value -> rand1] ,[Value -> rand2]) `(- ,rand1 ,rand2)]
        [(* ,[Value -> rand1] ,[Value -> rand2])
         (cond [(integer? rand1) `(* ,(sra rand1 shift-fixnum) ,rand2)]
               [(integer? rand2) `(* ,rand1 ,(sra rand2 shift-fixnum))]
               [else `(* ,rand1 (sra ,rand2 ,shift-fixnum))])]
        [(car ,[Value -> pair]) `(mref ,pair ,offset-car)]
        [(cdr ,[Value -> pair]) `(mref ,pair ,offset-cdr)]
        [(procedure-code ,[Value -> proc]) `(mref ,proc ,offset-procedure-code)]
        [(procedure-ref ,[Value -> proc] ,[Value -> ind])
         (if (integer? ind)
             `(mref ,proc ,(+ offset-procedure-data ind))
             `(mref ,proc (+ ,offset-procedure-data ,ind)))]
        [(vector-length ,[Value -> vec]) `(mref ,vec ,offset-vector-length)]
        [(vector-ref ,[Value -> vec] ,[Value -> ind])
         (if (integer? ind)
             `(mref ,vec ,(+ offset-vector-data ind))
             `(mref ,vec (+ ,offset-vector-data ,ind)))]
        [(global-ref ,lab) `(mref ,lab 0)]
        [(cons ,[Value -> e1] ,[Value -> e2])
         (let* ([tmp-car (unique-name 'tmp)]
                [tmp-cdr (unique-name 'tmp)]
                [tmp (unique-name 'tmp)]
                [e1^ (if (or (integer? e1) (uvar? e1)) e1 tmp-car)]
                [e2^ (if (or (integer? e2) (uvar? e2)) e2 tmp-cdr)]
                [bd1 (if (or (integer? e1) (uvar? e1)) '() `([,tmp-car ,e1]))]
                [bd2 (if (or (integer? e2) (uvar? e2)) '() `([,tmp-cdr ,e2]))])
           (make-let `(,@bd1 ,@bd2)
             `(let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                (begin (mset! ,tmp ,offset-car ,e1^)
                       (mset! ,tmp ,offset-cdr ,e2^)
                       ,tmp))))]
        [(make-procedure ,[Value -> lab] ,[Value -> e])
         (if (integer? e)
             (let ([tmp (unique-name 'tmp)])
               `(let ([,tmp (+ (alloc ,(+ disp-procedure-data e)) ,tag-procedure)]) ; what if it overflows?
                  (begin (mset! ,tmp ,offset-procedure-code ,lab)
                         ,tmp)))
             (let ([tmp1 (unique-name 'tmp)] [tmp2 (unique-name 'tmp)])
               `(let ([,tmp1 ,e])
                  (let ([,tmp2 (+ (alloc (+ ,disp-procedure-data ,tmp1)) ,tag-procedure)])
                    (begin (mset! ,tmp2 ,offset-procedure-code ,tmp1)
                           ,tmp2)))))]
        [(make-vector ,[Value -> e])
         (if (integer? e)
             (let ([tmp (unique-name 'tmp)])
               `(let ([,tmp (+ (alloc ,(+ disp-vector-data e)) ,tag-vector)])
                  (begin (mset! ,tmp ,offset-vector-length ,e)
                         ,tmp)))
             (let ([tmp1 (unique-name 'tmp)] [tmp2 (unique-name 'tmp)])
               `(let ([,tmp1 ,e])
                  (let ([,tmp2 (+ (alloc (+ ,disp-vector-data ,tmp1)) ,tag-vector)])
                    (begin (mset! ,tmp2 ,offset-vector-length ,tmp1)
                           ,tmp2)))))]
        [(void) $void]
        [(,[Value -> proc] ,[Value -> arg*] ...) `(,proc ,arg* ...)]
        [,x x])))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond] ,[Pred -> conseq] ,[Pred -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred])
         `(begin ,effect* ... ,pred)]
        [(let ([,uvar* ,[Value -> value*]] ...) ,[Pred -> pred])
         `(let ([,uvar* ,value*] ...) ,pred)]
        [(,rel ,[Value -> rand1] ,[Value -> rand2]) (guard (relop? rel))
         `(,rel ,rand1 ,rand2)]
        [(eq? ,[Value -> rand1] ,[Value -> rand2]) `(= ,rand1 ,rand2)]
        [(null? ,[Value -> e]) `(= ,e ,$nil)]
        [(boolean? ,[Value -> e]) `(= (logand ,e ,mask-boolean) ,tag-boolean)]
        [(fixnum? ,[Value -> e]) `(= (logand ,e ,mask-fixnum) ,tag-fixnum)]
        [(pair? ,[Value -> e]) `(= (logand ,e ,mask-pair) ,tag-pair)]
        [(procedure? ,[Value -> e]) `(= (logand ,e ,mask-procedure) ,tag-procedure)]
        [(vector? ,[Value -> e]) `(= (logand ,e ,mask-vector) ,tag-vector)]
        [(symbol? ,[Value -> e]) `(= (logand ,e ,mask-symbol) ,tag-symbol)]
        [,x x])))
  (define Effect
    (lambda (eff)
      (match eff
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect])
         `(begin ,effect* ... ,effect)]
        [(let ([,uvar* ,[Value -> value*]] ...) ,[Effect -> effect])
         `(let ([,uvar* ,value*] ...) ,effect)]
        [(write ,[Value -> expr]) `(,write-label ,expr)]
        [(inspect ,[Value -> expr]) `(,inspect-label ,expr)]
        [(call/cc ,[Value -> expr]) `(,call/cc-label ,expr)]
        [(global-set! ,lab ,[Value -> e]) `(mset! ,lab 0 ,e)]
        [(set-car! ,[Value -> pair] ,[Value -> e]) `(mset! ,pair ,offset-car ,e)]
        [(set-cdr! ,[Value -> pair] ,[Value -> e]) `(mset! ,pair ,offset-cdr ,e)]
        [(procedure-set! ,[Value -> proc] ,[Value -> ind] ,[Value -> e])
         (if (integer? ind)
             `(mset! ,proc ,(+ offset-procedure-data ind) ,e)
             `(mset! ,proc (+ ,offset-procedure-data ,ind) ,e))]
        [(vector-set! ,[Value -> vec] ,[Value -> ind] ,[Value -> e])
         (if (integer? ind)
             `(mset! ,vec ,(+ offset-vector-data ind) ,e)
             `(mset! ,vec (+ ,offset-vector-data ,ind) ,e))]
        [(,[Value -> proc] ,[Value -> arg*] ...) `(,proc ,arg* ...)]
        [(nop) '(nop)])))
  (define Immediate
    (lambda (i)
      (cond [(eq? i #t) $true]
            [(eq? i #f) $false]
            [(eq? i '()) $nil]
            [(symbol? i)
             (cond [(assq i symbol->index) =>
                    (lambda (index) (+ tag-symbol (ash (cdr index) shift-fixnum)))]
                   [else (let ([index current-dump-length])
                           (set! symbol->index (cons (cons i index) symbol->index))
                           (set! current-dump-length
                             (+ current-dump-length (add1 (string-length (symbol->string i)))))
                           (+ tag-symbol (ash index shift-fixnum)))])]
            [(integer? i) (ash i shift-fixnum)]
            [else
              (let ([complex (unique-label 'code)]
                    [lab (unique-label 'literal)])
                (set! complex-datum-label*
                  (cons
                    (list complex (specify-complex i) lab)
                    complex-datum-label*))
                `(mref ,lab ,0))])))
  (lambda (p)
    (set! current-dump-length 0)
    (set! symbol->index '())
    (set! complex-datum-label* '())

    (set! call/cc-label (unique-label 'call/cc))
    (set! write-label (unique-label 'write))
    (set! inspect-label (unique-label 'inspect))
    (set! decode-literal-label (unique-label 'decode-literal))

    (match p
      [(with-closure-length ,closure-length
         (with-global-data (,glab* ...)
           (outmost ,out
             (letrec ([,label* (lambda (,uvar* ...) ,[Value -> body*])] ...)
               ,[Value -> body]))))
       (match complex-datum-label*
         [([,complex* ,code* ,lab*] ...)
          (let ([l* (map (lambda (v) decode-literal-label) lab*)])
            `(with-label-alias ([,decode-literal-label "_scheme_decode_literal"]
                                [,call/cc-label "_scheme_call_with_current_continuation"]
                                [,write-label "_scheme_write"]
                                [,inspect-label "_scheme_inspect"])
               (with-global-data ([symbol-dump ,(map (lambda (x) (list 'quote (car x)))
                                                  (reverse symbol->index))]
                                  [,complex* (encode-literal (quote ,code*))] ...
                                  [roots (,lab* ... ,glab* ...)])
                 (with-closure-length ,closure-length
                   (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...)
                     (begin (mset! ,lab* 0 (,l* ,complex*)) ...
                            ,body))))))])])))

(define-who uncover-locals
  (define locals)
  (define Body
    (lambda (tail)
      (set! locals '())
      (Tail tail)
      `(locals ,locals ,tail)))
  (define Tail
    (lambda (t)
      (match t
        [(let ([,uvar* ,[Value ->]] ...) ,[Tail ->])
         (set! locals (append uvar* locals))
         (values)]
        [(begin ,[Effect ->] ... ,[Tail ->]) (values)]
        [(if ,[Pred ->] ,[Tail ->] ,[Tail ->]) (values)]
        [(alloc ,[Value ->]) (values)]
        [(,rator ,[Value ->] ,[Value ->]) (guard (or (binop? rator)
                                                     (eq? rator 'mref))) (values)]
        [(,[Value ->] ,[Value ->] ...) (values)]
        [,x (values)])))
  (define Pred
    (lambda (p)
      (match p
        [(let ([,uvar* ,[Value ->]] ...) ,[Pred ->])
         (set! locals (append uvar* locals))
         (values)]
        [(begin ,[Effect ->] ... ,[Pred ->]) (values)]
        [(if ,[Pred ->] ,[Pred ->] ,[Pred ->]) (values)]
        [(,rel ,[Value ->] ,[Value ->]) (values)]
        [,x (values)])))
  (define Effect
    (lambda (e)
      (match e
        [(let ([,uvar* ,[Value ->]] ...) ,[Effect ->])
         (set! locals (append uvar* locals))
         (values)]
        [(begin ,[Effect ->] ... ,[Effect ->]) (values)]
        [(if ,[Pred ->] ,[Effect ->] ,[Effect ->]) (values)]
        [(mset! ,[Value ->] ,[Value ->] ,[Value ->]) (values)]
        [(,[Value ->] ,[Value ->] ...) (values)]
        [(nop) (values)])))
  (define Value ; in fact here Value is identical to Tail
    (lambda (v)
      (match v
        [(let ([,uvar* ,[Value ->]] ...) ,[Value ->])
         (set! locals (append uvar* locals))
         (values)]
        [(begin ,[Effect ->] ... ,[Value ->]) (values)]
        [(if ,[Pred ->] ,[Value ->] ,[Value ->]) (values)]
        [(alloc ,[Value ->]) (values)]
        [(,rator ,[Value ->] ,[Value ->]) (guard (or (binop? rator)
                                                     (eq? rator 'mref))) (values)]
        [(,[Value ->] ,[Value ->] ...) (values)]
        [,x (values)])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body))))])))

(define-who remove-let
  (define reorder-assign
    (lambda (var-value-nuse-call?*)
      (let ([sorted (sort (lambda (x y)
                            (cond [(eq? (cadddr x) (cadddr y))
                                   (< (length (caddr x)) (length (caddr y)))]
                                  [(cadddr x) #f]
                                  [else #t])) var-value-nuse-call?*)])
        (map (lambda (x) `(set! ,(car x) ,(cadr x))) var-value-nuse-call?*))))
  (define Body
    (lambda (b)
      (match b
        [(locals ,locals ,[Tail -> tail u c]) `(locals ,locals ,tail)])))
  (define Tail
    (lambda (t)
      (match t
        [(let ([,uvar* ,[Value -> value* u* c*]] ...) ,[Tail -> tail u c])
         (values (make-begin `(,@(reorder-assign `((,uvar* ,value* ,u* ,c*) ...)) ,tail))
           (apply union u u*) (ormap id (cons c c*)))]
        [(begin ,[Effect -> effect* u* c*] ... ,[Tail -> tail u c])
         (values `(begin ,effect* ... ,tail)
           (apply union u u*) (ormap id (cons c c*)))]
        [(if ,[Pred -> cond u1 c1] ,[Tail -> conseq u2 c2] ,[Tail -> alter u3 c3])
         (values `(if ,cond ,conseq ,alter)
           (union u1 u2 u3) (or u1 u2 u3))]
        [(alloc ,[Value -> expr u c])
         (values `(alloc ,expr) u c)]
        [(,rator ,[Value -> rand1 u1 c1] ,[Value -> rand2 u2 c2]) (guard (or (binop? rator)
                                                                             (eq? rator 'mref)))
         (values `(,rator ,rand1 ,rand2)
           (union u1 u2) (or c1 c2))]
        [(,[Value -> proc u c] ,[Value -> arg* u* c*] ...)
         (values `(,proc ,arg* ...)
           (apply union u u*) (ormap id (cons c c*)))]
        [,x (values x '() #f)])))
  (define Pred
    (lambda (p)
      (match p
        [(let ([,uvar* ,[Value -> value* u* c*]] ...) ,[Pred -> pred u c])
         (values (make-begin `(,@(reorder-assign `((,uvar* ,value* ,u* ,c*) ...)) ,pred))
           (apply union u u*) (ormap id (cons c c*)))]
        [(begin ,[Effect -> effect* u* c*] ... ,[Pred -> pred u c])
         (values `(begin ,effect* ... ,pred)
           (apply union u u*) (ormap id (cons c c*)))]
        [(if ,[Pred -> cond u1 c1] ,[Pred -> conseq u2 c2] ,[Pred -> alter u3 c3])
         (values `(if ,cond ,conseq ,alter)
           (union u1 u2 u3) (or c1 c2 c3))]
        [(,rel ,[Value -> rand1 u1 c1] ,[Value -> rand2 u2 c2])
         (values `(,rel ,rand1 ,rand2)
           (union u1 u2) (or c1 c2))]
        [,x (values x '() #f)])))
  (define Effect
    (lambda (e)
      (match e
        [(let ([,uvar* ,[Value -> value* u* c*]] ...) ,[Effect -> effect u c])
         (values (make-begin `(,@(reorder-assign `((,uvar* ,value* ,u* ,c*) ...)) ,effect))
           (apply union u u*) (ormap id (cons c c*)))]
        [(begin ,[Effect -> effect* u* c*] ... ,[Effect -> effect u c])
         (values `(begin ,effect* ... ,effect)
           (apply union u u*) (ormap id (cons c c*)))]
        [(if ,[Pred -> cond u1 c1] ,[Effect -> conseq u2 c2 ] ,[Effect -> alter u3 c3])
         (values `(if ,cond ,conseq ,alter)
           (union u1 u2 u3) (or c1 c2 c3))]
        [(mset! ,[Value -> base u1 c1] ,[Value -> offset u2 c2] ,[Value -> expr u3 c3])
         (values `(mset! ,base ,offset ,expr)
           (union u1 u2 u3) (or c1 c2 c3))]
        [(,[Value -> proc u c] ,[Value -> arg* u* c*] ...)
         (values `(,proc ,arg* ...)
           (apply union u u*) #t)]
        [(nop) (values '(nop) '() #f)])))
  (define Value
    (lambda (v)
      (match v
        [(let ([,uvar* ,[Value -> value* u* c*]] ...) ,[Value -> value u c])
         (values (make-begin `(,@(reorder-assign `((,uvar* ,value* ,u* ,c*) ...)) ,value))
           (apply union u u*) (ormap id (cons c c*)))]
        [(begin ,[Effect -> effect* u* c*] ... ,[Value -> value u c])
         (values `(begin ,effect* ... ,value)
           (apply union u u*) (ormap id (cons c c*)))]
        [(if ,[Pred -> cond u1 c1] ,[Value -> conseq u2 c2] ,[Value -> alter u3 c3])
         (values `(if ,cond ,conseq ,alter)
           (union u1 u2 u3) (or c1 c2 c3))]
        [(alloc ,[Value -> expr u c])
         (values `(alloc ,expr) u c)]
        [(,rator ,[Value -> rand1 u1 c1] ,[Value -> rand2 u2 c2]) (guard (or (binop? rator)
                                                                             (eq? rator 'mref)))
         (values `(,rator ,rand1 ,rand2)
           (union u1 u2) (or c1 c2))]
        [(,[Value -> proc u c] ,[Value -> arg* u* c*] ...)
         (values `(,proc ,arg* ...)
           (apply union u u*) #t)]
        [,x (values x (if (uvar? x) (list x) '()) #f)])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body))))])))

(define verify-uil (lambda (x) x))

(define-who remove-complex-opera* ; can improve
  (define fresh-locals)
  (define introduce-local
    (lambda (x)
      (set! fresh-locals (cons x fresh-locals))))
  (define trivial?
    (lambda (v)
      (or (uvar? v) (label? v) (integer? v))))
  (define trivialize
    (lambda (rand)
      (cond [(trivial? rand) (cons '() rand)]
            [else (let ([temp (unique-name 't)])
                    (introduce-local temp)
                    (cons (list `(set! ,temp ,rand)) temp))])))
  (define remove-opera
    (lambda (rator . rand*)
      (let* ([effs-triv* (map trivialize rand*)]
             [eff* (apply append (map car effs-triv*))]
             [triv* (map cdr effs-triv*)])
        (make-begin `(,@eff* (,rator ,@triv*))))))
  (define process-procedure-call
    (lambda (rator rand*)
      (let* ([effs-triv* (map trivialize (cons rator rand*))]
             [eff* (apply append (map car effs-triv*))]
             [triv* (map cdr effs-triv*)])
        (make-begin `(,@eff* (,(car triv*) ,@(cdr triv*)))))))
  (define Body
    (lambda (b)
      (match b
        [(locals (,uvar* ...) ,tail)
         (set! fresh-locals '())
         (let ([tail^ (Tail tail)])
           `(locals ,(append uvar* fresh-locals) ,tail^))])))
  (define Tail
    (lambda (t)
      (match t
        [(if ,[Pred -> cond] ,[Tail -> conseq] ,[Tail -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Tail -> tail]) (make-begin `(,effect* ... ,tail))]
        [(,rator ,[Value -> rand1] ,[Value -> rand2]) (guard (binop? rator))
         (remove-opera rator rand1 rand2)]
        [(alloc ,[Value -> expr]) (remove-opera 'alloc expr)]
        [(mref ,[Value -> base] ,[Value -> offset])
         (remove-opera 'mref base offset)]
        [(,[Value -> rator] ,[Value -> rand*] ...) (process-procedure-call rator rand*)]
        [,x x])))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond] ,[Pred -> conseq] ,[Pred -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred]) (make-begin `(,effect* ... ,pred))]
        [(,rel ,[Value -> rand1] ,[Value -> rand2]) (remove-opera rel rand1 rand2)]
        [,x x])))
  (define Effect
    (lambda (e)
      (match e
        [(nop) (list 'nop)]
        [(mset! ,[Value -> base] ,[Value -> offset] ,[Value -> expr])
         (let* ([effs-triv* (map trivialize (list base offset))]
                [eff* (apply append (map car effs-triv*))]
                [triv* (map cdr effs-triv*)])
           (make-begin `(,@eff* (mset! ,@triv* ,expr))))]
        [(set! ,v ,[Value -> x]) `(set! ,v ,x)]
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect]) (make-begin `(,effect* ... ,effect))]
        [(,[Value -> proc] ,[Value -> arg*] ...) (process-procedure-call proc arg*)])))
  (define Value
    (lambda (v)
      (match v
        [(if ,[Pred -> cond] ,[Value -> conseq] ,[Value -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Value -> triv]) (make-begin `(,effect* ... ,triv))]
        [(,rator ,[Value -> rand1] ,[Value -> rand2]) (guard (binop? rator))
         (remove-opera rator rand1 rand2)]
        [(alloc ,[Value -> expr]) (remove-opera 'alloc expr)]
        [(mref ,[Value -> base] ,[Value -> offset])
         (remove-opera 'mref base offset)]
        [(,[Value -> proc] ,[Value -> arg*] ...) (process-procedure-call proc arg*)]
        [,x x])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body))))])))

(define-who flatten-set!
  (define do-flatten-set!
    (lambda (uvar value)
      (match value
        [(if ,pred ,conseq ,alter)
         `(if ,pred
              ,(do-flatten-set! uvar conseq)
              ,(do-flatten-set! uvar alter))]
        [(begin ,effect* ... ,value)
         (make-begin `(,effect* ... ,(do-flatten-set! uvar value)))]
        [,x `(set! ,uvar ,value)])))
  (define do-flatten-mset!
    (lambda (base offset value)
      (match value
        [(if ,pred ,conseq ,alter)
         `(if ,pred
              ,(do-flatten-mset! base offset conseq)
              ,(do-flatten-mset! base offset alter))]
        [(begin ,effect* ... ,value)
         (make-begin `(,effect* ... ,(do-flatten-mset! base offset value)))]
        [,x `(mset! ,base ,offset ,value)])))
  (define Body
    (lambda (b)
      (match b
        [(locals (,uvar* ...) ,[Tail -> tail])
         `(locals (,uvar* ...) ,tail)])))
  (define Tail
    (lambda (t)
      (match t
        [(if ,[Pred -> cond] ,[Tail -> conseq] ,[Tail -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Tail -> tail]) (make-begin `(,effect* ... ,tail))]
        [,x x])))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond] ,[Pred -> conseq] ,[Pred -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred]) (make-begin `(,effect* ... ,pred))]
        [,x x])))
  (define Effect
    (lambda (e)
      (match e
        [(nop) (list 'nop)]
        [(set! ,uvar ,[Value -> value])
         (do-flatten-set! uvar value)]
        [(mset! ,base ,offset ,[Value -> value])
         (do-flatten-mset! base offset value)]
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect]) (make-begin `(,effect* ... ,effect))]
        [,x x])))
  (define Value
    (lambda (v)
      (match v
        [(if ,[Pred -> cond] ,[Value -> conseq] ,[Value -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Value -> value]) (make-begin `(,effect* ... ,value))]
        [,x x])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body))))])))

(define-who insert-overflow-check
  (define new-local*)
  (define collect-label)
  (define wrap-with
    (lambda (expr x)
      (let ([tmp (unique-name 't)])
        (set! new-local* (cons tmp new-local*))
        (if (and (number? x) (= x 0))
            expr
            (let ([inc
                    (match x
                      [(,[e*] ...) (apply append e*)]
                      [0 '()]
                      [,x `((set! ,tmp (+ ,tmp ,x)))])])
              (make-nopless-begin
                `((set! ,tmp ,allocation-pointer-register)
                  ,@inc
                  (if (> ,tmp ,end-of-allocation-register)
                      (,collect-label)
                      (nop))
                  ,expr)))))))
  (define Body
    (lambda (b)
      (match b
        [(locals (,uvar* ...) ,tail)
         (set! new-local* '())
         (let-values ([(tail^ l h) (Tail tail)])
           `(locals (,uvar* ... ,new-local* ...)
              ,(wrap-with tail^ h)))])))
  (define Tail
    (lambda (t)
      (match t
        [(if ,cond ,[Tail -> conseq l1 h1] ,[Tail -> alter l2 h2])
         (if (or (not *max-conservative-range*)
                   (< (- (max h1 h2) (min l1 l2)) *max-conservative-range*))
             (let-values ([(cond^ l h) ((Pred l1 h1 l2 h2) cond)])
               (values `(if ,cond^ ,conseq ,alter) l h))
             (let-values ([(cond^ l3 h3) ((Pred 0 0 0 0) cond)])
               (values `(if ,cond^ ,(wrap-with conseq h1) ,(wrap-with alter h2)) l3 h3)))]
        [(begin ,eff* ... ,[Tail -> tail l h])
         (let-values ([(eff*^ l^ h^) ((Effect* l h) eff*)])
           (values (make-begin `(,eff*^ ... ,tail)) l^ h^))]
        [(alloc ,expr)
         (if (number? expr)
             (values e expr expr)
             (values (wrap-with e expr) 0 0))]
        [,x (values x 0 0)])))
  (define Pred
    (lambda (c-low c-high a-low a-high)
      (lambda (p)
        (match p
          [(true) (values p c-low c-high)]
          [(false) (values p a-low a-high)]
          [(if ,cond
               ,[(Pred c-low c-high a-low a-high) -> conseq l1 h1]
               ,[(Pred c-low c-high a-low a-high) -> alter l2 h2])
           (if (or (not *max-conservative-range*)
                   (< (- (max h1 h2) (min l1 l2)) *max-conservative-range*))
               (let-values ([(cond^ l h) ((Pred l1 h1 l2 h2) cond)])
                 (values `(if ,cond^ ,conseq ,alter) l h))
               (let-values ([(cond^ l3 h3) ((Pred 0 0 0 0) cond)])
                 (values `(if ,cond^ ,(wrap-with conseq h1) ,(wrap-with alter h2)) l3 h3)))]
          [(begin ,eff* ... ,[(Pred c-low c-high a-low a-high) -> pred l h])
           (let-values ([(eff*^ l^ h^) ((Effect* l h) eff*)])
             (values (make-begin `(,eff*^ ... ,pred)) l^ h^))]
          [,x (values x (min c-low a-low) (max c-high a-high))]))))
  (define Effect
    (lambda (low high)
      (lambda (e)
        (match e
          [(set! ,uvar (alloc ,expr))
           (if (number? expr)
               (values e (+ low expr) (+ high expr))
               (values (wrap-with e (list expr high)) 0 0))]
          [(mset! ,base ,offset (alloc ,expr))
           (if (number? expr)
               (values e (+ low expr) (+ high expr))
               (values (wrap-with e (list expr high)) 0 0))]
          [(set! ,uvar (,proc ,args* ...)) (guard (and (not (binop? proc))
                                                       (not (eq? proc 'mref))))
           (values (make-nopless-begin `(,e ,(wrap-with '(nop) high))) 0 0)]
          [(mset! ,base ,offset (,proc ,args* ...)) (guard (and (not (binop? proc))
                                                                (not (eq? proc 'mref))))
           (values (make-nopless-begin `(,e ,(wrap-with '(nop) high))) 0 0)]
          [(,proc ,arg* ...) (guard (and (not (eq? proc 'set!))
                                         (not (eq? proc 'mset!))
                                         (not (eq? proc 'nop))))
           (values (make-nopless-begin `(,e ,(wrap-with '(nop) high))) 0 0)]
          [(if ,cond ,[(Effect low high) -> conseq l1 h1] ,[(Effect low high) -> alter l2 h2])
           (if (or (not *max-conservative-range*)
                   (< (- (max h1 h2) (min l1 l2)) *max-conservative-range*))
               (let-values ([(cond^ l h) ((Pred l1 h1 l2 h2) cond)])
                 (values `(if ,cond^ ,conseq ,alter) l h))
               (let-values ([(cond^ l3 h3) ((Pred 0 0 0 0) cond)])
                 (values `(if ,cond^ ,(wrap-with conseq h1) ,(wrap-with alter h2)) l3 h3)))]
          [(begin ,eff* ... ,[(Effect low high) -> eff l h])
           (let-values ([(eff*^ l^ h^) ((Effect* l h) eff*)])
             (values (make-begin `(,eff*^ ... ,eff)) l^ h^))]
          [,x (values x low high)]))))
  (define Effect*
    (lambda (low high)
      (lambda (e*)
        (if (null? e*)
            (values '() low high)
            (let*-values ([(d l h) ((Effect* low high) (cdr e*))]
                          [(a l^ h^) ((Effect l h) (car e*))])
              (values (cons a d) l^ h^))))))
  (lambda (p)
    (if *collection-enabled*
        (begin
          (set! collect-label (unique-label 'collect))
          (match p
            [(with-label-alias (,label-alias* ...)
               (with-global-data ,data
                 (with-closure-length ,closure-length
                   (letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body]))))
             `(with-label-alias ([,collect-label "_scheme_collect"]
                                 ,label-alias* ...)
                (with-global-data ,data
                  (with-closure-length ,closure-length
                    (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body))))]))
        p)))

(define-who impose-calling-conventions
  (define new-frames)
  (define fetch-arguments
    (lambda (args which)
      (cond [(null? args) '()]
            [(null? which) (fetch-arguments args 1)]
            [(integer? which)
             (cons `(set! ,(car args) ,(index->frame-var which))
               (fetch-arguments (cdr args) (+ which 1)))]
            [else (cons `(set! ,(car args) ,(car which))
                    (fetch-arguments (cdr args) (cdr which)))])))
  (define fill-arguments
    (lambda (args which)
      (cond [(null? args) '()]
            [(not which)
             (let ([nfv (unique-name 'nfv)])
               (let ([result (cons `(set! ,nfv ,(car args))
                               (fill-arguments (cdr args) #f))])
                 (set! new-frames (cons
                                    (cons (caar new-frames) (cons nfv (cdar new-frames)))
                                    (cdr new-frames)))
                 result))]
            [(integer? which)
             (cons `(set! ,(index->frame-var which) ,(car args))
               (fill-arguments (cdr args) (+ which 1)))]
            [else (cons `(set! ,(car which) ,(car args))
                    (fill-arguments (cdr args) (cdr which)))])))
  (define Body
    (lambda (parameter)
      (lambda (b)
        (let ([rp (unique-name 'rp)])
          (set! new-frames '())
          (match b
            [(locals (,uvar* ...) ,[(Tail rp) -> tail])
             (let ([prologue (cons `(set! ,rp ,return-address-register)
                               (fetch-arguments parameter parameter-registers))])
               `(locals (,uvar* ... ,rp ,parameter ... ,@(apply append (map cdr new-frames)))
                  (with-return-point ,rp
                    (new-frames ,new-frames
                      (begin ,@prologue ,tail)))))])))))
  (define Tail
    (lambda (rp)
      (lambda (t)
        (match t
          [(if ,[Pred -> cond] ,[(Tail rp) -> conseq] ,[(Tail rp) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[Effect -> effect*] ... ,[(Tail rp) -> tail]) (make-begin `(,effect* ... ,tail))]
          [(,proc ,rand* ...) (guard (not (binop? proc))
                                (not (eq? proc 'mref))
                                (not (eq? proc 'alloc)))
           (let* ([in-register (take (length parameter-registers) rand*)]
                  [in-frame (drop (length parameter-registers) rand*)]
                  [fill-register (fill-arguments in-register parameter-registers)]
                  [fill-frame (fill-arguments in-frame 1)])
             `(begin ,@fill-frame
                     ,@fill-register
                     (set! ,return-address-register ,rp)
                     (,proc
                       ,frame-pointer-register
                       ,return-address-register
                       ,allocation-pointer-register
                       ,stack-base-register
                       ,@(if *collection-enabled* (list end-of-allocation-register) '())
                       ,@(map cadr fill-register) ,@(map cadr fill-frame))))]
          [,expr `(begin (set! ,return-value-register ,expr)
                         (,rp
                           ,frame-pointer-register
                           ,return-value-register
                           ,allocation-pointer-register
                           ,stack-base-register
                           ,@(if *collection-enabled* (list end-of-allocation-register) '())))]))))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond] ,[Pred -> conseq]  ,[Pred -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred]) (make-begin `(,effect* ... ,pred))]
        [,x x])))
  (define Effect
    (lambda (e)
      (match e
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect]) (make-begin `(,effect* ... ,effect))]
        [(,proc ,args* ...) (guard (and (not (eq? proc 'set!))
                                        (not (eq? proc 'nop))
                                        (not (eq? proc 'mset!))))
         (let ([rp-label (unique-label 'rp)])
           (set! new-frames (cons `(,rp-label) new-frames))
           (let* ([in-register (take (length parameter-registers) args*)]
                  [in-new-frame (drop (length parameter-registers) args*)]
                  [fill-register (fill-arguments in-register parameter-registers)]
                  [fill-new-frame (fill-arguments in-new-frame #f)])
             `(return-point ,rp-label
                (begin ,@fill-new-frame
                       ,@fill-register
                       (set! ,return-address-register ,rp-label)
                       (,proc
                         ,frame-pointer-register
                         ,return-address-register
                         ,allocation-pointer-register
                         ,stack-base-register
                         ,@(if *collection-enabled* (list end-of-allocation-register) '())
                         ,@(map cadr fill-register) ,@(map cadr fill-new-frame))))))]
        [(set! ,uvar (,proc ,args* ...)) (guard
                                             (not (binop? proc))
                                           (not (eq? proc 'mref))
                                           (not (eq? proc 'alloc)))
         `(begin ,(Effect `(,proc ,args* ...))
                 (set! ,uvar ,return-value-register))]
        [(mset! ,base ,offset (,proc ,args* ...)) (guard
                                                      (not (binop? proc))
                                                    (not (eq? proc 'mref))
                                                    (not (eq? proc 'alloc)))
         `(begin ,(Effect `(,proc ,args* ...))
                 (mset! ,base ,offset ,return-value-register))]
        [,x x])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,[(Body '()) -> body]))))
       (let ([body*^ (map (lambda (p b) ((Body p) b)) uvar* body*)])
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*^)] ...) ,body)))))])))

(define-who expose-allocation-pointer
  (define Body
    (lambda (b)
      (match b
        [(locals (,uvar* ...)
           (with-return-point ,rp
             (new-frames (,nfv* ...) ,[Tail -> tail])))
         `(locals (,uvar* ...)
            (with-return-point ,rp
              (new-frames (,nfv* ...) ,tail)))])))
  (define Tail
    (lambda (t)
      (match t
        [(if ,[Pred -> cond] ,[Tail -> conseq] ,[Tail -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Tail -> tail])
         (make-begin `(,effect* ... ,tail))]
        [,x x])))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond] ,[Pred -> conseq] ,[Pred -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred])
         (make-begin `(,effect* ... ,pred))]
        [,x x])))
  (define Effect
    (lambda (e)
      (match e
        [(set! ,uvar (alloc ,expr))
         `(begin (set! ,uvar ,allocation-pointer-register)
                 (set! ,allocation-pointer-register (+ ,allocation-pointer-register ,expr)))]
        [(mset! ,base ,offset (alloc ,expr))
         `(begin (mset! ,base ,offset ,allocation-pointer-register)
                 (set! ,allocation-pointer-register (+ ,allocation-pointer-register ,expr)))]
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect])
         (make-begin `(,effect* ... ,effect))]
        [,x x])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body))))])))

(define uncover-frame-conflict
  (letrec ([graph #f]
           [liveset-cons (lambda (x l) (if (or (uvar? x) (frame-var? x)) (set-cons x l) l))]
           [add-conflict-list
             (lambda (v live)
               (if (uvar? v)
                   (let ([entry (assq v graph)])
                     (set-cdr! entry (union live (cdr entry)))))
               (let add-v ([vv* live])
                 (unless (null? vv*)
                   (let ([vv (car vv*)])
                     (if (uvar? vv)
                         (let ([entry (assq vv graph)])
                           (set-cdr! entry (liveset-cons v (cdr entry))))))
                   (add-v (cdr vv*)))))])
    (define call-live)
    (define Program
      (lambda (p)
        (match p
          [(with-label-alias ,label-alias
             (with-global-data ,data
               (with-closure-length ,closure-length
                 (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
           `(with-label-alias ,label-alias
              (with-global-data ,data
                (with-closure-length ,closure-length
                  (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
    (define Body
      (lambda (b)
        (set! call-live '())
        (match b
          [(locals (,uvar* ...)
             (with-return-point ,rp
               (new-frames (,nfv* ...) ,tail)))
           (set! graph
             (map (lambda (x) (list x)) uvar*))
           (let-values ([(tail^ live) (Tail tail)])
             (let ([spill (filter uvar? call-live)])
               `(locals ,(difference uvar* spill)
                  (with-return-point ,rp
                    (new-frames (,nfv* ...)
                      (spills ,spill
                        (frame-conflict ,graph
                          (call-live ,call-live ,tail^))))))))])))
    (define Tail
      (letrec ([make-liveset
                 (lambda (loc)
                   (cond [(null? loc) '()]
                         [else (liveset-cons (car loc) (make-liveset (cdr loc)))]))])
        (lambda (t)
          (match t
            [(if ,cond ,[Tail -> conseq c-l] ,[Tail -> alter a-l])
             (let-values ([(cond^ live) ((Pred c-l a-l) cond)])
               (values `(if ,cond^ ,conseq ,alter) live))]
            [(begin ,effect* ... ,[Tail -> tail live])
             (let-values ([(eff*^ live^) ((Effect* live) effect*)])
               (values `(begin ,eff*^ ... ,tail) live^))]
            [(,triv ,loc* ...)
             (values t (make-liveset (cons triv loc*)))]))))
    (define Pred
      (lambda (post-c post-a)
        (lambda (p)
          (match p
            [(true) (values p post-c)]
            [(false) (values p post-a)]
            [(if ,cond ,[(Pred post-c post-a) -> conseq c-l] ,[(Pred post-c post-a) -> alter a-l])
             (let-values ([(cond^ live) ((Pred c-l a-l) cond)])
               (values `(if ,cond^ ,conseq ,alter) live))]
            [(begin ,effect* ... ,[(Pred post-c post-a) -> pred live])
             (let-values ([(eff*^ live^) ((Effect* live) effect*)])
               (values `(begin ,eff*^ ... ,pred) live^))]
            [(,rel ,x ,y)
             (values p (liveset-cons x (liveset-cons y (union post-c post-a))))]))))
    (define Effect
      (lambda (post)
        (lambda (e)
          (match e
            [(nop) (values e post)]
            [(return-point ,label ,[Tail -> tail live])
             (set! call-live (union post call-live))
             (values `(return-point ,label ,(union live post) ,post ,tail)
               (union live post))] ; todo(?)
            [(if ,cond ,[(Effect post) -> conseq c-l] ,[(Effect post) -> alter a-l])
             (let-values ([(cond^ live) ((Pred c-l a-l) cond)])
               (values `(if ,cond^ ,conseq ,alter) live))]
            [(begin ,effect* ... ,[(Effect post) -> effect live])
             (let-values ([(effect*^ live^) ((Effect* live) effect*)])
               (values `(begin ,effect*^ ... ,effect) live^))]
            [(mset! ,base ,offset (,rator ,x ,y))
             (values e (liveset-cons base (liveset-cons offset (liveset-cons x (liveset-cons y post)))))]
            [(mset! ,base ,offset ,expr)
             (values e (liveset-cons base (liveset-cons offset (liveset-cons expr post))))]
            [(set! ,v (,rator ,x ,y))
             (if (or (register? v) (memq v post))
                 (let ([post-rhs (difference post (list v))])
                   (add-conflict-list v post-rhs)
                   (values e (liveset-cons x (liveset-cons y post-rhs))))
                 (values '(nop) post))]
            [(set! ,v ,x)
             (if (or (register? v) (memq v post))
                 (let ([post-rhs (difference post (list v))])
                   (add-conflict-list v (difference post-rhs (list x)))
                   (values e (liveset-cons x post-rhs)))
                 (values '(nop) post))]))))
    (define Effect*
      (lambda (post)
        (lambda (e*)
          (cond [(null? e*) (values '() post)]
                [else
                  (let*-values ([(e*^ post^) ((Effect* post) (cdr e*))]
                                [(e^ post^^) ((Effect post^) (car e*))])
                    (values (cons e^ e*^) post^^))]))))
    Program))

(define-who pre-assign-frame
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Body
    (lambda (b)
      (match b
        [(locals ,locs
           (with-return-point ,rp
             (new-frames ,nfvs
               (spills ,spill
                 (frame-conflict ,frame-conflict
                   (call-live ,call-lives ,tail))))))
         (letrec ([subst (lambda (l homes)
                           (cond [(null? l) '()]
                                 [(frame-var? (car l)) (cons (car l) (subst (cdr l) homes))]
                                 [(assq (car l) homes) =>
                                  (lambda (x) (cons (cadr x) (subst (cdr l) homes)))]
                                 [else (subst (cdr l) homes)]))]
                  [first-available (lambda (forbid)
                                     (let loop ([i 0])
                                       (let ([fv (index->frame-var i)])
                                         (if (memq fv forbid) (loop (+ i 1)) fv))))]
                  [first-except-rp
                    (lambda (l)
                      (cond [(null? (cdr l)) (car l)]
                            [(eq? (car l) rp) (first-except-rp (cdr l))]
                            [else (car l)]))])
           (let ([homes^
                   (let assign ([spill spill])
                     (if (null? spill) '()
                         (let* ([now (first-except-rp spill)]
                                [home (assign (remq now spill))]
                                [conflict (subst (cdr (assq now frame-conflict)) home)])
                           (cons `(,now ,(first-available conflict)) home))))])
             (let ([rp-home (assq rp homes^)])
               (if (and rp-home (not (eq? (cadr rp-home) return-address-location)))
                   (format-error who "unable to save return point properly")))
             `(locals ,locs
                (new-frames ,nfvs
                  (locate ,homes^
                    (frame-conflict ,frame-conflict
                      (call-live ,call-lives ,tail)))))))])))
  Program)

(define-who assign-new-frame
  (define nfv**) ; side effects again...
  (define nfv-home**)
  (define assign-new-frames
    (lambda (nfv* size)
      (if (null? nfv*) '()
          (cons `(,(car nfv*) ,(index->frame-var (add1 size)))
            (assign-new-frames (cdr nfv*) (add1 size))))))
  (define Body
    (lambda (b)
      (match b
        [(locals (,uvar* ...)
           (new-frames (,nfv* ...)
             (locate ,home*
               (frame-conflict ,frame-conflict
                 (call-live ,call-lives ,tail)))))
         (set! nfv** nfv*)
         (set! nfv-home** '())
         (let ([tail^ ((Tail home*) tail)])
           `(locals ,(difference uvar* (apply append (map cdr nfv*)))
              (ulocals ()
                (locate ,(append home* (apply append nfv-home**))
                  (frame-conflict ,frame-conflict ,tail^)))))])))
  (define Tail
    (lambda (assign)
      (lambda (t)
        (match t
          [(if ,[(Pred assign) -> cond] ,[(Tail assign) -> conseq] ,[(Tail assign) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect assign) -> effect*] ... ,[(Tail assign) -> tail])
           (make-begin `(,effect* ... ,tail))]
          [,x x]))))
  (define Pred
    (lambda (assign)
      (lambda (p)
        (match p
          [(if ,[(Pred assign) -> cond] ,[(Pred assign) -> conseq] ,[(Pred assign) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect assign) -> effect*] ... ,[(Pred assign) -> pred])
           (make-begin `(,effect* ... ,pred))]
          [,x x]))))
  (define Effect
    (lambda (assign)
      (lambda (e)
        (match e
          [(return-point ,label ,live ,post ,tail)
           (let* ([fv* (union (filter frame-var? live)
                              (map (lambda (uv) (cadr (assq uv assign)))
                                (intersection (map car assign) live)))]
                  [size (if (null? fv*) 0 (+ 1 (apply max (map frame-var->index fv*))))]
                  [nfv* (cdr (assq label nfv**))]
                  [nfv-home* (assign-new-frames nfv* size)])
             (set! nfv-home** (cons nfv-home* nfv-home**))
             (if (zero? size) e         ; impossible though
                 `(begin (set! ,frame-pointer-register
                           (+ ,frame-pointer-register ,(ash size align-shift)))
                         (return-point ,label ,(ash size align-shift) ,post ,tail)
                         (set! ,frame-pointer-register
                           (- ,frame-pointer-register ,(ash size align-shift))))))]
          [(if ,[(Pred assign) -> cond] ,[(Effect assign) -> conseq] ,[(Effect assign) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect assign) -> effect*] ... ,[(Effect assign) -> effect])
           (make-begin `(,effect* ... ,effect))]
          [,x x]))))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))

(define-who finalize-frame-locations
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Body
    (lambda (b)
      (match b
        [(locate ,homes ,tail) b]
        [(locals ,locs
           (ulocals ,ulocs
             (locate ([,uvar* ,loc*] ...)
               (frame-conflict ,frame-conflict ,tail))))
         `(locals ,locs
            (ulocals ,ulocs
              (locate ([,uvar* ,loc*] ...)
                (frame-conflict ,frame-conflict ,((Tail `((,uvar* . ,loc*) ...)) tail)))))])))
  (define Tail
    (lambda (map)
      (lambda (t)
        (match t
          [(if ,[(Pred map) -> pred] ,[(Tail map) -> conseq] ,[(Tail map) -> alter])
           `(if ,pred ,conseq ,alter)]
          [(begin ,[(Effect map) -> effect*] ... ,[(Tail map) -> tail])
           `(begin ,effect* ... ,tail)]
          [(,[(Triv map) -> triv] ,[(Triv map) -> loc*] ...)
           `(,triv ,loc* ...)]))))
  (define Pred
    (lambda (map)
      (lambda (p)
        (match p
          [(if ,[(Pred map) -> cond] ,[(Pred map) -> conseq] ,[(Pred map) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect map) -> effect*] ... ,[(Pred map) -> pred])
           `(begin ,effect* ... ,pred)]
          [(,rel ,[(Triv map) -> left] ,[(Triv map) -> right])
           `(,rel ,left ,right)]
          [,x x]))))
  (define Effect
    (lambda (map)
      (lambda (e)
        (match e
          [(nop) (list 'nop)]
          [(return-point ,label ,size (,[(Var map) -> live*] ...) ,[(Tail map) -> tail])
           `(return-point ,label ,size ,live* ,tail)]
          [(mset! ,[(Triv map) -> base] ,[(Triv map) -> offset]
             (,op ,[(Triv map) -> e1] ,[(Triv map) -> e2]))
           `(mset! ,base ,offset (,op ,e1 ,e2))]
          [(mset! ,[(Triv map) -> base] ,[(Triv map) -> offset] ,[(Triv map) -> expr])
           `(mset! ,base ,offset ,expr)]
          [(set! ,[(Var map) -> v1] (,op ,[(Triv map) -> v2] ,[(Triv map) -> x]))
           `(set! ,v1 (,op ,v2 ,x))]
          [(set! ,[(Var map) -> var] ,[(Triv map) -> triv])
           (if (eq? var triv) (list 'nop)
               `(set! ,var ,triv))]
          [(if ,[(Pred map) -> cond] ,[(Effect map) -> conseq] ,[(Effect map) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect map) -> effect*] ... ,[(Effect map) -> effect])
           `(begin ,effect* ... ,effect)]))))
  (define Var
    (lambda (map)
      (lambda (v)
        (cond [(and (uvar? v) (assq v map)) => cdr]
              [else v]))))
  (define Triv Var)
  Program)

(define-who select-instructions
  (define not-so-trivial?
    (lambda (x)
      (or (frame-var? x)
          (label? x)
          (and (int64? x) (not (int32? x))))))
  (define very-trivial?
    (lambda (x)
      (or (uvar? x) (register? x) (int32? x))))
  (define Body
    (lambda (b)
      (match b
        [(locate ,homes ,tail) b]
        [(locals ,locs
           (ulocals ,ulocs
             (locate ,homes
               (frame-conflict ,frame-conflict ,[Tail -> tail u]))))
         `(locals ,locs
            (ulocals ,(append u ulocs)
              (locate ,homes
                (frame-conflict ,frame-conflict ,tail))))])))
  (define Tail
    (lambda (t)
      (match t
        [(if ,[Pred -> cond u1] ,[Tail -> conseq u2] ,[Tail -> alter u3])
         (values `(if ,cond ,conseq ,alter)
           (append u1 u2 u3))]
        [(begin ,[Effect -> effect* u*] ... ,[Tail -> tail u])
         (values (make-begin `(,effect* ... ,tail))
           (apply append u u*))]
        [(,triv ,loc* ...)
         (if (integer? triv)
             (let ([temp (unique-name 'u)])
               (values `(begin (set! ,temp ,triv) (,temp ,loc*))
                 (list temp)))
             (values t '()))])))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond u1] ,[Pred -> conseq u2] ,[Pred -> alter u3])
         (values `(if ,cond ,conseq ,alter)
           (append u1 u2 u3))]
        [(begin ,[Effect -> effect* u*] ... ,[Pred -> pred u])
         (values (make-begin `(,effect* ... ,pred))
           (apply append u u*))]
        [(,rel ,x ,y)
         (cond [(and (frame-var? x) (frame-var? y))
                (let ([temp (unique-name 'u)])
                  (values `(begin (set! ,temp ,x) (,rel ,temp ,y))
                    (list temp)))]
               [(or (label? x)
                    (and (integer? x)
                         (not (int32? x))))
                (let ([temp (unique-name 'u)])
                  (let-values ([(p^ u) (Pred `(begin (set! ,temp ,x) (,rel ,temp ,y)))])
                    (values p^ (cons temp u))))]
               [(or (label? y)
                    (and (integer? y)
                         (not (int32? y))))
                (let ([temp (unique-name 'u)])
                  (let-values ([(p^ u) (Pred `(begin (set! ,temp ,y) (,rel ,x ,temp)))])
                    (values p^ (cons temp u))))]
               [(and (integer? x) (integer? y)) ; yes we can do better but just leave it to PE
                (let ([temp (unique-name 'u)])
                  (values `(begin (set! ,temp ,x) (,rel ,temp ,y))
                    (list temp)))]
               [(integer? x) (values `(,(flip-relop rel) ,y ,x) '())]
               [else (values p '())])]
        [,x (values x '())])))
  (define Effect
    (lambda (e)
      (match e
        [(return-point ,label ,size ,live ,[Tail -> tail u])
         (values `(return-point ,label ,size ,live ,tail) u)]
        [(if ,[Pred -> cond u1] ,[Effect -> conseq u2] ,[Effect -> alter u3])
         (values `(if ,cond ,conseq ,alter)
           (append u1 u2 u3))]
        [(begin ,[Effect -> effect* u*] ... ,[Effect -> effect u])
         (values (make-begin `(,effect* ... ,effect))
           (apply append u u*))]
        [(nop) (values '(nop) '())]
        [(mset! ,base ,offset ,expr)
         (cond [(not (very-trivial? expr))
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u)
                                (Effect `(begin (set! ,temp ,expr)
                                                (mset! ,base ,offset ,temp)))])
                    (values eff (cons temp u))))]
               [(or (and (label? base) (number? offset) (zero? offset))
                    (and (label? offset) (number? base) (zero? base)))
                (values e '())]
               [(and (very-trivial? base)
                     (very-trivial? offset)
                     (or (not (int32? base)) (not (int32? offset))))
                (values e '())]
               [(very-trivial? base)
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u) (Effect `(mset! ,base ,temp ,expr))])
                    (values
                      (make-begin (cons `(set! ,temp ,offset) (list eff)))
                      (cons temp u))))]
               [else
                 (let ([temp (unique-name 'u)])
                   (let-values ([(eff u) (Effect `(mset! ,temp ,offset ,expr))])
                     (values
                       (make-begin (cons `(set! ,temp ,base) (list eff)))
                       (cons temp u))))])]
        [(set! ,v (mref ,base ,offset))
         (cond [(frame-var? v)
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u) (Effect `(set! ,temp (mref ,base ,offset)))])
                    (values
                      (make-begin (cons eff `((set! ,v ,temp))))
                      (cons temp u))))]
               [(or (and (label? base) (number? offset) (zero? offset))
                    (and (label? offset) (number? base) (zero? base)))
                (values e '())]
               [(and (very-trivial? base)
                     (very-trivial? offset)
                     (or (not (int32? base)) (not (int32? offset))))
                (values e '())]
               [(very-trivial? base)
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u) (Effect `(set! ,v (mref ,base ,temp)))])
                    (values
                      (make-begin (cons `(set! ,temp ,offset) (list eff)))
                      (cons temp u))))]
               [else
                 (let ([temp (unique-name 'u)])
                   (let-values ([(eff u) (Effect `(set! ,v (mref ,temp ,offset)))])
                     (values
                       (make-begin (cons `(set! ,temp ,base) (list eff)))
                       (cons temp u))))])]
        [(set! ,v (,rator ,v ,x))
         (cond [(and (eq? rator '*) (frame-var? v))
                (let ([temp (unique-name 'u)])
                  (values `(begin (set! ,temp ,x)
                                  (set! ,temp (* ,temp ,v))
                                  (set! ,v ,temp))
                    (list temp)))]
               [(or (label? x)
                    (and (int64? x) (not (int32? x)))
                    (and (frame-var? v) (frame-var? x)))
                (let ([temp (unique-name 'u)])
                  (values `(begin (set! ,temp ,x)
                                  (set! ,v (,rator ,v ,temp)))
                    (list temp)))]
               [else (values e '())])]
        [(set! ,v (,rator ,x ,v)) (guard (commutative? rator))
         (Effect `(set! ,v (,rator ,v ,x)))]
        [(set! ,v (,rator ,x ,y)) (guard (not (frame-var? v)))
         (let-values ([(f s)
                       (if (and (commutative? rator)
                                (or (not-so-trivial? y)
                                    (and (very-trivial? x)
                                         (or (uvar? y) (register? y))))) ; chance to coalesce
                           (values y x)
                           (values x y))])
           (let-values ([(e^ u)
                         (Effect `(begin (set! ,v ,f)
                                         (set! ,v (,rator ,v ,s))))])
             (values e^ u)))]
        [(set! ,fv (,rator ,x ,y))
         (cond [(and (not (eq? rator '*))
                     (very-trivial? x)
                     (not (eq? fv y))) ; otherwise not sound
                (let-values ([(e^ u)
                              (Effect `(begin (set! ,fv ,x)
                                              (set! ,fv (,rator ,fv ,y))))])
                  (values e^ u))]
               [(and (not (eq? rator '*))
                     (commutative? rator)
                     (very-trivial? y))
                (let-values ([(e^ u)
                              (Effect `(begin (set! ,fv ,y)
                                              (set! ,fv (,rator ,fv ,x))))])
                  (values e^ u))]
               [else
                 (let ([temp (unique-name 'u)])
                   (let-values ([(e^ u)
                                 (Effect `(begin (set! ,temp (,rator ,x ,y))
                                                 (set! ,fv ,temp)))])
                     (values e^ (cons temp u))))])]
        [(set! ,v ,x) (cond [(and (frame-var? v) (not-so-trivial? x))
                             (let ([temp (unique-name 'u)])
                               (values `(begin (set! ,temp ,x) (set! ,v ,temp))
                                 (list temp)))]
                            [else (values e '())])])))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))

(define uncover-register-conflict
  (letrec ([graph #f]
           [move #f]
           [liveset-cons (lambda (x l) (if (or (uvar? x) (register? x)) (set-cons x l) l))]
           [add-conflict-list
             (lambda (v live)
               (if (uvar? v)
                   (let ([entry (assq v graph)])
                     (set-cdr! entry (union live (cdr entry)))))
               (let add-v ([vv* live])
                 (unless (null? vv*)
                   (let ([vv (car vv*)])
                     (if (uvar? vv)
                         (let ([entry (assq vv graph)])
                           (set-cdr! entry (liveset-cons v (cdr entry))))))
                   (add-v (cdr vv*)))))]
           [add-move (lambda (x y)
                       (if (eq? x y) (void))
                       (if (uvar? x)
                           (let ([entry (assq x move)])
                             (set-cdr! entry (liveset-cons y (cdr entry)))))
                       (if (uvar? y)
                           (let ([entry (assq y move)])
                             (set-cdr! entry (liveset-cons x (cdr entry))))))])
    (define Program
      (lambda (p)
        (match p
          [(with-label-alias ,label-alias
             (with-global-data ,data
               (with-closure-length ,closure-length
                 (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
           `(with-label-alias ,label-alias
              (with-global-data ,data
                (with-closure-length ,closure-length
                  (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
    (define Body
      (lambda (b)
        (match b
          [(locate ,homes ,tail) b]
          [(locals ,locs (ulocals ,ulocs (locate ,homes (frame-conflict ,conflict ,tail))))
           (set! graph
             (append
               (map (lambda (x) (list x)) locs)
               (map (lambda (x) (list x)) ulocs)))
           (set! move
             (append
               (map (lambda (x) (list x)) locs)
               (map (lambda (x) (list x)) ulocs)))
           (Tail tail)
           (if *iterated-coalescing-enabled*
               `(locals ,locs
                  (ulocals ,ulocs
                    (locate ,homes
                      (frame-conflict ,conflict
                        (register-conflict ,graph
                          (register-move ,move ,tail))))))
               `(locals ,locs
                  (ulocals ,ulocs
                    (locate ,homes
                      (frame-conflict ,conflict
                        (register-conflict ,graph ,tail))))))])))
    (define Tail
      (letrec ([make-liveset
                 (lambda (loc)
                   (cond [(null? loc) '()]
                         [else (liveset-cons (car loc) (make-liveset (cdr loc)))]))])
        (lambda (t)
          (match t
            [(if ,cond ,[Tail -> c-l] ,[Tail -> a-l])
             ((Pred c-l a-l) cond)]
            [(begin ,effect* ... ,[Tail -> live])
             ((Effect* live) effect*)]
            [(,triv ,loc* ...)
             (make-liveset (cons triv loc*))]))))
    (define Pred
      (lambda (post-c post-a)
        (lambda (p)
          (match p
            [(true) post-c]
            [(false) post-a]
            [(if ,cond ,[(Pred post-c post-a) -> c-l] ,[(Pred post-c post-a) -> a-l])
             ((Pred c-l a-l) cond)]
            [(begin ,effect* ... ,[(Pred post-c post-a) -> live])
             ((Effect* live) effect*)]
            [(,rel ,x ,y)
             (liveset-cons x (liveset-cons y (union post-a post-c)))]))))
    (define Effect
      (lambda (post)
        (lambda (e)
          (match e
            [(nop) post]
            [(return-point ,label ,size ,live ,[Tail -> t-l])
             (union t-l post)]
            [(if ,cond ,[(Effect post) -> c-l] ,[(Effect post) -> a-l])
             ((Pred c-l a-l) cond)]
            [(begin ,effect* ... ,[(Effect post) -> live])
             ((Effect* live) effect*)]
            [(mset! ,base ,offset ,expr)
             (liveset-cons base (liveset-cons offset (liveset-cons expr post)))]
            [(set! ,v (,rator ,x ,y))
             (let ([post-rhs (difference post (list v))])
               (add-conflict-list v post-rhs)
               (liveset-cons x (liveset-cons y post-rhs)))]
            [(set! ,v ,x)
             (let ([post-rhs (difference post (list v))])
               (add-conflict-list v (difference post-rhs (list x)))
               (if *iterated-coalescing-enabled* (add-move v x))
               (liveset-cons x post-rhs))]))))
    (define Effect*
      (lambda (post)
        (lambda (e*)
          (cond [(null? e*) post]
                [else ((Effect ((Effect* post) (cdr e*))) (car e*))]))))
    Program))

(define-who assign-registers-coalesce
  (define K (length registers))
  (define ulocals)
  (define remq*
    (lambda (x tr)
      (cond [(null? tr) '()]
            [(pair? (car tr))
             (cons (remq* x (car tr)) (remq* x (cdr tr)))]
            [(eq? (car tr) x)
             (remq* x (cdr tr))]
            [else (cons (car tr) (remq* x (cdr tr)))])))
  (define follow
    (lambda (x mov)
      (cond [(assq x mov) => (lambda (x-y) (follow (cadr x-y) mov))]
            [else x])))
  (define select-entry
    (lambda (alst ok?)
      (let loop ([before '()] [after alst])
        (if (null? after) #f
            (let ([now (car after)])
              (if (ok? now)
                  (cons now
                    (append (reverse before) (cdr after)))
                  (loop (cons now before) (cdr after))))))))
  (define simplify
    (lambda (conf mov)
      (cond
        [(null? conf) '()]
        [(select-entry conf (lambda (e)
                              (and (< (length (cdr e)) K)
                                   (null? (cdr (assq (car e) mov))))))
         => (lambda (entry-rest)
              (let* ([entry (car entry-rest)]
                     [uvar (car entry)]
                     [uvar-conf0 (cdr entry)]
                     [rest (cdr entry-rest)]
                     [conf^ (remq* uvar rest)]
                     [mov^0 (filter
                              (lambda (e) (not (eq? uvar (car e))))
                              mov)]
                     [mov^ (remq* uvar mov^0)]
                     [assignment0 (simplify conf^ mov^)]
                     [uvar-conf (map (lambda (x) (follow x assignment0)) uvar-conf0)]
                     [available (difference registers uvar-conf)])
                (cons (list uvar (car available)) assignment0)))]
        [else (coalesce conf mov freeze)])))
  (define coalesce
    (lambda (conf mov k)
      (define coalescable
        (lambda (x y conf)
          (let ([x-conf (cdr (assq x conf))])
            (and (not (memq y x-conf))
                 (cond [(register? y)
                        (for-all
                          (lambda (t)
                            (or (register? t)
                                (memq y (cdr (assq t conf)))
                                (< (length (cdr (assq t conf))) K)))
                          x-conf)]
                       [else
                         (let* ([y-conf (cdr (assq y conf))]
                                [x-conf-sig
                                  (filter
                                    (lambda (t)
                                      (or (register? t)
                                          (>= (length (cdr (assq t conf))) K)))
                                    x-conf)]
                                [y-conf-sig
                                  (filter
                                    (lambda (t)
                                      (or (register? t)
                                          (>= (length (cdr (assq t conf))) K)))
                                    y-conf)])
                           (< (+ (length x-conf-sig) (length y-conf-sig)) K))])
                 y)))) ; can improve
      (cond
        [(select-entry mov (lambda (e)
                             (let ([x (car e)])
                               (exists (lambda (y) (coalescable x y conf)) (cdr e)))))
         => (lambda (entry-rest)
              (letrec ([replace
                         (lambda (x y t*)
                           (if (memq y t*)
                               (remq x t*)
                               (subst y x t*)))])
                (let* ([entry (car entry-rest)]
                       [x (car entry)]
                       [x-mov (cdr entry)]
                       [y (exists (lambda (y) (coalescable x y conf)) (cdr entry))]
                       [rest (cdr entry-rest)]
                       [mov^ (map (lambda (e)
                                    (if (eq? (car e) y)
                                        (cons (car e) (remq y (union x-mov (remq x (cdr e)))))
                                        (cons (car e) (replace x y (cdr e))))) rest)]
                       [x-conf (cdr (assq x conf))]
                       [conf^0 (filter
                                 (lambda (e) (not (eq? x (car e))))
                                 conf)]
                       [conf^ (map (lambda (e)
                                     (if (eq? (car e) y)
                                         (cons y (union x-conf (cdr e)))
                                         (cons (car e) (replace x y (cdr e))))) conf^0)]
                       [assignment0 (coalesce conf^ mov^ simplify)])
                  (cons (list x y) assignment0))))]
        [else (k conf mov)])))
  (define freeze
    (lambda (conf mov)
      (cond
        [(select-entry mov (lambda (e) (and (not (null? (cdr e)))
                                            (< (length (cdr (assq (car e) conf))) K))))
         => (lambda (entry-rest)
              (let* ([entry (car entry-rest)]
                     [uvar (car entry)]
                     [mov^ (map (lambda (e)
                                  (if (eq? (car e) uvar)
                                      (list uvar)
                                      (cons (car e) (remq uvar (cdr e)))))
                             mov)])
                (simplify conf mov^)))]
        [else (spill conf mov)])))
  (define spill
    (lambda (conf mov)
      (cond
        [(select-entry conf (lambda (e) (not (memq (car e) ulocals))))
         => (lambda (entry-rest)
              (let* ([entry (car entry-rest)]
                     [uvar (car entry)]
                     [uvar-conf0 (cdr entry)]
                     [rest (cdr entry-rest)]
                     [conf^ (remq* uvar rest)]
                     [mov^0 (filter
                              (lambda (e) (not (eq? uvar (car e))))
                              mov)]
                     [mov^ (remq* uvar mov^0)]
                     [assignment0 (simplify conf^ mov^)]
                     [uvar-conf (map (lambda (x) (follow x assignment0)) uvar-conf0)]
                     [available (difference registers uvar-conf)])
                (if (null? available)
                    assignment0
                    (cons (list uvar (car available)) assignment0))))]
        [else (format-error who
                "no candidate for register assignment in ~s" graph)])))
  (define Body
    (letrec ()
      (lambda (b)
        (match b
          [(locate ,homes ,tail) b]
          [(locals ,locs
             (ulocals ,ulocs
               (locate ,homes
                 (frame-conflict ,frame-conflict
                   (register-conflict ,register-conflict
                     (register-move ,register-move ,tail))))))
           (set! ulocals ulocs)
           (let* ([assignment0 (simplify register-conflict register-move)]
                  [assignment (map
                                (lambda (e) (list (car e) (follow (car e) assignment0)))
                                assignment0)]
                  [assigned (map car assignment)]
                  [spill (difference (append locs ulocs) assigned)])
             (if (null? spill)
                 `(locate ,(append assignment homes) ,tail)
                 `(locals ,(difference locs spill)
                    (ulocals ,ulocs
                      (spills ,(difference spill ulocs)
                        (locate ,homes
                          (frame-conflict ,frame-conflict ,tail)))))))]))))
  (lambda (p)
    (match p
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
       `(with-label-alias ,label-alias
          (with-global-data ,data
            (with-closure-length ,closure-length
              (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))

(define-who assign-registers-vanilla
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Body
    (letrec ([low-degree? (lambda (e) (< (length (cdr e)) (length registers)))]
             [select (lambda (conflict ok?)
                       (let loop ([before '()] [after conflict])
                         (if (null? after) #f
                             (let ([now (car after)])
                               (if (ok? now)
                                   (cons now
                                     (append (reverse before) (cdr after)))
                                   (loop (cons now before) (cdr after)))))))]
             [color (lambda (ulocs graph)
                      (if (null? graph) '()
                          (let* ([entry-rest
                                   (or (select graph low-degree?)
                                       (select graph (lambda (x) (not (memq x ulocs))))
                                       (format-error who
                                         "no candidate for register assignment in ~s" graph))]
                                 [entry (car entry-rest)]
                                 [rest (cdr entry-rest)])
                            (let* ([graph^ (map (lambda (e)
                                                  (cons (car e)
                                                    (difference (cdr e) (list (car entry))))) rest)]
                                   [assign (color ulocs graph^)]
                                   [conflict (cdr entry)]
                                   [now (car entry)]
                                   [conflict^
                                     (let replace ([conflict conflict])
                                       (cond [(null? conflict) '()]
                                             [(register? (car conflict))
                                              (cons (car conflict) (replace (cdr conflict)))]
                                             [(assq (car conflict) assign) =>
                                              (lambda (x) (cons (cadr x) (replace (cdr conflict))))]
                                             [else (replace (cdr conflict))]))]
                                   [available (difference registers conflict^)])
                              (if (null? available)
                                  assign
                                  (cons (cons now (cons (car available) '())) assign))))))])
      (lambda (b)
        (match b
          [(locate ,homes ,tail) b]
          [(locals ,locs
             (ulocals ,ulocs
               (locate ,homes
                 (frame-conflict ,frame-conflict
                   (register-conflict ,register-conflict ,tail)))))
           (let* ([assign (color ulocs register-conflict)]
                  [assigned (map car assign)]
                  [spill (difference (append locs ulocs) assigned)])
             (if (null? spill)
                 `(locate ,(append assign homes) ,tail)
                 `(locals ,(difference locs spill)
                    (ulocals ,ulocs
                      (spills ,(difference spill ulocs)
                        (locate ,homes
                          (frame-conflict ,frame-conflict ,tail)))))))]))))
  Program)

(define assign-registers
  (lambda (p)
    (if *iterated-coalescing-enabled*
        (assign-registers-coalesce p)
        (assign-registers-vanilla p))))

(define-who everybody-home?
  (define all-home?
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail))))) #f]
        [(locate (,home* ...) ,tail) #t]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(with-label-alias ,label-alias
         (with-global-data ,data
           (with-closure-length ,closure-length
             (letrec ([,label* (lambda () ,body*)] ...) ,body))))
       (andmap all-home? `(,body ,body* ...))]
      [,x (error who "invalid Program ~s" x)])))

(define-who assign-frame
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Body
    (lambda (b)
      (match b
        [(locate ,homes ,tail) b]
        [(locals ,locs
           (ulocals ,ulocs
             (spills ,spill
               (locate ,homes
                 (frame-conflict ,frame-conflict ,tail)))))
         (letrec ([subst (lambda (l homes)
                           (cond [(null? l) '()]
                                 [(frame-var? (car l)) (cons (car l) (subst (cdr l) homes))]
                                 [(assq (car l) homes) =>
                                  (lambda (x) (cons (cadr x) (subst (cdr l) homes)))]
                                 [else (subst (cdr l) homes)]))]
                  [first-available (lambda (forbid)
                                     (let loop ([i 0])
                                       (let ([fv (index->frame-var i)])
                                         (if (memq fv forbid) (loop (+ i 1)) fv))))])
           (let ([homes^
                   (let assign ([spill spill])
                     (if (null? spill) homes
                         (let* ([home (assign (cdr spill))]
                                [now (car spill)]
                                [conflict (subst (cdr (assq now frame-conflict)) home)])
                           (cons `(,now ,(first-available conflict)) home))))])
             `(locals ,locs
                (ulocals ,ulocs
                  (locate ,homes^
                    (frame-conflict ,frame-conflict ,tail))))))])))
  Program)

(define-who discard-call-live
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Body
    (lambda (b)
      (match b
        [(locate ([,uvar* ,loc*] ...) ,[Tail -> tail])
         `(locate ([,uvar* ,loc*] ...) ,tail)])))
  (define Tail
    (lambda (t)
      (match t
        [(if ,[Pred -> cond] ,[Tail -> conseq] ,[Tail -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Tail -> tail])
         `(begin ,effect* ... ,tail)]
        [(,triv ,loc* ...)
         (list triv)])))
  (define Pred
    (lambda (p)
      (match p
        [(if ,[Pred -> cond] ,[Pred -> conseq] ,[Pred -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Pred -> pred]) `(begin ,effect* ... ,pred)]
        [,x x])))
  (define Effect
    (lambda (e)
      (match e
        [(return-point ,label ,size ,live ,[Tail -> tail]) `(return-point ,label ,size ,live ,tail)]
        [(if ,[Pred -> cond] ,[Effect -> conseq] ,[Effect -> alter]) `(if ,cond ,conseq ,alter)]
        [(begin ,[Effect -> effect*] ... ,[Effect -> effect]) `(begin ,effect* ... ,effect)]
        [,x x])))
  Program)

(define-who finalize-locations
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Body
    (lambda (b)
      (match b
        [(locate ([,uvar* ,loc*] ...) ,tail)
         ((Tail `((,uvar* . ,loc*) ...)) tail)])))
  (define Tail
    (lambda (map)
      (lambda (t)
        (match t
          [(if ,[(Pred map) -> pred] ,[(Tail map) -> conseq] ,[(Tail map) -> alter])
           `(if ,pred ,conseq ,alter)]
          [(begin ,[(Effect map) -> effect*] ... ,[(Tail map) -> tail])
           `(begin ,effect* ... ,tail)]
          [(,[(Triv map) -> triv]) (list triv)]))))
  (define Pred
    (lambda (map)
      (lambda (p)
        (match p
          [(if ,[(Pred map) -> cond] ,[(Pred map) -> conseq] ,[(Pred map) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect map) -> effect*] ... ,[(Pred map) -> pred])
           `(begin ,effect* ... ,pred)]
          [(,rel ,[(Triv map) -> left] ,[(Triv map) -> right])
           `(,rel ,left ,right)]
          [,x x]))))
  (define Effect
    (lambda (map)
      (lambda (e)
        (match e
          [(nop) (list 'nop)]
          [(return-point ,label ,size ,live ,[(Tail map) -> tail])
           `(return-point ,label ,size ,live ,tail)]
          [(mset! ,[(Triv map) -> base] ,[(Triv map) -> offset] ,[(Triv map) -> expr])
           `(mset! ,base ,offset ,expr)]
          [(set! ,[(Var map) -> v1] (,op ,[(Triv map) -> v2] ,[(Triv map) -> x]))
           `(set! ,v1 (,op ,v2 ,x))]
          [(set! ,[(Var map) -> var] ,[(Triv map) -> triv])
           (if (eq? var triv) (list 'nop)
               `(set! ,var ,triv))]
          [(if ,[(Pred map) -> cond] ,[(Effect map) -> conseq] ,[(Effect map) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Effect map) -> effect*] ... ,[(Effect map) -> effect])
           `(begin ,effect* ... ,effect)]))))
  (define Var
    (lambda (map)
      (lambda (v)
        (if (uvar? v) (cdr (assq v map)) v))))
  (define Triv Var)
  Program)

(define-who expose-frame-var
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[(Tail 0) -> body*])] ...) ,[(Tail 0) -> body]))))
         `(with-label-alias ,label-alias
            (with-global-data ,data
              (with-closure-length ,closure-length
                (letrec ([,label* (lambda () ,body*)] ...) ,body))))])))
  (define Tail
    (lambda (offset)
      (lambda (t)
        (match t
          [(if ,[(Pred offset) -> pred offset^] ,conseq ,alter)
           `(if ,pred ,((Tail offset^) conseq) ,((Tail offset^) alter))]
          [(begin ,effect* ... ,tail)
           (let-values ([(effect*^ offset^) ((Effect* offset) effect*)])
             `(begin ,effect*^ ... ,((Tail offset^) tail)))]
          [(,[(Triv offset) -> triv]) (list triv)]))))
  (define Pred
    (lambda (offset)
      (lambda (p)
        (match p
          [(if ,[(Pred offset) -> cond offset^] ,conseq ,alter)
           (let-values ([(conseq^ offset1) ((Pred offset^) conseq)]
                        [(alter^ offset2) ((Pred offset^) alter)])
             (if (= offset1 offset2)
                 (values `(if ,cond ,conseq^ ,alter^) offset1)
                 (format-error who "inconsistent offsets in branches of ~s" p)))]
          [(begin ,effect* ... ,pred)
           (let*-values ([(effect*^ offset^) ((Effect* offset) effect*)]
                         [(pred^ offset^^) ((Pred offset^) pred)])
             (values `(begin ,effect*^ ... ,pred^) offset^^))]
          [(,rel ,[(Triv offset) -> left] ,[(Triv offset) -> right])
           (values `(,rel ,left ,right) offset)]
          [,x (values x offset)]))))
  (define Effect*
    (lambda (offset)
      (lambda (e*)
        (cond [(null? e*) (values '() offset)]
              [else (let*-values ([(eff off) ((Effect offset) (car e*))]
                                  [(eff* off^) ((Effect* off) (cdr e*))])
                      (values (cons eff eff*) off^))]))))
  (define Effect
    (lambda (offset)
      (lambda (e)
        (match e
          [(nop) (values (list 'nop) offset)]
          [(return-point ,label ,size ,live ,[(Tail offset) -> tail])
           (values `(return-point ,label ,size ,live ,tail) offset)]
          [(mset! ,[(Triv offset) -> base] ,[(Triv offset) -> off] ,[(Triv offset) -> expr])
           (values `(mset! ,base ,off ,expr) offset)]
          [(set! ,[(Var offset) -> v1] (,op ,[(Triv offset)  -> v2] ,[(Triv offset) -> x]))
           (if (eq? v1 frame-pointer-register)
               (cond [(and (eq? op '+) (integer? x))
                      (values `(set! ,v1 (,op ,v2 ,x)) (+ offset x))]
                     [(and (eq? op '-) (integer? x))
                      (values `(set! ,v1 (,op ,v2 ,x)) (- offset x))]
                     [else (format-error who "unknown frame pointer register value after ~s" e)])
               (values `(set! ,v1 (,op ,v2 ,x)) offset))]
          [(set! ,[(Var offset) -> var] ,[(Triv offset) -> triv])
           (if (eq? var frame-pointer-register)
               (format-error who "unknown frame pointer register value after ~s" e)
               (values `(set! ,var ,triv) offset))]
          [(if ,[(Pred offset) -> cond offset^] ,conseq ,alter)
           (let-values ([(conseq^ offset1) ((Effect offset^) conseq)]
                        [(alter^ offset2) ((Effect offset^) alter)])
             (if (= offset1 offset2)
                 (values `(if ,cond ,conseq^ ,alter^) offset1)
                 (format-error who "inconsistent offsets in branches of ~s" p)))]
          [(begin ,effect* ... ,effect)
           (let*-values ([(effect*^ offset^) ((Effect* offset) effect*)]
                         [(effect^ offset^^) ((Effect offset^) effect)])
             (values `(begin ,effect*^ ... ,effect^) offset^^))]))))
  (define Var
    (lambda (offset)
      (lambda (v)
        (if (frame-var? v)
            (make-disp-opnd
              frame-pointer-register
              (- (ash (frame-var->index v) align-shift) offset))
            v))))
  (define Triv Var)
  Program)

(define-who expose-basic-blocks
  (define frame-information)
  (define Program
    (lambda (p)
      (set! frame-information '())
      (match p
        [(with-label-alias ,label-alias
           (with-global-data ,data
             (with-closure-length ,closure-length
               (letrec ([,label* (lambda () ,[Tail -> body* block*])] ...) ,[Tail -> body block]))))
         (letrec ([pair (lambda (headers others)
                          (if (null? headers) '()
                              (append
                                (cons (car headers) (car others))
                                (pair (cdr headers) (cdr others)))))])
           (let ([blocks (pair `([,label* (lambda () ,body*)] ...) block*)])
             `(with-label-alias ,label-alias
                (with-global-data ,data
                  (with-closure-length ,closure-length
                    (with-frame-information ,(if *collection-enabled*
                                                frame-information
                                                '())
                      (letrec ,(append block blocks)
                        ,body)))))))])))
  (define Tail
    (lambda (t)
      (match t
        [(begin ,effect* ... ,[Tail -> tail block])
         (let-values ([(tail^ block^) ((Effect* tail) effect*)])
           (values tail^ (append block^ block)))]
        [(if ,cond ,[Tail -> conseq b1] ,[Tail -> alter b2])
         (let ([c (unique-label 'c)]
               [a (unique-label 'a)])
           (let-values ([(tail b3) ((Pred c a) cond)])
             (values tail (append b3
                            (cons `[,c (lambda () ,conseq)]
                              (append b1
                                (cons `[,a (lambda () ,alter)] b2)))))))]
        [,x (values x '())])))
  (define Pred
    (lambda (c a)
      (lambda (p)
        (match p
          [(true) (values (list c) '())]
          [(false) (values (list a) '())]
          [(if ,cond ,conseq ,alter)
           (let-values ([(c-t b1) ((Pred c a) conseq)]
                        [(a-t b2) ((Pred c a) alter)])
             (let ([c^ (unique-label 'c)]
                   [a^ (unique-label 'a)])
               (let-values ([(tail b3) ((Pred c^ a^) cond)])
                 (values tail (append b3
                                (cons `[,c^ (lambda () ,c-t)]
                                  (append b1
                                    (cons `[,a^ (lambda () ,a-t)] b2))))))))]
          [(begin ,effect* ... ,[(Pred c a) -> tail b1])
           (let-values ([(tail^ b2) ((Effect* tail) effect*)])
             (values tail^ (append b2 b1)))]
          [(,rel ,x1 ,x2)
           (values `(if (,rel ,x1 ,x2) (,c) (,a)) '())]))))
  (define Effect
    (lambda (k)
      (lambda (e)
        (match e
          [(nop) (values k '())]
          [(return-point ,label ,size ,live ,[Tail -> tail block])
           (set! frame-information (cons (list label size live) frame-information))
           (values tail (append block (list `[,label (lambda () ,k)])))]
          [(if ,cond ,conseq ,alter)
           (let ([c (unique-label 'c)]
                 [a (unique-label 'a)]
                 [j (unique-label 'j)])
             (let-values ([(c-t b1) ((Effect (list j)) conseq)]
                          [(a-t b2) ((Effect (list j)) alter)]
                          [(tail b3) ((Pred c a) cond)])
               (values tail (append b3
                              (cons `[,c (lambda () ,c-t)]
                                (append b1
                                  (cons `[,a (lambda () ,a-t)]
                                    (append b2
                                      (list `[,j (lambda () ,k)])))))))))]
          [(begin ,effect* ... ,[(Effect k) -> tail b1])
           (let-values ([(tail^ b2) ((Effect* tail) effect*)])
             (values tail^ (append b2 b1)))]
          [,x (values (make-begin `(,x ,k)) '())]))))
  (define Effect*
    (lambda (k)
      (lambda (e*)
        (cond [(null? e*) (values k '())]
              [else (let*-values ([(t b1) ((Effect* k) (cdr e*))]
                                  [(tail b2) ((Effect t) (car e*))])
                      (values tail (append b2 b1)))]))))
  Program)

(define-who optimize-jumps
  (define graph)
  (define trace-graph
    (lambda (start)
      (cond [(assq start graph) =>
             (lambda (x)
               (let ([dest (trace-graph (cdr x))])
                 (set-cdr! x dest)
                 dest))]
            [else start])))
  (define Tail
    (lambda (t)
      (match t
        [(begin ,[Effect -> eff*] ... ,[Tail -> tail])
         `(begin ,eff* ... ,tail)]
        [(if (,rel ,x ,y) (,c) (,a))
         `(if (,rel ,x ,y) (,(trace-graph c)) (,(trace-graph a)))]
        [(,triv) (list (trace-graph triv))])))
  (define Effect
    (lambda (e)
      (match e
        [(set! ,v1 (,rator ,v1 ,v2)) (guard (label? v2))
         `(set! ,v1 (,rator ,v1 ,(trace-graph v2)))]
        [(set! ,v1 ,v2) (guard (label? v2))
         `(set! ,v1 ,(trace-graph v2))]
        [,x x])))
  (define build-tail
    (lambda (label t)
      (match t
        [(,lab) (guard (label? lab))
         (if (eq? (trace-graph lab) label)
             (list `(,label (lambda () ,x)))
             (begin (set! graph (cons (cons label lab) graph))
                    '()))]
        [,x (list `(,label (lambda () ,x)))])))
  (define build
    (lambda (p)
      (set! graph '())
      (match p
        [(with-label-alias ,label-alias
             (with-global-data ,global-data
               (with-closure-length ,closure-length
                 (with-frame-information ,frame-information
                   (letrec ([,label* (lambda () ,body*)] ...) ,body)))))
         `(with-label-alias ,label-alias
            (with-global-data ,global-data
              (with-closure-length ,closure-length
                (with-frame-information ,frame-information
                  (letrec ,(apply append (map build-tail label* body*)) ,body)))))])))
  (lambda (p)
    (if *optimize-jumps-enabled*
        (match (build p)
          [(with-label-alias ,label-alias
             (with-global-data ,global-data
               (with-closure-length ,closure-length
                 (with-frame-information ,frame-information
                   (letrec ([,label* (lambda () ,[Tail -> body*])] ...) ,[Tail -> body])))))
           `(with-label-alias ,label-alias
              (with-global-data ,global-data
                (with-closure-length ,closure-length
                  (with-frame-information ,frame-information
                    (letrec ([,label* (lambda () ,body*)] ...) ,body)))))])
        p)))

(define-who flatten-program
  (define label-alias)
  (define Program
    (lambda (p)
      (match p
        [(with-label-alias ,la
           (with-global-data ,data
             (with-closure-length ,closure-length
               (with-frame-information ,frame-information
                 (letrec ([,label* (lambda () ,body*)] ...) ,body)))))
         (set! label-alias la)
         (letrec ([next-label (lambda (label*)
                                (if (null? (cdr label*))
                                    #f
                                    (let ([next (cadr label*)])
                                      (cond [(assq next closure-length) #f]
                                            [(assq next frame-information) #f]
                                            [else next]))))]
                  [cat (lambda (label* body*)
                         (cond [(null? label*) '()]
                               [else
                                 (let* ([lab (car label*)]
                                        [info
                                          (cond [(assq lab closure-length) =>
                                                 (lambda (l) `((align 8) ; hard coded...
                                                               (zeros ,tag-procedure) ; `align' code address
                                                               (quad ,(cadr l))))]
                                                [(assq lab frame-information) =>
                                                 (lambda (i) `((live-mask
                                                                 ,(sra (cadr i) align-shift)
                                                                 ,(map frame-var->index (caddr i)))
                                                               (quad ,(cadr i))))]
                                                [else '()])]
                                        [next (next-label label*)])
                                   (append info
                                     (cons lab
                                       (append ((Tail next) (car body*))
                                         (cat (cdr label*) (cdr body*))))))]))])
           (let ([next (next-label (cons 'dummy label*))])
             (list
               (cons 'data data)
               (cons 'code (append ((Tail next) body) (cat label* body*))))))])))
  (define Tail
    (lambda (next)
      (lambda (t)
        (match t
          [(begin ,[Effect -> eff*] ... ,[(Tail next) -> tail])
           (append eff* tail)]
          [(if (,rel ,x ,y) (,[Triv -> c]) (,[Triv -> a]))
           (cond [(eq? c next) `((if (not (,rel ,x ,y)) (jump ,a)))]
                 [(eq? a next) `((if (,rel ,x ,y) (jump ,c)))]
                 [else `((if (,rel ,x ,y) (jump ,c))
                         (jump ,a))])]
          [(,[Triv -> triv])
           (cond [(eq? triv next) '()]
                 [else (list `(jump ,triv))])]))))
  (define Effect
    (lambda (e)
      (match e
        [(mset! ,v ,[Triv -> base] ,[Triv -> offset])
         `(mset! ,v ,base ,offset)]
        [(set! ,v (mref ,[Triv -> base] ,[Triv -> offset]))
         `(set! ,v (mref ,base ,offset))]
        [(set! ,v1 (,rator ,[Triv -> v1] ,[Triv -> v2]))
         `(set! ,v1 (,rator ,v1 ,v2))]
        [(set! ,v1 ,[Triv -> v2])
         `(set! ,v1 ,v2)])))
  (define Triv
    (lambda (triv)
      (cond [(assq triv label-alias) => cadr]
            [else triv])))
  Program)

;; taken from yinwang
(define *enable-analyze* #f)

(define *all-closures* '())

(define-who analyze-closure-size
  (define Lambda
    (lambda (x)
      (match x
        [(lambda (,fml* ...)
           (bind-free (,cp ,free* ...)
             ,[Expr -> s*]))
         s*]
        [,x (error who "invalid Lambda ~s" x)])))
  (define Expr
    (lambda (x)
      (match x
        [(with-global-data ,data (outmost ,out ,[e])) e]
        [,label (guard (label? label)) '()]
        [,uvar (guard (uvar? uvar)) '()]
        [(quote ,imm) '()]
        [(if ,[test-s*] ,[conseq-s*] ,[altern-s*])
         (append test-s* conseq-s* altern-s*)]
        [(begin ,[s**] ... ,[s*]) (apply append s* s**)]
        [(let ([,lhs* ,[s**]] ...) ,[s*]) (apply append s* s**)]
        [(letrec ([,llabel* ,[Lambda -> s**]] ...)
           (closures ([,name* ,clabel* ,free** ...] ...)
             ,[s*]))
         (apply append (map length free**) s* s**)]
        [(,prim ,[s**] ...)
         (guard (primitive? prim))
         (apply append s**)]
        [(,[s*] ,[s**] ...) (apply append s* s**)]
        [,x (error who "invalid Expr ~s" x)])))
  (define analyze
    (lambda (x)
      (let ([s* (Expr x)])
        (let ([n (length s*)])
          (set! *all-closures* (append *all-closures* s*))
          (printf "closure num = ~s, avg = ~s: ~s\n"
                  n
                  (if (= n 0) '* (exact->inexact (/ (apply + s*) n)))
                  s*)))
      x))
  (lambda (x)
    (if *enable-analyze*
        (analyze x)
        x)))

(define *all-code-size* '())

(define analyze-code-size
  (lambda (x)
    (define analyze
      (lambda (x)
        (match x
          [((data ,data* ...)
            (code ,[ins*] ...))
           (printf "code size: ~a\n" (apply + ins*))
           (set! *all-code-size* (cons (apply + ins*) *all-code-size*))
           x]
          [,x (if (or (label? x)
                      (eq? (car x) 'quad)
                      (eq? (car x) 'live-mask)
                      (eq? (car x) 'align)
                      (eq? (car x) 'zeros)) 0 1)])))
    (if *enable-analyze*
        (analyze x)
        x)))

(define test-all-analyze
  (lambda ()
    (define bool->word
      (lambda (x)
        (if x "Yes" "No")))
    (fluid-let ([*enable-analyze* #t]
                [*all-closures* '()]
                [*all-code-size* '()])
      (test-all #f)
      (printf "\n** Options **
        scheme standard:               ~a
        garbage collection:            ~a
        encode large literals:         ~a
        optimize globals:              ~a
        iterated register coalescing:  ~a
        closure optimization:          ~a
        pre-optimization:              ~a
        optimize jumps:                ~a\n\n"
              *standard*
              (bool->word *collection-enabled*)
              (if (not *max-inline-literal-size*) "No" (format "Above ~a" *max-inline-literal-size*))
              (bool->word *optimize-global-enabled*)
              (bool->word *iterated-coalescing-enabled*)
              (bool->word *closure-optimization-enabled*)
              (bool->word *cp-1-enabled*)
              (bool->word *optimize-jumps-enabled*))
      (printf "** closure analysis report **
       total closures created:  ~a
       total free var:          ~a
       average free var:        ~a\n\n"
              (length *all-closures*)
              (apply + *all-closures*)
              (exact->inexact (/ (apply + *all-closures*)
                                 (length *all-closures*))))
      (printf "** code length report **
       total code length:    ~a
       average code length:  ~a\n"
              (apply + *all-code-size*)
              (exact->inexact (/ (apply + *all-code-size*)
                                 (length *all-code-size*)))))))

(define-syntax emit-jump
  (syntax-rules ()
    [(_ opcode ?target)
     (let ([target ?target])
       (cond [(label? target) (emit opcode (label->x86-64-label target))]
             [(string? target) (emit opcode target)]
             [else (emit opcode (format "*~a" (rand->x86-64-arg target)))]))]))

(define-syntax emit-program
  (syntax-rules ()
    [(_ code code* ...)
     (begin
       (emit '.globl "_scheme_entry")
       (emit-label "_scheme_entry")
       (emit 'pushq 'rbx)
       (emit 'pushq 'rbp)
       (emit 'pushq 'r12)
       (emit 'pushq 'r13)
       (emit 'pushq 'r14)
       (emit 'pushq 'r15)
       (emit 'movq 'rdi frame-pointer-register)
       (emit 'movq 'rdi stack-base-register)
       (emit 'movq 'rdx end-of-allocation-register)
       (emit 'movq 'rsi allocation-pointer-register)
       (emit 'leaq "_scheme_exit(%rip)" return-address-register)
       code code* ...
       (emit '.globl "_scheme_exit")
       (emit-label "_scheme_exit")
       (unless (eq? return-value-register 'rax)
         (emit 'movq return-value-register 'rax))
       (emit 'popq 'r15)
       (emit 'popq 'r14)
       (emit 'popq 'r13)
       (emit 'popq 'r12)
       (emit 'popq 'rbp)
       (emit 'popq 'rbx)
       (emit 'ret))]))

(define reverse-buffer)
(define reverse-size)
(define reverse-max)
(define reverse-first?)
(define reverse-padding)
(define reverse-delim)
(define reverse-out)
(define reverse-init
  (lambda ()
    (set! reverse-buffer '())
    (set! reverse-size 0)
    (set! reverse-first? #t)))
(define reverse-clear
  (lambda ()
    (if reverse-first?
        (set! reverse-first? #f)
        (printf "~a" reverse-delim))
    (for-each reverse-out reverse-buffer)
    (set! reverse-buffer '())
    (set! reverse-size 0)))
(define reverse-sweep
  (lambda ()
    (when reverse-padding
      (unless (= 0 reverse-size)
        (let loop ([i (- reverse-max reverse-size)])
          (unless (= i 0)
            (begin (reverse-put reverse-padding)
                   (loop (sub1 i)))))))))
(define reverse-put
  (lambda (byte)
    (set! reverse-buffer (cons byte reverse-buffer))
    (set! reverse-size (add1 reverse-size))
    (when (= reverse-size reverse-max) (reverse-clear))))
(define-syntax with-reverse-buffer
  (syntax-rules ()
    [(_ g f p d expr* ...)
     (begin
       (set! reverse-max g)
       (set! reverse-out f)
       (set! reverse-padding p)
       (set! reverse-delim d)
       (reverse-init)
       expr* ...
       (reverse-sweep))]))

(define-who generate-x86-64
  (define BaseOffset
    (lambda (base offset)
      (cond [(int32? base) (make-disp-opnd offset base)]
            [(int32? offset) (make-disp-opnd base offset)]
            [else (make-index-opnd base offset)])))
  (define Program
    (lambda (p)
      (match p
        [((data ,data* ...)
          (code ,stmt* ...))
         (letrec ([emit* (lambda (s)
                           (unless (null? s)
                             (Statement (car s))
                             (emit* (cdr s))))])
           (emit '.data)
           (emit-static-data data*)
           (emit '.text)
           (emit-program (emit* stmt*)))])))
  (define Label
    (lambda (lab)
      (if (string? lab)
          (format "L~a(%rip)" lab)
          (rand->x86-64-arg lab))))
  (define Statement
    (lambda (s)
      (match s
        [(align ,align) (emit '.align (number->string align))]
        [(zeros ,n) (emit '.zero (number->string n))]
        [(quad ,dw) (emit '.quad (number->string dw))]
        [(live-mask ,n ,live)
         (printf "    .byte 0b")
         (with-reverse-buffer 8 display 0 ", 0b"
           (let loop ([i 0])
             (unless (= i n)
               (if (and (not (eq? i (frame-var->index return-address-location)))
                        (memq i live))
                   (reverse-put 1)
                   (reverse-put 0))
               (loop (+ i 1)))))
         (printf "\n")]
        [,label (guard (or (label? label) (string? label)))
          (emit-label label)]
        [(jump ,dst) (emit-jump 'jmp dst)]
        [(if (,rel ,x ,y) (jump ,dst))
         (emit 'cmpq y x)
         (emit-jump (rel->assembly rel) dst)]
        [(if (not (,rel ,x ,y)) (jump ,dst))
         (emit 'cmpq y x)
         (emit-jump (not-rel->assembly rel) dst)]
        [(mset! ,base ,offset ,expr)
         (cond [(and (or (label? base) (string? base))
                     (number? offset) (zero? offset))
                (emit 'movq (rand->x86-64-arg expr) (Label base))]
               [(and (or (label? offset) (string? offset))
                     (number? base) (zero? base))
                (emit 'movq (rand->x86-64-arg expr) (Label offset))]
               [else (Statement `(set! ,(BaseOffset base offset) ,expr))])]
        [(set! ,v (mref ,base ,offset))
         (cond [(and (or (label? base) (string? base))
                     (number? offset) (zero? offset))
                (emit 'movq (Label base) (rand->x86-64-arg v))]
               [(and (or (label? offset) (string? offset))
                     (number? base) (zero? base))
                (emit 'movq (Label offset) (rand->x86-64-arg v))]
               [else (Statement `(set! ,v ,(BaseOffset base offset)))])]
        [(set! ,v1 (,rator ,v1 ,v2))
         (emit (binop->assembly rator) v2 v1)]
        [(set! ,v1 ,v2)
         (cond [(or (label? v2) (string? v2)) (emit 'leaq (Label v2) v1)]
               [else (emit 'movq v2 v1)])])))
  Program)

;; needs documentation
(define-who emit-static-data
  (define output-byte
    (lambda (b) (printf "~2,'0,,:X" b)))
  (define Byte reverse-put)
  (define Ptr
    (lambda (ptr)
      (let loop ([c 8] [x ptr])
        (unless (zero? c)
          (Byte (mod x 256))
          (loop (- c 1) (div x 256))))))
  (define Char
    (lambda (ch) (Byte (char->integer ch))))
  (define Datum
    (lambda (lit)
      (cond [(pair? lit)
             (Byte flag-pair)
             (Datum (car lit))
             (Datum (cdr lit))]
            [(and (vector? lit) (zero? (vector-length lit)))
             (Byte flag-empty-vector)]
            [(vector? lit)
             (Byte flag-vector)
             (Ptr (ash (vector-length lit) shift-fixnum))
             (vector-for-each Datum lit)
             (Byte flag-vector-end)]
            [(= lit $true) (Byte flag-true)]
            [(= lit $false) (Byte flag-false)]
            [(= lit $nil) (Byte flag-nil)]
            [else
              (Byte flag-trivial)
              (Ptr lit)])))
  (define Data
    (lambda (data)
      (match data
        [(symbol-dump ((quote ,sym*) ...))
         (emit '.globl "_scheme_symbol_dump")
         (emit-label "_scheme_symbol_dump")
         (if (null? sym*)
             (emit '.ascii "\"\"")
             (begin
               (printf "    .quad 0x")
               (with-reverse-buffer 8 output-byte 0 ", 0x"
                 (for-each
                   (lambda (sym)
                     (string-for-each Char (symbol->string sym))
                     (Char #\x00))
                   sym*))
               (printf "\n")))]
        [(roots (,root* ...))
         (emit '.align "8")
         (emit '.globl "_scheme_roots")
         (emit-label "_scheme_roots")
         (emit '.quad (number->string (ash (length root*) align-shift)))
         (for-each
           (lambda (lab)
             (emit-label lab)
             (emit '.quad "0"))
           root*)]
        [(,lab (encode-literal (quote ,complex)))
         (emit-label lab)
         (printf "    .quad 0x")
         (with-reverse-buffer 8 output-byte 0 ", 0x"
           (Datum complex))
         (printf "\n")])))
  (lambda (data*)
    (for-each Data data*)))

(compiler-passes '(
  parse-scheme
  proceduralize-primitive
  optimize-direct-call
  uncover-assigned
  purify-letrec
  optimize-constant
  optimize-useless
  uncover-outmost
  convert-assignments
  convert-complex-datum
  remove-anonymous-lambda
  sanitize-binding-forms
  uncover-free
  convert-closures
  optimize-known-call
  uncover-well-known
  optimize-free
  optimize-self-reference
  analyze-closure-size
  introduce-procedure-primitives
  lift-letrec
  normalize-context
  specify-representation
  uncover-locals
  remove-let
  verify-uil
  remove-complex-opera*
  flatten-set!
  insert-overflow-check
  impose-calling-conventions
  expose-allocation-pointer
  uncover-frame-conflict
  pre-assign-frame
  assign-new-frame
  (iterate
    finalize-frame-locations
    select-instructions
    uncover-register-conflict
    assign-registers
    (break when everybody-home?)
    assign-frame)
  discard-call-live
  finalize-locations
  expose-frame-var
  expose-basic-blocks
  optimize-jumps
  flatten-program
  analyze-code-size
  generate-x86-64
))

(load "tests15.scm")

(trusted-passes #t)

;; special tests

(define long `,(list 'quote tests))

(define normalizer
  '(letrec
       ([assq
          (lambda (x l)
            (if (null? l) #f
                (if (eq? (car (car l)) x)
                    (car l)
                    (assq x (cdr l)))))]
        [val1
          (lambda (e env)
            (cond [(symbol? e) (cdr (assq e env))]
                  [(eq? (car e) 'quote) (cadr e)]
                  [(eq? (car e) 'lambda)
                   (lambda (arg)
                     (val1 (caddr e) (cons (cons (caadr e) arg) env)))]
                  [else ((val1 (car e) env) (val1 (cadr e) env))]))]
        [val (lambda (e) (val1 e '()))]
        [uvars '(a b c d e f g h i j k l m n o p q r s t u v w x y z)]
        [c 0]
        [newvar
          (lambda ()
            (set! c (+ c 1))
            (letrec ([locate
                       (lambda (i l)
                         (if (eq? i 1)
                             (car l)
                             (locate (- i 1) (cdr l))))])
              (locate c uvars)))]
        [created-neutral '()]
        [neutral?1
          (lambda (x l)
            (cond [(null? l) #f]
                  [(eq? (caar l) x) (cdar l)]
                  [else (neutral?1 x (cdr l))]))]
        [neutral?
          (lambda (x) (neutral?1 x created-neutral))]
        [create-neutral
          (lambda (e)
            (letrec ([ne (lambda (y)
                           (create-neutral (cons ne y)))])
              (set! created-neutral (cons (cons ne e) created-neutral))
              ne))]
        [read-back
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
                  [else `',e]))]
        [normalize (lambda (e) (read-back (val e)))])
     (normalize '(((lambda (x) (lambda (y) (x y)))
                   (lambda (x) (x x)))
                  (lambda (x)
                    (lambda (y) x))))))

(define yin-yang
  '(let ([count 50]
         [res '()])
     (let ([yin
             ((lambda (cc)
                (set! res (cons '! res))
                (set! count (- count 1))
                (if (= count 0)
                    (lambda (x) res)
                    cc))
              (call/cc (lambda (c) c)))])
       (let ([yang
               ((lambda (cc)
                  (set! res (cons '- res))
                  (set! count (- count 1))
                  (if (= count 0)
                      (lambda (x) res)
                      cc))
                (call/cc (lambda (c) c)))])
         (yin yang)))))

(define debug-trace
  '(letrec ([kk #f]
            [fact
              (lambda (n)
                (cond [(<= n 0) (call/cc (lambda (k) (set! kk k) 1))]
                      [else (* n (fact (- n 1)))]))])
     (begin
       (fact 10)
       (inspect kk))))

(define garbage
  '(letrec ([malloc
              (lambda (n)
                (if (= 0 n)
                    'success
                    (let ([trash (make-vector 10000)])
                      (vector-set! trash 0 trash)
                      (malloc (- n 1)))))])
     (malloc 25)))

