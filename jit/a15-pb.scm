;; todo: * use SCCs in purify-letrec, and assimilate elsewhere;
;;       * in chez raw data are wrapped in a `raw' form; but here it's an overkill
;;       * remove useless set!
;;       * how to trace globals (more) precisely?
;;         A: no you can't do so with minimal effort (live masks should work?)

(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "../match.scm")
(load "helpers.scm")
(load "driver.scm")
(load "../fmts.pretty")
(load "../a15-wrapper.scm")

(game-eval compile)

(define *standard* 'r6rs)
(define *cp-1-enabled* #t) ; better set to #f when testing trivial cases
(define *closure-optimization-enabled* #t)
(define *iterated-coalescing-enabled* #t)
(define *optimize-jumps-enabled* #t)
(define *max-inline-literal-size* #f)
(define *collection-enabled* #f)
(define *max-conservative-range* 1024) ; may ask for more space than actually needed
(define *continuation-enabled* #f)
(define *jit-after* 1)

(set! registers '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
(set! caller-saved-registers registers)
(set! parameter-registers '())
(set! allocation-pointer-register 'r0)
(set! frame-pointer-register 'r1)
(set! return-value-register 'r2)

(define mask-symbol #b111)
(define tag-symbol  #b100)

(define mask-char   #b11111111)
(define tag-char    #b11111110)
(define shift-char  8)

(define flag-true $true)
(define flag-false $false)
(define flag-nil $nil)
(define flag-trivial 0)
(define flag-pair 1)
(define flag-vector 2)
(define flag-vector-end 3)
(define flag-empty-vector 4)

(define int16?
    (lambda (x)
      (and (and (integer? x) (exact? x))
           (<= (- (expt 2 15)) x (- (expt 2 15) 1)))))

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
  '(+ - * logand logor sra ash quotient remainder))
(define binop?
  (lambda (x)
    (memq x binops)))
(define commutative?
  (lambda (x)
    (memq x '(+ * logand logor))))
(define binop->pb
  (lambda (op)
    (match op
      [+ 'bin_add]
      [- 'bin_sub]
      [* 'bin_mul]
      [logand 'bin_and]
      [logor 'bin_ior]
      [sra 'bin_asr]
      [ash 'bin_lsl]
      [quotient 'bin_div]
      [remainder 'bin_rem])))

(define overflow?
  (lambda (x)
    (and (integer? x)
         (exact? x)
         (not (fixnum-range? x)))))

(define relops
  '(= < <= > >=))
(define relop?
  (lambda (x) (memq x relops)))
(define rel->pb
  (lambda (rel)
    (match rel
      [= 'cmp_eq]
      [< 'cmp_lt]
      [<= 'cmp_le]
      [> 'cmp_gt]
      [>= 'cmp_ge])))
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

(define trivial?
    (lambda (v)
      (or (uvar? v) (label? v) (integer? v))))

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
  `(+ - * quotient remainder car cdr cons make-vector vector-length vector-ref void make-procedure procedure-ref procedure-code integer->char char->integer read-char ,@(if *continuation-enabled* '(call-with-current-continuation) '())))
(define side-effect-primitives
  '(read-char ,@(if *continuation-enabled* '(call-with-current-continuation) '())))
(define predicate-primitives
  '(<= < = >= > boolean? eq? fixnum? null? pair? vector? procedure? symbol? char=? char?))
(define effect-primitives
  '(set-car! set-cdr! vector-set! procedure-set! inspect write display))
(define value-primitive?
  (lambda (x) (memq x value-primitives)))
(define predicate-primitive?
  (lambda (x) (memq x predicate-primitives)))
(define effect-primitive?
  (lambda (x) (memq x effect-primitives)))
(define primitive?
  (lambda (x) (or (value-primitive? x) (predicate-primitive? x) (effect-primitive? x))))
(define side-effect-primitive?
  (lambda (x) (memq x side-effect-primitives)))

(define user-primitive
  `([+             . 2]
    [-             . 2]
    [*             . 2]
    [quotient      . 2]
    [remainder     . 2]
    [car           . 1]
    [cdr           . 1]
    [cons          . 2]
    [make-vector   . 1]
    [vector-length . 1]
    [vector-ref    . 2]
    [void          . 0]
    [char->integer . 1]
    [integer->char . 1]
    [<=            . 2]
    [<             . 2]
    [=             . 2]
    [>=            . 2]
    [>             . 2]
    [char=?        . 2]
    [boolean?      . 1]
    [eq?           . 2]
    [fixnum?       . 1]
    [null?         . 1]
    [pair?         . 1]
    [procedure?    . 1]
    [vector?       . 1]
    [symbol?       . 1]
    [char?         . 1]
    [set-car!      . 2]
    [set-cdr!      . 2]
    [vector-set!   . 3]
    ,@(if *continuation-enabled* '([call-with-current-continuation . 1]) '())))
(define user-primitive?
  (lambda (x) (assq x user-primitive)))
(define user-primitive->arity
  (lambda (x) (cdr (assq x user-primitive))))

(define aux-keywords
  '(else unquote))
(define aux-keyword?
  (lambda (x) (memq x aux-keywords)))
(define syntax-keywords
  '(quote quasiquote if and or not cond begin set! lambda let letrec))
(define syntax-keyword?
  (lambda (x) (memq x syntax-keywords)))

(define-who parse-scheme
  (define check-bind-variable
    (lambda (var* e)
      (cond [(not (for-all symbol? var*))
             (format-error who "invalid bound variable in ~s" e)]
            [(not (set? var*))
             (format-error who "duplicate bound variable in ~s" e)]
            [else (void)])))
  (define check-parameter
    (lambda (var* e)
      (cond [(not (for-all symbol? var*))
             (format-error who "invalid parameter list in ~s" e)]
            [(not (set? var*))
             (format-error who "invalid parameter list in ~s" e)]
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
            [(char? d) (void)]
            [else (format-error who "invalid datum ~s" d)])))
  (define convert-and
    (lambda (rand*)
      (cond [(null? rand*) '(quote #t)]
            [(null? (cdr rand*)) (car rand*)]
            [else `(if ,(car rand*)
                       ,(convert-and (cdr rand*))
                       (quote #f))])))
  (define convert-or
    (lambda (rand*)
      (cond [(null? rand*) '(quote #f)]
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
                        (if (eq? (cdr clause*) '())
                            (make-begin body*)
                            (format-error who "misplaced aux keyword else"))
                        `(if ,((Expr env) cond)
                             ,(make-begin body*)
                             ,(convert-cond (cdr clause*) env))))])))
  (define convert-quasiquote
    (lambda (qd env level)
      (define unchanged
        (lambda (qd expr)
          (and (pair? expr)
               (eq? (car expr) 'quote)
               (eq? qd (cadr expr)))))
      (cond [(and (pair? qd)
                  (eq? (car qd) 'unquote)
                  (not (assq 'unquote env))
                  (= level 0))
             ((Expr env) (cadr qd))]
            [(pair? qd)
             (let* ([a (car qd)]
                    [d (cdr qd)]
                    [level^ (cond [(eq? a 'quasiquote) (add1 level)]
                                  [(eq? a 'unquote) (sub1 level)]
                                  [else level])]
                    [a^ (convert-quasiquote a env level)]
                    [d^ (convert-quasiquote d env level^)])
               (if (and (unchanged a a^) (unchanged d d^))
                   `(quote ,qd)
                   `(cons ,a^ ,d^)))]
            [(vector? qd)
             (let ([qd^ (vector-map (lambda (qd) (convert-quasiquote qd env level)) qd)]
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
                        ,(make-begin `(,@fill ,tmp))))))]
            [else
              (check-datum qd)
              `(quote ,qd)])))
  (define Var
    (lambda (env)
      (lambda (var)
        (cond [(assq var env) => cdr]
              [(user-primitive? var) var]
              [(syntax-keyword? var) (format-error who "invalid syntax ~s" var)]
              [(aux-keyword? var) (format-error who "misplaced aux keyword ~s" var)]
              [else (format-error who "variable ~s is not bound" var)]))))
  (define Expr
    (lambda (env)
      (lambda (x)
        (match x
          [#t '(quote #t)]
          [#f '(quote #f)]
          [,n (guard (integer? x) (exact? x) (fixnum-range? x)) `(quote ,n)]
          [,ch (guard (char? ch)) `(quote ,ch)] ; ascii only
          [call/cc (guard *continuation-enabled*) 'call-with-current-continuation]
          [,var (guard (symbol? var)) ((Var env) var)]
          [(,proc ,[(Expr env) -> arg*] ...) (guard (assq proc env))
           `(,((Expr env) proc) ,arg* ...)]
          [(call/cc ,rand* ...) (guard *continuation-enabled*)
           ((Expr env) `(call-with-current-continuation ,rand* ...))]
          [(,prim ,[(Expr env) -> rand*] ...) (guard (user-primitive? prim))
           (if (= (user-primitive->arity prim) (length rand*))
               `(,prim ,rand* ...)
               (format-error who "incorrect argument count in call ~s" x))]
          [(quote ,datum)
           (check-datum datum)
           `(quote ,datum)]
          [(quasiquote ,quasidatum) (convert-quasiquote quasidatum env 0)]
          [(if ,[(Expr env) -> cond] ,[(Expr env) -> conseq])
           `(if ,cond ,conseq (void))]
          [(if ,[(Expr env) -> cond] ,[(Expr env) -> conseq] ,[(Expr env) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(and ,[(Expr env) -> rand*] ...) (convert-and rand*)]
          [(or ,[(Expr env) -> rand*] ...) (convert-or rand*)]
          [(not ,[(Expr env) -> rand]) `(if ,rand (quote #f) (quote #t))]
          [(cond ,clause* ...)
           (if (<= (length clause*) 0)
               (format-error who "invalid syntax ~s" x)
               (convert-cond clause* env))]
          [(begin ,[(Expr env) -> expr*] ... ,[(Expr env) -> expr])
           (make-begin `(,expr* ... ,expr))]
          [(set! ,var ,[(Expr env) -> expr])
           (if (not (symbol? var))
               (format-error who "invalid syntax ~s" x)
               (if (not (assq var env))
                   (if (user-primitive? var)
                       (format-error who "attemp to assign immutable variable ~s" var)
                       (format-error who "variable ~s is not bound" var))
                   `(set! ,((Var env) var) ,expr)))]
          [(lambda (,formal* ...) ,expr* ...)
           (if (<= (length expr*) 0) (format-error who "invalid syntax ~s" x)) ; can improve
           (check-parameter formal* x)
           (let* ([uvar* (map unique-name formal*)]
                  [env^ (append (map cons formal* uvar*) env)]
                  [body* (map (Expr env^) expr*)])
             `(lambda (,uvar* ...)
                ,(make-begin `(,body* ...))))]
          [(let ([,var* ,[(Expr env) -> expr*]] ...) ,body* ...)
           (if (<= (length body*) 0) (format-error who "invalid syntax ~s" x))
           (check-bind-variable var* x)
           (let* ([uvar* (map unique-name var*)]
                  [env^ (append (map cons var* uvar*) env)]
                  [body*^ (map (Expr env^) body*)])
             `(let ([,uvar* ,expr*] ...)
                ,(make-begin `(,body*^ ...))))]
          [(letrec ([,var* ,expr*] ...) ,body* ...)
           (if (<= (length body*) 0) (format-error who "invalid syntax ~s" x))
           (check-bind-variable var* x)
           (let* ([uvar* (map unique-name var*)]
                  [env^ (append (map cons var* uvar*) env)]
                  [expr*^ (map (Expr env^) expr*)]
                  [body*^ (map (Expr env^) body*)])
             `(letrec ([,uvar* ,expr*^] ...)
                ,(make-begin `(,body*^ ...))))]
          [(,[(Expr env) -> proc] ,[(Expr env) -> arg*] ...) `(,proc ,arg* ...)]
          [,x (format-error who "invalid expression ~s" x)]))))
  (lambda (p) ((Expr '()) p)))

(define-who proceduralize-primitives
  (define bindings)
  (define Expr
    (lambda (e)
      (match e
        [,prim (guard (user-primitive? prim))
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
        [(quote ,imm) `(quote ,imm)]
        [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
        [,x (guard (uvar? x)) x])))
  (lambda (p)
    (set! bindings '())
    (let ([p^ (Expr p)])
      (make-letrec (map cdr bindings) p^))))

(define-who convert-complex-datum
  (define bindings)
  (define fillings)
  (define size
    (lambda (d)
      (cond [(pair? d) (add1 (+ (size (car d)) (size (cdr d))))]
            [(vector? d) (add1 (apply + (vector->list (vector-map size d))))]
            [else 1])))
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
        [(quote ,imm)
         (if (and (or (pair? imm) (vector? imm)))
             (if (or (not *max-inline-literal-size*) (< (size imm) *max-inline-literal-size*))
                 (let ([imm^ (do-conversion imm)])
                   (cond [(and (pair? imm))
                          (let ([tmp (unique-name 'complex)])
                            (set! bindings (cons `(,tmp ,imm^) bindings))
                            tmp)]
                         [(vector? imm)
                          (set! bindings (cons (caadr imm^) bindings))
                          (set! fillings (append (remove-last (cdaddr imm^)) fillings))
                          (caaadr imm^)]))
                 (let ([tmp (unique-name 'complex)])
                   (set! bindings (cons (list tmp (list 'quote imm)) bindings))
                   tmp))
             `(quote ,imm))]
        [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
        [,x (guard (uvar? x)) x]
        [,lab (guard (label? lab)) lab])))
  (lambda (p)
    (set! bindings '())
    (set! fillings '())
    (make-let bindings
      (make-begin `(,@fillings ,(Expr p))))))

(define optimize-direct-call
  (lambda (e)
    (match e
      [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
      [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
      [(let ([,uvar* ,[body*]] ...) ,[body]) `(let ([,uvar* ,body*] ...) ,body)]
      [(letrec ([,uvar* ,[body*]] ...) ,[body]) `(letrec ([,uvar* ,body*] ...) ,body)]
      [(lambda (,uvar* ...) ,[body]) `(lambda (,uvar* ...) ,body)]
      [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
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
        [(quote ,imm) (values e '())]
        [(,[Expr -> proc a] ,[Expr -> arg* a*] ...)
         (values `(,proc ,arg* ...) (apply union a a*))]
        [,x (guard (uvar? x)) (values x '())])))
  (lambda (p)
    (match p
      [,[Expr -> e a] e])))

(define-who purify-letrec ; see ``fixing letrec''
  (define simple?
    (lambda (x* no-capture? no-effect?)
      (lambda (e)
        (match e
          [(if ,cond ,conseq ,alter)
           (and ((simple? x* no-capture? no-effect?) cond)
                ((simple? x* no-capture? no-effect?) conseq)
                ((simple? x* no-capture? no-effect?) alter))]
          [(begin ,expr* ... ,expr)
           (for-all (simple? x* no-capture? no-effect?) (cons expr expr*))]
          [(let ([,uvar* ,expr*] ...) (assigned (,as* ...) ,expr))
           (for-all (simple? x* no-capture? no-effect?) (cons expr expr*))]
          [(letrec ([,uvar* ,expr*] ...) ,body) ((simple? x* no-capture? no-effect?) body)]
          [(set! ,uvar ,expr)
           (if no-effect? #f
               ((simple? x* no-capture? no-effect?) expr))]
          [(lambda (,uvar* ...) (assigned (,as* ...) ,expr)) ((simple? x* #f #f) expr)]
          [(call-with-current-continuation ,expr) (guard *continuation-enabled*)
           (cond [no-effect? #f]
                 [(and no-capture? (eq? *standard* 'r5rs)) #f]
                 [else ((simple? x* no-capture? no-effect?) expr)])]
          [(,prim ,rand* ...)
           (guard (or (effect-primitive? prim)
                      (side-effect-primitive? prim)))
           (if no-effect? #f (for-all (simple? x* no-capture? no-effect?) rand*))]
          [(,prim ,rand* ...) (guard (primitive? prim))
           (for-all (simple? x* no-capture? no-effect?) rand*)]
          [(quote ,imm) #t]
          [(,proc ,arg* ...)
           (cond [no-effect? #f]
                 [(and no-capture? (eq? *standard* 'r5rs)) #f]
                 [else (for-all (simple? x* no-capture? no-effect?) (cons proc arg*))])]
          [,x (guard (uvar? x)) (not (memq x x*))]))))
  (define effect-free? (simple? '() #f #t))
  (define partition
    (lambda (x* b* as* body-lambda? no-effect?)
      (let loop ([x* x*] [b* b*] [as* as*] [simple* '()] [lambda* '()] [complex* '()])
        (if (null? b*)
            (values as* simple* lambda* complex*)
            (let* ([b (car b*)]
                   [x (car b)]
                   [e (cadr b)])
              (cond
                [(and (memq x as*) (not body-lambda?))
                 (loop x* (cdr b*) as* simple* lambda* (cons b complex*))]
                [(and (not (memq x as*)) (lambda? e))
                 (loop x* (cdr b*) as* simple* (cons b lambda*) complex*)]
                [(or (not (pair? e))
                     (and (not (eq? (car e) 'let))
                          (not (eq? (car e) 'letrec))
                          ((simple? x* #t no-effect?) e)))
                 (loop x* (cdr b*) as* (cons b simple*) lambda* complex*)]
                [else
                  (match e
                    [(letrec ([,x*^ ,e*^] ...) ,body^)
                     (loop (append x*^ x*) (cons (list x body^) (cdr b*))
                       as* simple* (append `([,x*^ ,e*^] ...) lambda*) complex*)]
                    [(let ([,x*^ ,e*^] ...) (assigned (,as*^ ...) ,body^))
                     (let-values ([(as*^^ simple*^ lambda*^ complex*^)
                                   (partition '() `([,x*^ ,e*^] ...) as*^
                                     (effect-free? body^) ; or lambda?
                                     (not (effect-free? body^)))])
                       (if (null? complex*^)
                           (loop (append x*^ x*) (cons (list x body^) (cdr b*))
                             (append as*^^ as*^ as*)
                             (append simple*^ simple*)
                             (append lambda*^ lambda*)
                             complex*)
                           (loop x* (cdr b*) as* simple* lambda* (cons b complex*))))]
                    [,x (loop x* (cdr b*) as* simple* lambda* (cons b complex*))])]))))))
  (lambda (e)
    (match e
      [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
      [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
      [(let ([,uvar* ,[expr*]] ...) (assigned (,as* ...) ,[expr]))
       `(let ([,uvar* ,expr*] ...) (assigned (,as* ...) ,expr))]
      [(letrec ([,uvar* ,[expr*]] ...) (assigned (,as* ...) ,[body]))
       (let-values ([(as* simple* lambda* complex*)
                     (partition uvar* `((,uvar* ,expr*) ...) as* (effect-free? body) #f)])
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
           (make-let-assigned simple*
             (intersection (map car simple*) as*)
             (make-let-assigned `([,c-x* (void)] ...)
               c-x*
               (make-letrec lambda*
                 innermost)))))]
      [(set! ,uvar ,[expr]) `(set! ,uvar ,expr)]
      [(lambda (,uvar* ...) (assigned (,as* ...) ,[expr]))
       `(lambda (,uvar* ...) (assigned (,as* ...) ,expr))]
      [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
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
        [quotient      quotient]
        [remainder     remainder]
        [car           car]
        [cdr           cdr]
        [cons          cons]
        [vector-length vector-length]
        [char->integer char->integer]
        [integer->char integer->char]
        [<=            <=]
        [<             <]
        [=             =]
        [>=            >=]
        [>             >]
        [char=?        char=?]
        [boolean?      boolean?]
        [eq?           eq?]
        [fixnum?       fixnum?]
        [null?         null?]
        [pair?         pair?]
        [procedure?    procedure?]
        [vector?       vector?]
        [symbol?       symbol?]
        [char?         char?]
        [,x            #f])))
  (define complex?
    (lambda (im)
      (not (or (null? im)
               (symbol? im)
               (char? im)
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

(define-who convert-assignments
  (define Expr
    (lambda (assigned)
      (lambda (e)
        (match e
          [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
          [(begin ,[expr*] ... ,[expr]) `(begin ,expr* ... ,expr)]
          [(let ([,uvar* ,[expr*]] ...) (assigned (,as* ...) ,expr))
           (let* ([new-t* (map unique-name as*)]
                  [correspond (map cons as* new-t*)]
                  [subst (lambda (x) (cond [(assq x correspond) => cdr]
                                           [else x]))]
                  [new-bind* (map subst uvar*)])
             (make-let `([,new-bind* ,expr*] ...)
               (make-let `([,as* (cons ,new-t* (void))] ...)
                 ((Expr (append as* assigned)) expr))))]
          [(letrec ([,uvar* ,[expr*]] ...) ,[expr]) `(letrec ([,uvar* ,expr*] ...) ,expr)]
          [(set! ,uvar ,[expr])
           (if (memq uvar assigned) `(set-car! ,uvar ,expr) `(set! ,uvar ,expr))]
          [(lambda (,uvar* ...) (assigned (,as* ...) ,expr))
           (let* ([new-t* (map unique-name as*)]
                  [correspond (map cons as* new-t*)]
                  [subst (lambda (x) (cond [(assq x correspond) => cdr]
                                           [else x]))]
                  [new-bind* (map subst uvar*)])
             `(lambda (,new-bind* ...)
                ,(make-let `([,as* (cons ,new-t* (void))] ...)
                   ((Expr (append as* assigned)) expr))))]
          [(,prim ,[rand*] ...) (guard (primitive? prim)) `(,prim ,rand* ...)]
          [(quote ,imm) `(quote ,imm)]
          [(,[proc] ,[arg*] ...) `(,proc ,arg* ...)]
          [,x (guard (uvar? x)) (if (memq x assigned) `(car ,x) x)]))))
  (lambda (p) ((Expr '()) p)))

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
        [(quote ,imm) `(quote ,imm)]
        [(,[Expr -> proc] ,[Expr -> arg*] ...) `(,proc ,arg* ...)]
        [,x x])))
  Expr)

(define sanitize-binding-forms
  (lambda (e)
    (match e
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
        [(quote ,imm) (values e '())]
        [(,[Expr -> proc f] ,[Expr -> arg* f*] ...)
         (values `(,proc ,arg* ...) (apply union f f*))]
        [,x (guard (uvar? x)) (values x (list x))])))
  (lambda (p)
    (let-values ([(p^ fv) (Expr p)]) p^)))

(define-who convert-closures
  (define Expr
    (lambda (e)
      (match e
        [(if ,[Expr -> cond] ,[Expr -> conseq] ,[Expr -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Expr -> expr*] ... ,[Expr -> expr])
         `(begin ,expr* ... ,expr)]
        [(let ([,uvar* ,[Expr -> body*]] ...) ,[Expr -> body])
         `(let ([,uvar* ,body*] ...) ,body)]
        [(letrec ([,uvar* (lambda (,formal** ...)
                            (free (,fv** ...) ,[Expr -> body*]))] ...)
           ,[Expr -> body])
         (let ([cp* (map (lambda (x) (unique-name 'cp)) uvar*)]
               [lab* (map unique-label uvar*)])
           `(letrec ([,lab* (lambda (,cp* ,formal** ...)
                              (bind-free (,cp* ,fv** ...) ,body*))] ...)
              (closures ([,uvar* ,lab* ,fv** ...] ...) ,body)))]
        [(,prim ,[Expr -> rand*] ...) (guard (primitive? prim))
         `(,prim ,rand* ...)]
        [(quote ,imm) e]
        [(,[Expr -> proc] ,[Expr -> arg*] ...)
         (if (uvar? proc)
             `(,proc ,proc ,arg* ...)
             (let ([tmp (unique-name 'tmp)])
               `(let ([,tmp ,proc])
                  (,tmp ,tmp ,arg* ...))))]
        [,x (guard (uvar? x)) x])))
  Expr)

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
          [(quote ,imm) e]
          [(,proc ,[(Expr known) -> arg*] ...)
           (if (memq proc known)
               `(,(unique-label proc) ,arg* ...)
               `(,proc ,arg* ...))]
          [,x (guard (uvar? x)) x]))))
  (lambda (p)
    (if *closure-optimization-enabled*
        ((Expr '()) p)
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
        [(quote ,imm) (values `(quote ,imm) '())]
        [(,[proc u] ,cp ,[arg* u*] ...)
         (values `(,proc ,cp ,arg* ...) (apply union u u*))]
        [,x (guard (uvar? x)) (values x (list x))]
        [,lab (guard (label? lab)) (values lab '())])))
  (lambda (p)
    (if *closure-optimization-enabled*
        (let-values ([(p^ u) (Expr p)])
          p^)
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
    (lambda (wk-uvar)
      (lambda (e)
        (match e
          [(letrec ([,lab* ,lam*] ...)
             (closures (,cls* ...)
               (well-known (,wk* ...) ,body)))
           (let*-values ([(wk-cls* oth-cls*0 wk-uvar^)
                          (partition cls* '() (append wk* wk-uvar))]
                         [(wk-lab^)
                          (map (lambda (cls) (cadr cls)) wk-cls*)]
                         [(oth-cls*)
                          (map (lambda (cls)
                                 (cons (car cls)
                                   (cons (cadr cls)
                                     (difference (cddr cls) wk-uvar^))))
                            oth-cls*0)]
                         [(lam*^) (map (Lambda wk-uvar^ wk-lab^) lab* lam*)])
             `(letrec ([,lab* ,lam*^] ...)
                (closures (,oth-cls* ...)
                  (well-known ,(difference wk* wk-uvar^)
                    ,((Expr wk-uvar^) body)))))]
          [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
          [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
          [(let ([,x* ,[e*]] ...) ,[body]) `(let ([,x* ,e*] ...) ,body)]
          [(,prim ,[rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(quote ,imm) `(quote ,imm)]
          [(,[proc] ,cp ,[arg*] ...)
           (if (memq cp wk-uvar)
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
                  ,((Expr wk-uvar) body))))]))))
  (lambda (p)
    (if *closure-optimization-enabled* ((Expr '()) p) p)))

(define-who optimize-self-reference
  (define Expr
    (lambda (self cp)
      (lambda (expr)
        (match expr
          [(letrec ([,lab* ,lam*] ...)
             (closures ([,f* ,code* ,[fv**] ...] ...)
               (well-known ,wk* ,[body])))
           (let* ([lab->self `((,code* . ,f*) ...)]
                  [fv**^ (map (lambda (f fv*) (remq f fv*)) f* fv**)]
                  [lam*^ (map (lambda (lab lam)
                                (cond [(assq lab lab->self) =>
                                       (lambda (self) ((Lambda (cdr self)) lam))]
                                      [else ((Lambda #f) lam)]))
                           lab* lam*)])
             `(letrec ([,lab* ,lam*^] ...)
                (closures ([,f* ,code* ,fv**^ ...] ...)
                  (well-known ,wk* ,body))))]
          [(if ,[cond] ,[conseq] ,[alter]) `(if ,cond ,conseq ,alter)]
          [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
          [(let ([,x* ,[e*]] ...) ,[body]) `(let ([,x* ,e*] ...) ,body)]
          [(,prim ,[rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
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
  (lambda (p) (if *closure-optimization-enabled* ((Expr #f #f) p) p)))

(define-who introduce-procedure-primitives
  (define closure-length) ; actually free-variable length
  (define index-of
    (lambda (x fvs)
      (let loop ([fv fvs] [i 0])
        (cond [(null? fv) #f]
              [(eq? (car fv) x) i]
              [else (loop (cdr fv) (add1 i))]))))
  (define Lambda
    (lambda (wk-lab*)
      (lambda (lab l)
        (match l
          [(lambda (,formal* ...)
             (bind-free (,cp ,fv* ...) ,body))
           `(lambda (,formal* ...) ,((Expr cp fv* (memq lab wk-lab*)) body))]))))
  (define Closure
    (lambda (cp fvs wk? wk*)
      (lambda (c)
        (match c
          [(,uvar ,lab ,[(Expr cp fvs wk?) -> fv*] ...)
           (cond
             [(and (eq? (length fv*) 1) (memq uvar wk*))
              (values
                '()
                '(void)
                uvar lab
                `((,uvar ,(car fv*))))]
             [(and (eq? (length fv*) 2) (memq uvar wk*))
              (values
                `((,uvar (cons (void) (void))))
                `(begin
                   (set-car! ,uvar ,(car fv*))
                   (set-cdr! ,uvar ,(cadr fv*)))
                uvar lab '())]
             [else
               (set! closure-length (cons (list lab (ash (length fv*) align-shift)) closure-length))
               (let ([set-free (let loop ([fv fv*] [i 0])
                                 (cond [(null? fv) '()]
                                       [else (cons `(procedure-set! ,uvar (quote ,i) ,(car fv))
                                               (loop (cdr fv) (add1 i)))]))])
                 (values
                   `((,uvar (make-procedure ,lab (quote ,(length fv*)))))
                   (if (null? set-free) '(void) `(begin ,@set-free))
                   uvar lab '()))])]))))
  (define Expr
    (lambda (cp fvs wk?)
      (lambda (e)
        (match e
          [(if ,[(Expr cp fvs wk?) -> cond] ,[(Expr cp fvs wk?) -> conseq] ,[(Expr cp fvs wk?) -> alter])
           `(if ,cond ,conseq ,alter)]
          [(begin ,[(Expr cp fvs wk?) -> expr*] ... ,[(Expr cp fvs wk?) -> expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,uvar* ,[(Expr cp fvs wk?) -> body*]] ...) ,[(Expr cp fvs wk?) -> body])
           `(let ([,uvar* ,body*] ...) ,body)]
          [(letrec ([,lab* ,lambda*] ...)
             (closures (,[(Closure cp fvs wk? wk*) -> bind** fill* uv* lb* single**] ...)
               (well-known (,wk* ...)
                 ,[(Expr cp fvs wk?) -> body])))
           (let ([wk-lab (let loop ([uv* uv*] [lb* lb*])
                           (cond [(null? uv*) '()]
                                 [(memq (car uv*) wk*)
                                  (cons (car lb*) (loop (cdr uv*) (cdr lb*)))]
                                 [else (loop (cdr uv*) (cdr lb*))]))])
             `(letrec ([,lab* ,(map (Lambda wk-lab) lab* lambda*)] ...)
                ,(make-let (apply append bind**)
                   (make-let (apply append single**)
                     (make-begin `(,fill* ... ,body))))))]
          [(,prim ,[(Expr cp fvs wk?) -> rand*] ...) (guard (primitive? prim))
           `(,prim ,rand* ...)]
          [(quote ,imm) e]
          [(,[(Expr cp fvs wk?) -> proc] ,[(Expr cp fvs wk?) -> arg*] ...)
           (if (label? proc)            ; in the presence of optimize-known-call
               `(,proc ,arg* ...)
               `((procedure-code ,proc) ,arg* ...))]
          [,x (guard (uvar? x))
            (cond
              [(and wk? (eq? (length fvs) 1) (eq? x (car fvs)))
               cp]
              [(and wk? (eq? (length fvs) 2) (eq? x (car fvs)))
               `(car ,cp)]
              [(and wk? (eq? (length fvs) 2) (eq? x (cadr fvs)))
               `(cdr ,cp)]
              [(index-of x fvs) =>
               (lambda (i) `(procedure-ref ,cp (quote ,i)))]
              [else x])]
          [,lab (guard (label? lab)) lab]))))
  (lambda (x)
    (set! closure-length '())
    (let ([x^ ((Expr #f '() #f) x)])
      `(with-closure-length
         ,(if *collection-enabled*
              closure-length
              '())
         ,x^))))

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
        [(quote ,imm) (values `(quote ,imm) '())]
        [(,[Expr -> proc b] ,[Expr -> arg* b*] ...)
         (values `(,proc ,arg* ...) (apply append b b*))]
        [,x (values x '())])))
  (lambda (p)
    (match p
      [(with-closure-length ,closure-length
         ,[Expr -> body binds])
       `(with-closure-length ,closure-length
          (letrec ,binds ,body))])))

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
        [(,prim ,[Value -> rand*] ...) (guard (side-effect-primitive? prim))
         `(,prim ,rand* ...)]
        [(,prim ,[Effect -> rand*] ...) (guard (value-primitive? prim))
         (make-nopless-begin `(,rand* ...))]
        [(,prim ,[Effect -> rand*] ...) (guard (predicate-primitive? prim))
         (make-nopless-begin `(,rand* ...))]
        [(,prim ,[Value -> rand*] ...) (guard (effect-primitive? prim))
         `(,prim ,rand* ...)]
        [(,[Value -> proc] ,[Value -> arg*] ...)
         `(,proc ,arg* ...)]
        [,lab (guard (label? lab)) '(nop)]
        [,uvar (guard (uvar? uvar)) '(nop)]
        [,x (format-error who "invalid expression ~s in effect context" x)])))
  (lambda (p)
    (match p
      [(with-closure-length ,closure-length
         (letrec ([,lab* (lambda (,uvar* ...) ,[Value -> body*])] ...)
           ,[Value -> body]))
       `(with-closure-length ,closure-length
          (letrec ([,lab* (lambda (,uvar* ...) ,body*)] ...)
            ,body))])))

(define-who expose-library-procedures
  (define lib-prim*
    `(,@(if *continuation-enabled* '(call-with-current-continuation) '()) inspect write display read-char))
  (define lib-proc*
    (map
      (lambda (p)
        (string-append "_scheme_"
          (let* ([s (string-copy (symbol->string p))] ; value of symbol->string is immutable
                 [len (string-length s)])
            (let loop ([i 0])
              (cond
                [(= i len) s]
                [(char=? (string-ref s i) #\-)
                 (string-set! s i #\_)
                 (loop (add1 i))]
                [else (loop (add1 i))])))))
      lib-prim*))
  (define lib-prim?
    (lambda (x) (memq x lib-prim*)))
  (define lib-lab*)
  (define lib-prim-lab*)
  (define Expr
    (lambda (e)
      (match e
        [(if ,[Expr -> cond] ,[Expr -> conseq] ,[Expr -> alter])
         `(if ,cond ,conseq ,alter)]
        [(begin ,[Expr -> effect*] ... ,[Expr -> expr])
         `(begin ,effect* ... ,expr)]
        [(let ([,uvar* ,[Expr -> value*]] ...) ,[Expr -> expr])
         `(let ([,uvar* ,value*] ...) ,expr)]
        [(,prim ,[Expr -> rand*] ...) (guard (lib-prim? prim))
         `(,(cdr (assq prim lib-prim-lab*)) ,rand* ...)]
        [(,prim ,[Expr -> rand*] ...) (guard (primitive? prim))
         `(,prim ,rand* ...)]
        [(true) e]
        [(false) e]
        [(nop) e]
        [(quote ,datum) e]
        [(,[Expr -> proc] ,[Expr -> arg*] ...) `(,proc ,arg* ...)]
        [,x x])))
  (lambda (p)
    (set! lib-lab* (map unique-label lib-prim*))
    (set! lib-prim-lab* (map cons lib-prim* lib-lab*))
    (match p
      [(with-closure-length ,closure-length
         (letrec ([,label* (lambda (,uvar* ...) ,[Expr -> body*])] ...)
           ,[Expr -> body]))
       `(with-label-alias ([,lib-lab* ,lib-proc*] ...)
          (with-closure-length ,closure-length
            (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...)
              ,body)))])))

(define-who specify-representation
  (define decode-literal-label)
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

  (define trivialize
    (lambda (k . e*)
      (let* ([t* (map (lambda (e) (if (trivial? e) #f (unique-name 'tmp))) e*)]
             [e*^ (map (lambda (e t) (or t e)) e* t*)]
             [bd* (map (lambda (e t) (if t (list (list t e)) '())) e* t*)])
        (make-let (apply append bd*)
          (apply k e*^)))))

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
        [(+ ,[Value -> rand1] ,[Value -> rand2]) `(+ ,rand1 ,rand2)]
        [(- ,[Value -> rand1] ,[Value -> rand2]) `(- ,rand1 ,rand2)]
        [(* ,[Value -> rand1] ,[Value -> rand2])
         (cond [(integer? rand1) `(* ,(sra rand1 shift-fixnum) ,rand2)]
               [(integer? rand2) `(* ,rand1 ,(sra rand2 shift-fixnum))]
               [else ; sra is evil
                 (trivialize
                   (lambda (rand1)
                     (trivialize
                       (lambda (rand2)
                         `(* ,rand1 ,rand2))
                       `(sra ,rand2 ,shift-fixnum)))
                   rand1)])]
        [(quotient ,[Value -> rand1] ,[Value -> rand2]) `(ash (quotient ,rand1 ,rand2) ,shift-fixnum)]
        [(remainder ,[Value -> rand1] ,[Value -> rand2]) `(remainder ,rand1 ,rand2)]
        [(car ,[Value -> pair]) `(mref ,pair ,offset-car)]
        [(cdr ,[Value -> pair]) `(mref ,pair ,offset-cdr)]
        [(char->integer ,[Value -> ch])
         (if (integer? ch)
             (ash (sra ch shift-char) shift-fixnum)
             `(ash (sra ,ch ,shift-char) ,shift-fixnum))]
        [(integer->char ,[Value -> n])
         (if (integer? n)
             (+ tag-char (ash n (- shift-char shift-fixnum)))
             `(+ ,tag-char (ash ,n ,(- shift-char shift-fixnum))))]
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
        [(cons ,[Value -> e1] ,[Value -> e2])
         (trivialize
           (lambda (e1^ e2^)
             (let ([tmp (unique-name 'tmp)])
               `(let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                  (begin (mset! ,tmp ,offset-car ,e1^)
                         (mset! ,tmp ,offset-cdr ,e2^)
                         ,tmp))))
           e1 e2)]
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
        [(,[Value -> proc] ,[Value -> arg*] ...)
         (apply trivialize ; instruction address is evil
           (lambda arg*
             (trivialize
               (lambda (proc)
                 `(,proc ,arg* ...))
               proc))
           arg*)]
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
        [(char=? ,[Value -> rand1] ,[Value -> rand2]) `(= ,rand1 ,rand2)]
        [(null? ,[Value -> e]) `(= ,e ,$nil)]
        [(boolean? ,[Value -> e]) `(= (logand ,e ,mask-boolean) ,tag-boolean)]
        [(fixnum? ,[Value -> e]) `(= (logand ,e ,mask-fixnum) ,tag-fixnum)]
        [(pair? ,[Value -> e]) `(= (logand ,e ,mask-pair) ,tag-pair)]
        [(procedure? ,[Value -> e]) `(= (logand ,e ,mask-procedure) ,tag-procedure)]
        [(vector? ,[Value -> e]) `(= (logand ,e ,mask-vector) ,tag-vector)]
        [(symbol? ,[Value -> e]) `(= (logand ,e ,mask-symbol) ,tag-symbol)]
        [(char? ,[Value -> e]) `(= (logand ,e ,mask-char) ,tag-char)]
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
        [(nop) '(nop)]
        [(,[Value -> proc] ,[Value -> arg*] ...)
         (apply trivialize
           (lambda arg*
             (trivialize
               (lambda (proc)
                 `(,proc ,arg* ...))
               proc))
           arg*)])))
  (define Immediate
    (lambda (i)
      (cond [(eq? i #t) $true]
            [(eq? i #f) $false]
            [(eq? i '()) $nil]
            [(symbol? i)
             (cond [(assq i symbol->index) =>
                    (lambda (index) (+ tag-symbol (cdr index)))]
                   [else (let ([index current-dump-length])
                           (set! symbol->index (cons (cons i index) symbol->index))
                           (set! current-dump-length
                             (+ current-dump-length
                               8 ; 8 bit length + 8 bit characters
                               (* 8 (string-length (symbol->string i)))))
                           (+ tag-symbol index))])]
            [(integer? i) (ash i shift-fixnum)]
            [(char? i) (+ (ash (char->integer i) shift-char) tag-char)]
            [else
              (let ([complex (unique-label 'code)])
                (set! complex-datum-label*
                  (cons
                    (list complex (specify-complex i))
                    complex-datum-label*))
                `(,decode-literal-label ,complex))])))
  (lambda (p)
    (set! decode-literal-label (unique-label 'decode-literal-label))
    (set! current-dump-length 0)
    (set! symbol->index '())
    (set! complex-datum-label* '())
    (match p
      [(with-label-alias (,alias* ...)
         (with-closure-length ,closure-length
           (letrec ([,label* (lambda (,uvar* ...) ,[Value -> body*])] ...)
             ,[Value -> body])))
       (match complex-datum-label*
         [([,complex* ,code*] ...)
          `(with-label-alias ([,decode-literal-label "_scheme_decode_literal"]
                              ,alias* ...)
             (with-global-data ([symbol-dump ,(map (lambda (x) (list 'quote (car x)))
                                                (reverse symbol->index))]
                                [,complex* (encode-literal (quote ,code*))] ...)
               (with-closure-length ,closure-length
                 (letrec ([,label* (lambda (,uvar* ...) ,body*)] ...)
                   ,body))))])])))

(define-syntax match-with-default-wrappers
  (let ()
    (define bend
      (lambda (qq x)
        (syntax-case x (quasiquote)
          [quasiquote qq]
          [(a . d) #`(#,(bend qq #'a) . #,(bend qq #'d))]
          [x #'x])))
    (lambda (x)
      (syntax-case x ()
        [(k x [p r])
         (with-syntax ([(la gd cl) #'(la gd cl)]
                       [qq #'quasiquote])
           #`(match x
               [(with-label-alias ,la
                  (with-global-data ,gd
                    (with-closure-length ,cl
                      p)))
                `(with-label-alias ,la
                   (with-global-data ,gd
                     (with-closure-length ,cl
                       ,#,(bend #'qq #'r))))]))]))))

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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body)])))

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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body)])))

(define verify-uil (lambda (x) x))

(define-who remove-complex-opera* ; can improve
  (define fresh-locals)
  (define introduce-local
    (lambda (x)
      (set! fresh-locals (cons x fresh-locals))))
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
  (define process-procedure-call ; instruction address is evil
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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body)])))

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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body)])))

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
                      (begin
                        (set! ,tmp (- ,tmp ,allocation-pointer-register))
                        (,collect-label ,tmp))
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
             (values t expr expr)
             (values (wrap-with t expr) 0 0))]
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
             (let ([prologue (fetch-arguments parameter parameter-registers)])
               `(locals (,uvar* ... ,parameter ... ,@(apply append (map cdr new-frames)))
                  (new-frames ,new-frames
                    (begin ,@prologue ,tail))))])))))
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
                     (,proc
                       ,frame-pointer-register
                       ,allocation-pointer-register
                       ,@(if *continuation-enabled* (list stack-base-register) '())
                       ,@(if *collection-enabled* (list end-of-allocation-register) '())
                       ,@(map cadr fill-register) ,@(map cadr fill-frame))))]
          [,expr `(begin (set! ,return-value-register ,expr)
                         (return
                           ,frame-pointer-register
                           ,return-value-register
                           ,allocation-pointer-register
                           ,@(if *continuation-enabled* (list stack-base-register) '())
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
                       (,proc
                         ,frame-pointer-register
                         ,allocation-pointer-register
                         ,@(if *continuation-enabled* (list stack-base-register) '())
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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,[(Body '()) -> body])
       (let ([body*^ (map (lambda (p b) ((Body p) b)) uvar* body*)])
         `(letrec ([,label* (lambda () ,body*^)] ...) ,body))])))

(define-who expose-allocation-pointer
  (define Body
    (lambda (b)
      (match b
        [(locals (,uvar* ...)
           (new-frames (,nfv* ...) ,[Tail -> tail]))
         `(locals (,uvar* ...)
            (new-frames (,nfv* ...) ,tail))])))
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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda (,uvar* ...) ,body*)] ...) ,body)])))

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
        (match-with-default-wrappers p
          [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
           `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
    (define Body
      (lambda (b)
        (set! call-live '())
        (match b
          [(locals (,uvar* ...)
             (new-frames (,nfv* ...) ,tail))
           (set! graph
             (map (lambda (x) (list x)) uvar*))
           (let-values ([(tail^ live) (Tail tail)])
             (let ([spill (filter uvar? call-live)])
               `(locals ,(difference uvar* spill)
                  (new-frames (,nfv* ...)
                    (spills ,spill
                      (frame-conflict ,graph
                        (call-live ,call-live ,tail^)))))))])))
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
            [(return ,loc* ...)
             (values t (make-liveset loc*))]
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
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
  (define Body
    (lambda (b)
      (match b
        [(locals ,locs
           (new-frames ,nfvs
             (spills ,spill
               (frame-conflict ,frame-conflict
                 (call-live ,call-lives ,tail)))))
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
                            [else (car l)]))])
           (let ([homes^
                   (let assign ([spill spill])
                     (if (null? spill) '()
                         (let* ([now (first-except-rp spill)]
                                [home (assign (remq now spill))]
                                [conflict (subst (cdr (assq now frame-conflict)) home)])
                           (cons `(,now ,(first-available conflict)) home))))])
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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))

(define-who finalize-frame-locations
  (define Program
    (lambda (p)
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
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
          [(return ,[(Triv map) -> loc*] ...)
           `(return ,loc* ...)]
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
          [(set! rdx (sign-of rax)) e]
          [(set! (rax rdx) (quotient (rax rdx) ,[(Triv map) -> y]))
           `(set! (rax rdx) (quotient (rax rdx) ,y))]
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
  (define very-trivial?
    (lambda (x)
      (or (uvar? x) (register? x) (int16? x))))
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
        [(return ,loc* ...) (values t '())]
        [(,triv ,loc* ...)
         (if (and (not (label? triv))
                  (or (not (very-trivial? triv)) (integer? triv)))
             (let ([temp (unique-name 'u)])
               (values `(begin (set! ,temp ,triv) (,temp ,loc* ...))
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
         (cond [(not (very-trivial? x))
                (let ([temp (unique-name 'u)])
                  (let-values ([(p^ u) (Pred `(begin (set! ,temp ,x) (,rel ,temp ,y)))])
                    (values p^ (cons temp u))))]
               [(not (very-trivial? y))
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
         (cond [(or (not (very-trivial? expr)) (integer? expr))
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u)
                                (Effect `(begin (set! ,temp ,expr)
                                                (mset! ,base ,offset ,temp)))])
                    (values eff (cons temp u))))]
               [(not (very-trivial? base))
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u)
                                (Effect `(begin (set! ,temp ,base)
                                                (mset! ,temp ,offset ,expr)))])
                    (values eff (cons temp u))))]
               [(or (not (very-trivial? offset))
                    (and (integer? base) (integer? offset)))
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u)
                                (Effect `(begin (set! ,temp ,offset)
                                                (mset! ,base ,temp ,expr)))])
                    (values eff (cons temp u))))]
               [else (values e '())])]
        [(set! ,v (mref ,base ,offset))
         (cond [(frame-var? v)
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u) (Effect `(set! ,temp (mref ,base ,offset)))])
                    (values
                      (make-begin (cons eff `((set! ,v ,temp))))
                      (cons temp u))))]
               [(not (very-trivial? base))
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u)
                                (Effect `(begin (set! ,temp ,base)
                                                (set! ,v (mref ,temp ,offset))))])
                    (values eff (cons temp u))))]
               [(or (not (very-trivial? offset))
                    (and (integer? base) (integer? offset)))
                (let ([temp (unique-name 'u)])
                  (let-values ([(eff u)
                                (Effect `(begin (set! ,temp ,offset)
                                                (set! ,v (mref ,base ,temp))))])
                    (values eff (cons temp u))))]
               [else (values e '())])]
        [(set! ,v (,rator ,x ,y))
         (cond [(frame-var? v)
                (let ([temp (unique-name 'u)])
                  (let-values ([(e^ u)
                                (Effect `(begin (set! ,temp (,rator ,x ,y))
                                                (set! ,v ,temp)))])
                    (values e^ (cons temp u))))]
               [(and (int16? x)
                     (commutative? rator))
                (Effect `(set! ,v (,rator ,y ,x)))]
               [(not (or (register? x) (uvar? x)))
                (let ([temp (unique-name 'u)])
                  (let-values ([(e^ u)
                                (Effect `(begin (set! ,temp ,x)
                                                (set! ,v (,rator ,temp ,y))))])
                    (values e^ (cons temp u))))]
               [(not (very-trivial? y))
                (let ([temp (unique-name 'u)])
                  (let-values ([(e^ u)
                                (Effect `(begin (set! ,temp ,y)
                                                (set! ,v (,rator ,x ,temp))))])
                    (values e^ (cons temp u))))]
               [else (values e '())])]
        [(set! ,v ,x) (cond [(and (frame-var? v)
                                  (not (or (register? x) (uvar? x))))
                             (let ([temp (unique-name 'u)])
                               (values `(begin (set! ,temp ,x) (set! ,v ,temp))
                                       (list temp)))]
                            [else (values e '())])])))
  (lambda (p)
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))

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
        (match-with-default-wrappers p
          [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
           `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
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
            [(return ,loc* ...)
             (make-liveset loc*)]
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
            [(set! rdx (sign-of rax))
             (set-cons 'rax (difference post '(rdx)))]
            [(set! (rax rdx) (quotient (rax rdx) ,y))
             (liveset-cons y (set-cons 'rdx (set-cons 'rax post)))]
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
                "no candidate for register assignment in ~s" conf)])))
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
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))

(define-who assign-registers-vanilla
  (define Program
    (lambda (p)
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
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
    (match-with-default-wrappers x
      [(letrec ([,label* (lambda () ,body*)] ...) ,body)
       (andmap all-home? `(,body ,body* ...))])))

(define-who assign-frame
  (define Program
    (lambda (p)
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
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
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
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
        [(return ,loc* ...) '(return)]
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
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
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
          [(return) t]
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
          [(set! rdx (sign-of rax)) e]
          [(set! (rax rdx) (quotient (rax rdx) ,[(Triv map) -> y]))
           `(set! (rax rdx) (quotient (rax rdx) ,y))]
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
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[(Tail 0) -> body*])] ...) ,[(Tail 0) -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
  (define Tail
    (lambda (offset)
      (lambda (t)
        (match t
          [(if ,[(Pred offset) -> pred offset^] ,conseq ,alter)
           `(if ,pred ,((Tail offset^) conseq) ,((Tail offset^) alter))]
          [(begin ,effect* ... ,tail)
           (let-values ([(effect*^ offset^) ((Effect* offset) effect*)])
             `(begin ,effect*^ ... ,((Tail offset^) tail)))]
          [(return) t]
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
          [(set! rdx (sign-of rax)) (values e offset)]
          [(set! (rax rdx) (quotient (rax rdx) ,[(Triv offset) -> y]))
           (values `(set! (rax rdx) (quotient (rax rdx) ,y)) offset)]
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
                 (format-error who "inconsistent offsets in branches of ~s" e)))]
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

(define-who prepend-jit-flags
  (define Tail
    (lambda (t)
      (if *jit-after*
          (make-begin `((jit ,*jit-after*) ,t))
          t)))
  (lambda (p)
    (match-with-default-wrappers p
      [(letrec ([,label* (lambda () ,[Tail -> body*])] ...) ,body)
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))

(define-who expose-basic-blocks
  (define frame-information)
  (define Program
    (lambda (p)
      (set! frame-information '())
      (match-with-default-wrappers p
        [(letrec ([,label* (lambda () ,[(Tail #t) -> body* block*])] ...) ,[(Tail #t) -> body block])
         (letrec ([pair (lambda (headers others)
                          (if (null? headers) '()
                              (append
                                (cons (car headers) (car others))
                                (pair (cdr headers) (cdr others)))))])
           (let ([blocks (pair `([,label* (lambda () ,body*)] ...) block*)])
             `(with-frame-information ,frame-information
                (letrec ,(append block blocks)
                  ,body))))])))
  (define Tail
    (lambda (real-tail)
      (lambda (t)
        (match t
          [(begin ,effect* ... ,[(Tail real-tail) -> tail block])
           (let-values ([(tail^ block^) ((Effect* tail) effect*)])
             (values tail^ (append block^ block)))]
          [(if ,cond ,[(Tail real-tail) -> conseq b1] ,[(Tail real-tail) -> alter b2])
           (let ([c (unique-label 'c)]
                 [a (unique-label 'a)])
             (let-values ([(tail b3) ((Pred c a) cond)])
               (values tail (append b3
                              (cons `[,c (lambda () ,conseq)]
                                (append b1
                                  (cons `[,a (lambda () ,alter)] b2)))))))]
          [(return) (values '(return) '())]
          [(,triv) (values (if real-tail `(tail-call ,triv) `(call ,triv)) '())]
          [,x (values x '())]))))
  (define Pred
    (lambda (c a)
      (lambda (p)
        (match p
          [(true) (values (list 'jump c) '())]
          [(false) (values (list 'jump a) '())]
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
           (values `(if (,rel ,x1 ,x2) (jump ,c) (jump ,a)) '())]))))
  (define Effect
    (lambda (k)
      (lambda (e)
        (match e
          [(nop) (values k '())]
          [(return-point ,label ,size ,live ,[(Tail #f) -> tail block])
           (values (make-begin `(,tail ,k)) block)]
          [(if ,cond ,conseq ,alter)
           (let ([c (unique-label 'c)]
                 [a (unique-label 'a)]
                 [j (unique-label 'j)])
             (let-values ([(c-t b1) ((Effect (list 'jump j)) conseq)]
                          [(a-t b2) ((Effect (list 'jump j)) alter)]
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
        [(if (,rel ,x ,y) (jump ,c) (jump ,a))
         `(if (,rel ,x ,y) (jump ,(trace-graph c)) (jump ,(trace-graph a)))]
        [(return) t]
        [(jump ,triv) (list 'jump (trace-graph triv))]
        [(tail-call ,triv) (list 'tail-call (trace-graph triv))])))
  (define Effect
    (lambda (e)
      (match e
        [(set! ,v1 (,rator ,v2 ,v3))
         `(set! ,v1 (,rator ,(trace-graph v2) ,(trace-graph v3)))]
        [(set! ,v1 ,v2) (guard (label? v2))
         `(set! ,v1 ,(trace-graph v2))]
        [(call ,triv) (list 'call (trace-graph triv))]
        [,x x])))
  (define build-tail
    (lambda (label t)
      (match t
        [(jump ,lab) (guard (label? lab))
         (if (eq? (trace-graph lab) label)
             (list `(,label (lambda () ,t)))
             (begin (set! graph (cons (cons label lab) graph))
                    '()))]
        [,x (list `(,label (lambda () ,x)))])))
  (define build
    (lambda (p)
      (set! graph '())
      (match-with-default-wrappers p
        [(with-frame-information ,frame-information
           (letrec ([,label* (lambda () ,body*)] ...) ,body))
         `(with-frame-information ,frame-information
            (letrec ,(apply append (map build-tail label* body*)) ,body))])))
  (lambda (p)
    (if *optimize-jumps-enabled*
        (match-with-default-wrappers (build p)
          [(with-frame-information ,frame-information
             (letrec ([,label* (lambda () ,[Tail -> body*])] ...) ,[Tail -> body]))
           `(with-frame-information ,frame-information
              (letrec ([,label* (lambda () ,body*)] ...) ,body))])
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
                                      (cond [else next]))))]
                  [cat (lambda (label* body*)
                         (cond [(null? label*) '()]
                               [else
                                 (let* ([lab (car label*)]
                                        [info
                                          (cond [else '()])]
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
          [(if (,rel ,x ,y) (jump ,[Triv -> c]) (jump ,[Triv -> a]))
           (cond [(eq? c next) `((if (not (,rel ,x ,y)) (jump ,a)))]
                 [(eq? a next) `((if (,rel ,x ,y) (jump ,c)))]
                 [else `((if (,rel ,x ,y) (jump ,c))
                         (jump ,a))])]
          [(return) (list t)]
          [(call ,triv) (list t)]
          [(tail-call ,triv) (list t)]
          [(jump ,[Triv -> triv])
           (cond [(eq? triv next) '()]
                 [else (list `(jump ,triv))])]))))
  (define Effect
    (lambda (e)
      (match e
        [(mset! ,v ,[Triv -> base] ,[Triv -> offset])
         `(mset! ,v ,base ,offset)]
        [(set! ,v (mref ,[Triv -> base] ,[Triv -> offset]))
         `(set! ,v (mref ,base ,offset))]
        [(set! ,v1 (,rator ,[Triv -> v2] ,[Triv -> v3]))
         `(set! ,v1 (,rator ,v2 ,v3))]
        [(set! ,v1 ,[Triv -> v2])
         `(set! ,v1 ,v2)]
        [(call ,[Triv -> v]) `(call ,v)]
        [(jit ,x) e])))
  (define Triv
    (lambda (triv)
      (cond [(assq triv label-alias) => cadr]
            [else triv])))
  Program)

(define rand->pb-arg
  (lambda (rand)
    (match rand
      [r0 0]
      [r1 1]
      [r2 2]
      [r3 3]
      [r4 4]
      [r5 5]
      [r6 6]
      [r7 7]
      [r8 8]
      [r9 9]
      [r10 10]
      [r11 11]
      [r12 12]
      [r13 13]
      [r14 14]
      [r15 15]
      [,x rand])))

(module (use-reverse-buffer with-reverse-buffer reverse-put)
  (define reverse-buffer)
  (define reverse-size)
  (define reverse-max)
  (define reverse-first?)
  (define reverse-omissible?)
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
      (let loop ([head #t] [c reverse-size] [buf reverse-buffer])
        (cond [(zero? c) (void)]
              [(and head
                    (reverse-omissible? (car buf))
                    (> c 1))
               (loop #t (sub1 c) (cdr buf))]
              [else
                (reverse-out (car buf))
                (loop #f (sub1 c) (cdr buf))]))
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
  (define use-reverse-buffer
    (lambda (g f p d o thunk)
      (begin
        (set! reverse-max g)
        (set! reverse-out f)
        (set! reverse-padding p)
        (set! reverse-delim d)
        (set! reverse-omissible? o)
        (reverse-init)
        (thunk)
        (reverse-sweep))))
  (define-syntax with-reverse-buffer
    (syntax-rules ()
      [(_ g f p d o expr* ...)
       (use-reverse-buffer g f p d o (lambda () expr* ...))])))

(define-who generate-portable-bytecode
  (define gathering-label)
  (define maybe-printf
    (lambda args
      (unless gathering-label
        (apply printf args))))

  (define pc)
  (define label-pc)
  (define label->pc
    (lambda (lab)
      (if gathering-label
          0
          (cadr (assq lab label-pc)))))

  (define-syntax emit
    (syntax-rules ()
      [(_ opcode)
       (begin
         (set! pc (+ 1 pc))
         (maybe-printf "  pb_mk_~a(),\n" opcode))]
      [(_ opcode rand)
       (begin
         (set! pc (+ 1 pc))
         (maybe-printf "  pb_mk_~a_~a(~a),\n" opcode
           (if (integer? rand) 'immediate 'register)
           (rand->pb-arg rand)))]
      [(_ opcode rand1 rand2)
       (begin
         (set! pc (+ 1 pc))
         (maybe-printf "  pb_mk_~a_~a(~a, ~a),\n" opcode
           (if (integer? rand2) 'immediate 'register)
           (rand->pb-arg rand1)
           (rand->pb-arg rand2)))]
      [(_ opcode rand1 rand2 rand3)
       (begin
         (set! pc (+ 1 pc))
         (maybe-printf "  pb_mk_~a_~a(~a, ~a, ~a),\n" opcode
           (if (integer? rand3) 'immediate 'register)
           (rand->pb-arg rand1)
           (rand->pb-arg rand2)
           (rand->pb-arg rand3)))]))
  (define-syntax emit-jump
    (syntax-rules ()
      [(_ pos dst)
       (begin
         (set! pc (+ 1 pc))
         (maybe-printf "  pb_mk_jmp_~a(~a),\n" pos (* 4  (- (label->pc dst) pc))))]))
  (define-syntax emit-call
    (syntax-rules ()
      [(_ opcode ?target)
       (begin
         (let ([target ?target])
           (if (label? target)
               (emit opcode (* 4  (- (label->pc target) pc)))
               (emit opcode target))))]))
  (define-syntax emit-program
    (syntax-rules ()
      [(_ code code* ...)
       (begin
         (maybe-printf "instruction_t __code[] = {\n")
         code code* ...
         (maybe-printf "};\n"))]))

  (define Program
    (lambda (p)
      (match p
        [((data ,data* ...)
          (code ,stmt* ...))
         (letrec ([emit* (lambda (s)
                           (unless (null? s)
                             (Statement (car s))
                             (emit* (cdr s))))])
           (printf "#include \"pb.h\"\n")

           (emit-static-data data*)
           (set! pc 0)
           (set! label-pc '())
           (set! gathering-label #t)
           (emit-program (emit* stmt*))

           (set! pc 0)
           (set! gathering-label #f)
           (emit-program (emit* stmt*)))])))
  (define Statement
    (lambda (s)
      (match s
        [,label (guard (or (label? label) (string? label)))
          (when gathering-label
            (set! label-pc (cons `(,label ,pc) label-pc)))]
        [(jit ,n)
         (set! pc (+ 3 pc))
         (maybe-printf "  pb_mk_jit(~a),\n" (rand->pb-arg n))]
        [(jump ,dst) (emit-jump 'always dst)]
        [(call ,dst) (emit-call 'call dst)]
        [(tail-call ,dst) (emit-call 'tcall dst)]
        [(return) (emit 'return)]
        [(if (,rel ,x ,y) (jump ,dst))
         (emit (rel->pb rel) x y)
         (emit-jump 'true dst)]
        [(if (not (,rel ,x ,y)) (jump ,dst))
         (emit (rel->pb rel) x y)
         (emit-jump 'false dst)]
        [(mset! ,base ,offset ,expr) (emit 'st expr base offset)]
        [(set! ,v (mref ,base ,offset)) (emit 'ld v base offset)]
        [(set! ,v1 (,rator ,v2 ,v3)) (emit (binop->pb rator) v1 v2 v3)]
        [(set! ,v1 ,v2)
         (cond [(disp-opnd? v1) (emit 'st v2 (disp-opnd-reg v1) (disp-opnd-offset v1))]
               [(disp-opnd? v2) (emit 'ld v1 (disp-opnd-reg v2) (disp-opnd-offset v2))]
               [(label? v2)
                (set! pc (+ 1 pc))
                (maybe-printf "  pb_mk_addr(~a, ~a),\n" (rand->pb-arg v1) (- (label->pc v2) pc))]
               [(and (integer? v2) (not (int16? v2)))
                (set! pc (+ 3 pc))
                (maybe-printf "  pb_mk_literal(~a, ~a),\n" (rand->pb-arg v1) (rand->pb-arg v2))]
               [else (emit 'mov v1 v2)])])))
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
         (printf "uptr __scheme_symbol_dump[] = {")
         (unless (null? sym*)
           (printf "0x")
           (with-reverse-buffer 8 output-byte 0 ",0x" (lambda (x) (zero? x))
             (for-each
               (lambda (sym)
                 (let ([str (symbol->string sym)])
                   (Ptr (string-length str))
                   (string-for-each
                     (lambda (ch) (Ptr (char->integer ch)))
                     (symbol->string sym))))
               sym*)))
         (printf "};\n")]
        [(,lab (encode-literal (quote ,complex)))
         (emit-label lab)
         (printf "    .quad 0x")
         (with-reverse-buffer 8 output-byte 0 ",0x" (lambda (x) (zero? x))
           (Datum complex))
         (printf "\n")])))
  (lambda (data*)
    (for-each Data data*)))

(compiler-passes '(
  parse-scheme
  proceduralize-primitives
  optimize-direct-call
  uncover-assigned
  purify-letrec
  optimize-constant
  optimize-useless
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
  introduce-procedure-primitives
  lift-letrec
  normalize-context
  expose-library-procedures
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
  prepend-jit-flags
  expose-basic-blocks
  optimize-jumps
  flatten-program
  generate-portable-bytecode
))

(load "../tests15.scm")

(define shell
  (lambda (s . args)
    (system (apply format s args))))
(shell "~a -m64 -g -c runtime.c pb.c ir.c common.c genir.c hashtbl.c select.c set.c list.c compile.c assembler.c color.c liveness.c gen_x86_64.c" (c-compiler))

(trusted-passes #t)
