; SICP exercise 5.50
;
; Use the compiler to compile the metacircular evaluator of section 4.1 and
; run this program using the register-machine simulator. (To compile more than
; one definition at a time, you can package the definitions in a begin.) The
; resulting interpreter will run very slowly because of the multiple levels of
; interpretation, but getting all the details to work is an instructive
; exercise.

; Oh boy. This is going to be so much fun!
;
; r5rs makes a return. It is required in the tests. I can probably work it out
; of this, but it will be too much work.
;
; Running the tests of the evaluator results in a whooping amount of 215 524
; instructions, 8036 pushes and a maximum stack depth of 22. If that is not
; cool, I don't know what is.
;
; Let's start with using the open-coding compiler, just because it's cooler
; that way.

(load-relative "tests/helpers/monitored-stack.scm")
(load-relative "38.scm")

; This is the code of the metacircular evaluator. Note that it has some
; modifications. They are at the end of the code and are highlighted.

(define metacircular-evaluator
  '(begin
     (define (evaluate exp env)
       (cond ((self-evaluating? exp) exp)
             ((variable? exp) (lookup-variable-value exp env))
             ((quoted? exp) (text-of-quotation exp))
             ((assignment? exp) (eval-assignment exp env))
             ((definition? exp) (eval-definition exp env))
             ((if? exp) (eval-if exp env))
             ((lambda? exp)
              (make-procedure (lambda-parameters exp)
                              (lambda-body exp)
                              env))
             ((begin? exp)
              (eval-sequence (begin-actions exp) env))
             ((cond? exp) (evaluate (cond->if exp) env))
             ((application? exp)
              (apply-procedure (evaluate (operator exp) env)
                               (list-of-values (operands exp) env)))
             (else
              (error "Unknown expression type - EVALUATE" exp))))

     (define (apply-procedure procedure arguments)
       (cond ((primitive-procedure? procedure)
              (apply-primitive-procedure procedure arguments))
             ((compound-procedure? procedure)
              (eval-sequence
                (procedure-body procedure)
                (extend-environment
                  (procedure-parameters procedure)
                  arguments
                  (procedure-environment procedure))))
             (else
              (error "Unknown procedure type - APPLY-PROCEDURE" procedure))))

     (define (list-of-values exps env)
       (if (no-operands? exps)
           '()
           (cons (evaluate (first-operand exps) env)
                 (list-of-values (rest-operands exps) env))))

     (define (eval-if exp env)
       (if (true? (evaluate (if-predicate exp) env))
           (evaluate (if-consequent exp) env)
           (evaluate (if-alternative exp) env)))

     (define (eval-sequence exps env)
       (cond ((last-exp? exps) (evaluate (first-exp exps) env))
             (else (evaluate (first-exp exps) env)
                   (eval-sequence (rest-exps exps) env))))

     (define (eval-assignment exp env)
       (set-variable-value! (assignment-variable exp)
                            (evaluate (assignment-value exp) env)
                            env)
       'ok)

     (define (eval-definition exp env)
       (define-variable! (definition-variable exp)
                         (evaluate (definition-value exp) env)
                         env)
       'ok)

     (define (self-evaluating? exp)
       (cond ((number? exp) true)
             ((string? exp) true)
             (else false)))

     (define (variable? exp) (symbol? exp))

     (define (quoted? exp)
       (tagged-list? exp 'quote))

     (define (text-of-quotation exp) (cadr exp))

     (define (tagged-list? exp tag)
       (if (pair? exp)
           (eq? (car exp) tag)
           false))

     (define (assignment? exp) (tagged-list? exp 'set!))
     (define (assignment-variable exp) (cadr exp))
     (define (assignment-value exp) (caddr exp))

     (define (definition? exp)
       (tagged-list? exp 'define))
     (define (definition-variable exp)
       (if (symbol? (cadr exp))
         (cadr exp)
         (caadr exp)))
     (define (definition-value exp)
       (if (symbol? (cadr exp))
           (caddr exp)
           (make-lambda (cdadr exp)
                        (cddr exp))))

     (define (lambda? exp) (tagged-list? exp 'lambda))
     (define (lambda-parameters exp) (cadr exp))
     (define (lambda-body exp) (cddr exp))
     (define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

     (define (if? exp) (tagged-list? exp 'if))
     (define (if-predicate exp) (cadr exp))
     (define (if-consequent exp) (caddr exp))
     (define (if-alternative exp)
       (if (not (null? (cdddr exp)))
           (cadddr exp)
           'false))

     (define (make-if predicate consequent alternative)
       (list 'if predicate consequent alternative))

     (define (begin? exp) (tagged-list? exp 'begin))
     (define (begin-actions exp) (cdr exp))
     (define (last-exp? seq) (null? (cdr seq)))
     (define (first-exp seq) (car seq))
     (define (rest-exps seq) (cdr seq))
     (define (sequence->exp seq)
       (cond ((null? seq) seq)
             ((last-exp? seq) (first-exp seq))
             (else (make-begin seq))))
     (define (make-begin seq) (cons 'begin seq))

     (define (application? exp) (pair? exp))
     (define (operator exp) (car exp))
     (define (operands exp) (cdr exp))
     (define (no-operands? ops) (null? ops))
     (define (first-operand ops) (car ops))
     (define (rest-operands ops) (cdr ops))

     (define (cond? exp) (tagged-list? exp 'cond))
     (define (cond-clauses exp) (cdr exp))
     (define (cond-else-clause? clause)
       (eq? (cond-predicate clause) 'else))
     (define (cond-predicate clause) (car clause))
     (define (cond-actions clause) (cdr clause))
     (define (cond->if exp)
       (expand-clauses (cond-clauses exp)))
     (define (expand-clauses clauses)
       (if (null? clauses)
           'false
           (let ((first (car clauses))
                 (rest (cdr clauses)))
             (if (cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last - COND->IF" clauses))
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest))))))

     (define (true? x) (not (eq? x false)))
     (define (false? x) (eq? x false))

     (define (make-procedure parameters body env)
       (list 'procedure parameters body env))
     (define (compound-procedure? p)
       (tagged-list? p 'procedure))
     (define (procedure-parameters p) (cadr p))
     (define (procedure-body p) (caddr p))
     (define (procedure-environment p) (cadddr p))

     (define (enclosing-environment env) (cdr env))
     (define (first-frame env) (car env))
     (define the-empty-environment '())

     (define (make-frame variables values)
       (cons variables values))
     (define (frame-variables frame) (car frame))
     (define (frame-values frame) (cdr frame))
     (define (add-binding-to-frame! var val frame)
       (set-car! frame (cons var (car frame)))
       (set-cdr! frame (cons val (cdr frame))))
     (define (extend-environment vars vals base-env)
       (if (= (length vars) (length vals))
           (cons (make-frame vars vals) base-env)
           (if (< (length vars) (length vals))
               (error "Too many arguments supplied" vars vals)
               (error "Too few arguments supplied" vars vals))))
     (define (lookup-variable-value var env)
       (define (env-loop env)
         (define (scan vars vals)
           (cond ((null? vars)
                  (env-loop (enclosing-environment env)))
                 ((eq? var (car vars))
                  (car vals))
                 (else (scan (cdr vars) (cdr vals)))))
         (if (eq? env the-empty-environment)
             (error "Unbound variable" var)
             (let ((frame (first-frame env)))
               (scan (frame-variables frame)
                     (frame-values frame)))))
       (env-loop env))
     (define (set-variable-value! var val env)
       (define (env-loop env)
         (define (scan vars vals)
           (cond ((null? vars)
                  (env-loop (enclosing-environment env)))
                 ((eq? var (car vars))
                  (set-car! vals val))
                 (else (scan (cdr vars) (cdr vals)))))
         (if (eq? env the-empty-environment)
             (error "Unbound variable - SET!" var)
             (let ((frame (first-frame env)))
               (scan (frame-variables frame)
                     (frame-values frame)))))
       (env-loop env))
     (define (define-variable! var val env)
       (let ((frame (first-frame env)))
         (define (scan vars vals)
           (cond ((null? vars)
                  (add-binding-to-frame! var val frame))
                 ((eq? var (car vars))
                  (set-car! vals val))
                 (else (scan (cdr vars) (cdr vals)))))
         (scan (frame-variables frame)
               (frame-values frame))))

     (define (primitive-procedure? proc)
       (tagged-list? proc 'evaluator-primitive))
     (define (primitive-implementation proc) (cadr proc))

     (define primitive-procedures
       (list (list 'car car)
             (list 'cdr cdr)
             (list 'cons cons)
             (list 'null? null?)
             (list 'pair? pair?)
             (list '= =)
             (list '+ +)
             (list '- -)
             (list '* *)
             (list '/ /)
             (list '< <)))

     (define (primitive-procedure-names)
       (map car primitive-procedures))
     (define (primitive-procedure-objects)
       (map (lambda (proc) (list 'evaluator-primitive (cadr proc)))
            primitive-procedures))

     (define (apply-primitive-procedure proc args)
       (apply-in-underlying-scheme (primitive-implementation proc) args))

     (define (setup-environment)
       (let ((initial-env
               (extend-environment (primitive-procedure-names)
                                   (primitive-procedure-objects)
                                   the-empty-environment)))
         (define-variable! 'true true initial-env)
         (define-variable! 'false false initial-env)
         initial-env))

     ; These are the additions. Those procedures are used by the evaluator.
     ; Instead of adding them as primitives, I have implemented them here for
     ; additional fun.

     (define (map proc lst)
       (if (null? lst)
           '()
           (cons (proc (car lst))
                 (map proc (cdr lst)))))

     (define (cadr lst) (car (cdr lst)))
     (define (cddr lst) (cdr (cdr lst)))
     (define (caadr lst) (car (car (cdr lst))))
     (define (caddr lst) (car (cdr (cdr lst))))
     (define (cdadr lst) (cdr (car (cdr lst))))
     (define (cdddr lst) (cdr (cdr (cdr lst))))
     (define (cadddr lst) (car (cdr (cdr (cdr lst)))))

     (define (not x) (if x false true))

     (define (length lst)
       (if (null? lst)
           0
           (+ 1 (length (cdr lst)))))

     (define the-global-environment (setup-environment))))

; The compiler also needs some modifications.
;
; First, it needs to understand let expressions.

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((names (map car (cadr exp)))
        (values (map cadr (cadr exp)))
        (body (cddr exp)))
    (cons (cons 'lambda (cons names body))
          values)))

; Here is the modified compile-exp. It just has let

(define (compile-exp exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((let? exp) (compile-exp (let->combination exp) target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile-exp (cond->if exp) target linkage))
        ((open-coded? exp) (compile-open-coded exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

; The compiler requires a bunch of additional primitive procedures.

(define (apply-primitive proc args)
  (apply (car (cdr proc)) args))

(define extra-primitives
  `((number? ,number?)
    (string? ,string?)
    (symbol? ,symbol?)
    (< ,<)
    (error ,error)
    (eq? ,eq?)
    (list ,list)
    (set-car! ,set-car!)
    (set-cdr! ,set-cdr!)
    (apply-in-underlying-scheme ,apply-primitive)))

(set! primitive-procedures (append extra-primitives primitive-procedures))
(set! the-global-environment (setup-environment))

; Since we're using the open-coding compiler, we need some additional
; operations too:

(define extra-operations `((= ,=) (+ ,+) (- ,-) (* ,*)))

; This is a little trick to count the number of instructions.
; instruction-execution-proc gets called only when the instruction is about to
; be executed, the count is genuine.

(define total-instructions 0)
(define (instruction-execution-proc inst)
  (set! total-instructions (+ total-instructions 1))
  (cdr inst))

; This is our machine:

(define machine
  (make-machine
    '(arg1 arg2 val env exp continue proc argl unev)
    (append cm-operations extra-operations)
    explicit+compile-text))

; Finally, let's compile the evaluator in it.

(compile-in-machine machine metacircular-evaluator)
