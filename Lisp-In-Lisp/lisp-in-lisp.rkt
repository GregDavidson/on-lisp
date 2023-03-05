#lang racket

; * Lisp defined in Lisp, i.e. a Lisp Meta-Circular-Evaluator
; We present a simple implementation of a simple scheme-like Lisp
; implemented with a simple Racket Program
; to aid understanding of Lisp Semantics

; By dictionary we mean a mapping from symbols to values.
; We define procedures for implementing two kinds of
; dictionaries later.

;; Special Forms and Macro Forms are processed similarly
;; The first element of the Form must be a symbol
;; Those symbols must be mapped to a special procedure

;; Special-Forms maps symbols to procedures for evaluating special forms
;; the procedures have the same signature as eval
;; i.e. they take a form and an environment and return a value
;; Eval will delegate evaluation to the special procedure
(define Special-Forms (empty-dict))

;; Macro-Forms maps symbols to procedures which transforming
;; the original form into a new form before regular evaluation.
;; Macro Transformation does not require a runtime environment,
;; therefore Macros can be "compiled out" for efficiency
(define Macro-Forms (empty-dict))

; Top-Env is a Modifiable Dictionary mapping names to values
; It also currently inherits from racket/base
; although set! can only modify things we've defined
(define Top-Env (empty-mdict))
(define Default-Namespace (module->namespace 'racket/base))

;; an atomic expression is anything which is not a list
;; a list is a sequence of pairs
;; for evaluation semantics, this will meet our needs:
(define (atomic? x) (not (pair? x)))

; evaluate a symbolic expression representing Lisp Code
; in a given environment or the Top-Env.
(define (new-eval sexp [env (Top-Env)])
  (cond [(symbol? sexp) (env-lookup sexp env)]
        [(null? sexp) (raise-arguments-error 'new-eval "expected non-null value")]
        [(atomic? sexp) sexp]  ; non-symbol atoms evaluate to themselves
        [(macro-form? sexp) (new-eval (macro-expand sexp) env)]
        [(special-form? sexp) (eval-special-form (car sexp) env)]
        [#t (let ( [values (map new-eval sexp)] )
              (apply (car values) (cdr values)) )] ) )

;; ** Special Forms

(define (special-form? form) (dict-lookup (car form) Special-Forms))

;; Lookup the procedure for evaluating this kind of special form.
;; Call that procedure on the raw form and the runtime environment.
(define (eval-special-form form env)
  (let ([binding (dict-lookup (car form) Special-Forms)])
    ((dict-binding-value binding) form env) ) )

;; Define a new special form
;; Note the boiler plate in the calls to this function.
;; Do you see how you could eliminate such using a macro?
(define (def-special name proc)
  (set! Special-Forms (cons (cons name proc))) )

(def-special 'quote (λ (form env) (cadr form)))

(def-special 'if (λ (form env) (if (new-eval (car form) env)
                                   (new-eval (cadr form) env)
                                   (new-eval (caddr form) env) )))

(def-special 'define
              (λ (form env)
                (if (not (eq? env Top-Env))
                    (raise-arguments-error 'define "not at top level" "form" form)
                    (let ([setting (mdict-lookup-binding env)])
                      (if setting
                          (raise-arguments-error 'define "already defined" "form" form)
                          (set! Top-Env
                                (mdict-cons (mdict-setting
                                                  (cadr form)
                                                  (new-eval (caddr form) env) )) ) ) ) ) ) )

(def-special 'set!
              (λ (form env)
                (let ([binding (dict-lookup-binding env)])
                  (if (not binding)
                      (raise-arguments-error 'set! "no definition" "symbol" (cadr form))
                      ; Change the modifiable cdr of the setting pair
                      (set-mcdr! (cadr form) (new-eval (caddr form) env)) ) ) ) )

(def-special 'lambda (λ (form env)
                       (let ( [params (cadr form)] [body (cddr form)])
                         (new-closure env params body) ) ))

;; bind'λ to the same procedure as 'lambda
(def-special 'λ (cdr (assoc 'lambda Special-Forms)))

;; ** Closures

;; A closure encloses a lambda expression with the environment within which it
;; was defined. We really only need to keep the bindings which are referenced by
;; symbols which are present in the body of the lambda. Here we're keeping the
;; whole environment for simplicity.

;; create a closure procedure which when called
;; evaluates each form in body
;; in the context of env extended with the parameters
;; bound to the given arguments
(define (new-closure env params body)
  (λ args (let ([new-env (env-extend env params args)])
            (iter-map (λ (form) (new-eval form new-env)) body) )) )

;; map proc over list returning the last result
(define (iter-map proc list) (foldl (λ (x _) (proc x)) #f list))

;; return env extended with the bindings between parameters and arguments
;; a symbol in parameters will bind all remaining arguments, e.g.
;; parameters=all-args will bind all-args to all of the arguments (as a list)
;; parameters=(a b . rest) will bind rest to (cddr arguments)
;; otherwise (length parameters) must equal (length arguments)
(define (env-extend env parameters arguments)
  (define (add params args)
    (cond [(symbol? params) (cons (dict-setting params args) env)]
          [(and (null? params) (null? args)) env]
          [(and (pair? params) (pair? args))
           (cons (dict-setting (car params) (car args))
                 (add (cdr params) (cdr args)) ) ]
          [#t (raise-arguments-error
               'lambda
               "expected #params = #args" "params" parameters "args" arguments )] ) )
  (add params args) )

;; ** Macros

;; We can define the rest of the usual special forms as macros
;; and programmers can define more Macros as desired.

(define (macro-form? form) (dict-lookup (car form) Macro-Forms))

;; Lookup the procedure for transforming this syntactic macro.
;; Call that procedure on the raw form to produce a new form.
(define (macro-expand form)
  (let ([binding (dict-lookup (car form) Macro-Forms)])
    ((dict-binding-value binding) form) ) )

;; Define a new macro form
(define (def-macro name proc)
  (set! Macro-Forms (cons (cons name proc))) )

;; begin lets you nest multiple forms where one form is allowed
;; it's syntactic sugar for calling a lambda with no arguments
;; begin - (begin form...) -->  ( (λ () form...) )
(def-macro 'begin (λ (form) `( (λ () ,@(cdr form)) )) )

;; (macro-expand '(begin (set! x (+ x 1)) (printf "x = ~a\n")))
;; -->
;; ( (λ () (set! x (+ x 1)) (printf "x = ~a\n")) )

;; let is syntactic sugar for calling a lambda on some values
;; (let bindings body) --> ( (λ parameters body) arguments )
(def-macro 'let
            (λ (form) (let ( [head (cadr form)] [body (cddr form)] )
                        (let ( [vars (map car head)] [vals (map cadr head)] )
                          `( (λ ,vars ,@body) ,vars ) ) )) )

;; let* - (let* bindings body) --> body nested in nested regular let forms
(def-macro 'let*
  (λ (form) (let ([head (cadr form)] [body (cddr form)])
              (if (null? head)
                  `(begin ,@body)
                  (let ( [var1 (car head)] [val1 (cadr head)] )
                    `(let ([,var1 ,val1]) (let* ,(cdr head) ,@body)) ) ) )) )

;; letrec - (letrec bindings body) -->  ( (λ parameters (set! p1 a1)... body) dummy-args )
;; cond - nested if expressions
;; define -- if we change the above define to define-binding or somesuch
;; then (define (f parameters) body) --> (define f  (λ parameters body) )

;; Since these transformation don't require the runtime environment
;; they can be done at compile or load time, or the first time
;; they are encountered.  Therefore they are essentially free.

** Errors: Raising Exceptions

; What shall we do when someone tries to evaluate invalid code?
; Racket provides a sophisticated system for handling exceptions.
; For now, we'll just use raise-arguments-error to
; abort the current evaluation
; print a helpful message

** Dictionaries

; For simplicity, we're currently implementing Dictionaries
; as lists of (symbol . value) pairs.

(define (empty-dict) '())

; (define empty-dict? null?)

;; We're only going to check the first Pair for now
; (define (non-empty-dict? d)
;   (and (pair? d) (pair? (car d)) (symbol? (car (car d)))) )

; (define dict-setting cons)             ; non-modifiable binding
; (define dict-cons cons)                ; add binding to dictionary
(define dict-lookup-binding assoc)      ; returns a pair
; (define dict-binding-name car)
(define dict-binding-value car)

; Try to find name in a Dictionary
(define (dict-lookup name dict)
  (if (not (symbol? name))
      (raise-arguments-error 'dict-lookup "expected a symbol" "name" name)
      (let ( [binding (dict-lookup-binding name dict)] )
        (if (not binding)
            (raise-arguments-error 'dict-lookup "expected a value" "name" name)
            (dict-binding-value pair) ) ) ) )

** Modifiable Dictionaries

; For simplicity we implement
; a Modifiable Dictionary (mdict) as a list of settings.
; a setting as an mpair (modifiable pair)
; an mpair is created by mcons
; an mpair consists of an mcar and an mcdr

(define (empty-mdict) '())
(define empty-mdict? null?)
(define mdict-cons cons)                ; add setting to mdict

(define mdict-setting mcons)            ; modifiable binding
(define mdict-setting? mpair?)          ; modifiable binding
(define mdict-setting-name mcar)
(define mdict-setting-value mcdr)

;; We're only going to check the first setting for efficiency
(define (non-empty-mdict? d)
  (and (pair? d) (mdict-setting? (car d)) (symbol? (mdict-setting-name (car d)))) )

;; return setting (mpair) or #f (false)
(define (mdict-lookup-setting name mdict)
  ;; member returns tail of list starting with element found, or #f
  (let ([tail (member name mdict (λ (sym setting) (eq? sym (mdict-setting-name setting))))])
    (and tail (car tail)) ) )

; Try to find name in a Modifiable Dictionary
(define (mdict-lookup name mdict)
  (if (not (symbol? name))
      (raise-arguments-error 'mdict-lookup "expected a symbol" "name" name)
      (let ( [setting (mdict-lookup-setting name mdict)] )
        (if (not setting)
            (raise-arguments-error 'mdict-lookup "expected a value" "name" name)
            (mdict-setting-value setting) ) ) ) )

** Environments

; For simplicity, we're currently implementing Environments
; as Modifiable Dictionaries

; Looking up values will search
; 1. the current environment
; 2. Top-Env
; 3. the Default-Namespace in Racket

; Looking up bindings will search
; 1. the current environment
; 2. Top-Env

; define adds bindings to Top-Env
; apply adds bindings to the current environment

; Lookup a symbol in the mdict env
; or else the Top-Env
; or else the Default-Namespace
(define (env-lookup name env)
  (let ( [binding (mdict-lookup-binding name env)] )
    (if binding
        (mdict-setting-value binding)
        (let ( [binding (mdict-lookup-binding name Top-Env)] )
          (if binding
              (mdict-setting-value binding)
              (with-handlers              ; let's try the racket/base environment
                ([exn:fail:contract:variable? ; thrown on undefined symbol
                  (λ (exn) (raise-arguments-error 'env-lookup
                                                   "expected binding" "name" name)) ])
                (eval name Default-Namespace) ) ) ) ) ) )
