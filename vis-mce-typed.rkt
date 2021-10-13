#lang typed/racket
(define-type ValuePairs (Pairof Values (Listof Values))) ; NonEmpty
(require/typed racket/base [apply (-> Procedure (Listof Values) Values)])
; (require/typed racket/base [map (-> Procedure ValuePairs ValuePairs)])
               
;; A meta-circular evaluator defines a language by writing an interpreter
;; in an identical or similar language.

;; Here we're defining a language called VIS-Scheme
;; aka Virtual Infinity Systems Scheme aka VIS, which
;; (1) is similar to the original Scheme Language
;; (2) provides a model for understanding similar languages
;; (3) allows us to try out experimental extensions

;; We should keep our implementation details cleanly separated from our
;; semantic functions
;; - (vis-env . bindings)
;;   - creates an environment from 0 or more (key . value) pairs/Data1/Greg/WebPages/ngender.net
;; - (vis-eval symbolic-expression environment)
;;   - evaluates an s-exp in a given env, yielding a value

;; Compared to the first Scheme Standard, VIS currently lacks
;; - many Scheme functions - we can just borrow them from Racket!
;; - a macro defining system - and macro-definable special forms!
;; - reified continuations
;; - side effects (except for monotonic define in s-exp sequences)
;; VIS procedures are FUNCTIONAL, i.e. they compute a single value
;; from their arguments without any side-effects.
;; VIS programs are MONOTONIC, i.e. the top level environment will grow as
;; a side-effect of any top-level (define ...) special forms.

;; These types may change!  In particular:
;; - Values may include more possibilities
;; - Envs may not always be association lists
(define-type Values
  (U Number String Symbol Boolean Procedure closures '() (Pairof Values Values)) )
(define-type Bindings (Pairof Symbol Values))
(define-type Binding-Lookups (U Bindings #f))
(define-type Envs (Listof Bindings))
(define-type EnvPairs (Pair Bindings Envs)) ; NonEmpty

;; (define ...) forms return Extended Environments, which
;; are not Values, i.e. they cannot be passed as arguments
;; to Functors, stored in environments nor returned as top-level
;; values!
(struct extended-envs ([env : (Pairof Bindings Envs)]))
(define-type Evaluations (U Values extended-envs))

;; An "Improper "List" of Symbols
(define-type Parameters (U '() Symbol (Pairof Symbol Parameters)))
(: parameters (-> Values Parameters))
(define (parameters x)
  (cond [(null? x) x]
        [(symbol? x) x]
        [(pair? x) (let ( [name (car x)] [rest (cdr x)] )
                     (when (not (symbol? x))
                       (raise-argument-error 'parameters "symbol?" 1) )
                     (cons name (parameters rest)) ) ]
        [else (raise-argument-error 'parameters "Parameters" 1)] ) )
                     
;; Closures contain a lambda's parameters and body and the
;; environment which was current when the lambda was created.
;; If part of a fancy define, the name will also be stored.
(struct closures
  ( [name : (U Symbol #f)]
    [env : Envs]
    [params : Parameters]
    [body : Values] ) )
(define-type Functors (U Procedure closures))

;; Symbols evaluate to their value in the environment.
;; Lists evaluate as Forms.
;; Everything else evaluates to itself.
(: vis-eval (-> Values Envs Values))
(define (vis-eval expr env)
  (let ( [value (env-eval env expr)] )
    (when (extended-envs? value)
      (raise-result-error 'vis-eval "Value" 1) )
    value ) )

(: env-eval (-> Envs Values Evaluations))
(define (env-eval env expr)
  (cond [(symbol? expr) (env-key->value env expr)]
        [(and (pair? expr) (list? (cdr expr))) (env-form->value env expr)]
        [else expr] ) )

; We expect but can't guarantee proc : Functors
; (: vis-apply (-> Functors (Listof Values) Values))
(: vis-apply (-> Values (Listof Values) Values))
(define (vis-apply proc args)
  (cond [(closures? proc) (apply-closure proc args)]
        [(procedure? proc) (apply proc args)]
        [else (raise-argument-error 'vis-apply "Functor" 1)] ) )

;; vis-env takes 0 or more bindings as (key . racket-procedure) pairs,
;; i.e. an association list of Racket functions.
(: vis-env (-> (Listof (Pairof Symbol Procedure)) Envs))
(define (vis-env bindings) bindings)

;; inject any desired pure functional procedures from Racket
;; into the initial environment:
(define *vis-top-level-environment*
  (vis-env `(
             ;; Numeric Functions
             (= . ,=) (+ . ,+) (- . ,-) (* . ,*)
             ;; Pair and List Functions
             (pair? . ,pair?) (null? . ,null?)
             (cons . ,cons) (car . ,car) (cdr . ,cdr) (list . ,list)
             ;; String Functions
             (string? . ,string?) (string . ,string-append-immutable)
             (format . ,format)
             )) )

(: vis-binding (-> Symbol Values Bindings))
(define vis-binding cons)
(: vis-binding->key (-> Bindings Symbol))
(define vis-binding->key car)
(: vis-binding->value (-> Bindings Values))
(define vis-binding->value cdr)

(: env-key->binding (-> Envs Symbol Binding-Lookups))
(define (env-key->binding env key) (assoc key env))
(: env-key->value (-> Envs Symbol Values))
(define (env-key->value env key)
  (let ( [lookup (env-key->binding env key)] )
    (when (not lookup)
      (raise-result-error
       'env-key-binding "unbound symbol" 2) )
    (cdr lookup) ) )
(: env-binding->env (-> Envs Bindings EnvPairs))
(define (env-binding->env env binding) (cons binding env))
(: env-key-value->env (-> Envs Symbol Values EnvPairs))
(define (env-key-value->env env key value)
  (env-binding->env env (cons key value)) )

;; Match parameters with values to extend the environment.
;; See the test examples at the end of this file!
(: env-parameters-values->env (-> Envs Parameters (Listof Values) Envs))
(define (env-parameters-values->env env keys values)
  (cond [(symbol? keys) (cons (vis-binding keys values) env)]
        [(and (null? keys) (null? values)) env]
        [(null? keys) (raise-argument-error 'env-parameters-values->env "exhausted keys" 3)]
        [(null? values) (raise-argument-error 'env-parameters-values->env "exhausted values" 2)]
        [else (cons (vis-binding (car keys) (car values))
                    (env-parameters-values->env env (cdr keys) (cdr values)) )] ) )

(: env-form->value (-> Envs ValuePairs Evaluations))
(define (env-form->value env expr)
  (let ( [head (car expr)] [tail (cdr expr)] )
    (cond
      ; special forms
      [(eq? head 'quote) (car tail)]
      [(eq? head 'if) (eval-if env tail)]
      [(eq? head 'define) (eval-define-fancy env tail)]
      [(eq? head 'begin) (eval-begin env tail)]
      [(or (eq? head 'λ) (eq? head 'lambda)) (eval-lambda #f env tail)]
      ; regular forms
      ; type checker really should deduce type of elem!!
      ; it also should deduce that values is non-empty!!
      [else (let ( [values (map (λ (elem) (env-eval env elem)) expr)] )
              (vis-apply (car values) (cdr values)) )] ) ) )

(: eval-if (-> Envs (Listof Values) Values))
(define (eval-if env args)
  (when (not (= 3 (length args)))
    (raise-argument-error 'eval-if "arity!=3" 2) )
  (let ( [test (car args)] [if-true (cadr args)] [if-false (caddr args)] )
    (if (vis-eval test env)
        (vis-eval if-true env)
        (vis-eval if-false env) ) ) )

(: eval-define-fancy (-> Envs (Listof Values) extended-envs))
(define (eval-define-fancy env args)
  (when (not (= 2 (length args)))
    (raise-argument-error 'eval-if "arity!=2" 2) )
  (let ( [head (car args)] [value (cadr args)] )
    (cond [(symbol? head) (eval-define env head value)]
          [(pair? head) (let ([name (car head)])
                          (when (not (symbol? name))
                            (raise-argument-error 'eval-define-fancy "Parameters" 2) )
                          (eval-define env name (eval-lambda name env (list (cdr head) value))) ) ]
          [else (raise-argument-error 'eval-define-fancy "Parameters" 2)] ) ) )

(: eval-define (-> Envs Symbol Values extended-envs))
(define (eval-define env name value)
  (extended-envs (env-key-value->env env name (vis-eval value env))) )

;; When we apply a closure to some arguments,
;; we extend the embedded environment by
;; binding the lambda parameters to the provided arguments
;; and evaluate the lambda expression in that environment.
(: apply-closure (-> closures (Listof Values) Values))
(define (apply-closure closure args)
  (vis-eval
   (closures-body closure)
   (env-parameters-values->env (closures-env closure) (closures-params closure) args) ) )

;; A VIS Lambda is functional therefore body-list
;; must be a singleton or an implied begin form
(: eval-lambda (-> (U Symbol #f) Envs (Listof Values) closures))
(define (eval-lambda name env args)
  (when (not (> 2 (length args)))
    (raise-argument-error 'eval-lambda "arity" 3) )
  (let ( [parameters (parameters (car args))] [body-list (cdr args)] )
    (when (not (= 1 (length body-list)))
      (raise-argument-error 'eval-lambda "singleton-list?" 4) )
    (closures name env parameters (if (= 1 (length body-list)) body-list (cons 'begin body-list) )) ) )

;; The above covers the functional semantics
;; of VIS-Scheme and the code id quite elegant.
;; Now we deal with the monotonic side-effects
;; present in blocks and sequences and the code
;; is a bit messier!

;; Evaluate a non-empty list of s-exprs in sequence.
;; Should any of them be define forms,
;; propagate the new environment into the evaluation
;; of the subsequent forms within the begin.  Return
;; the value of the last s-expr.  (A similar algorithm
;; is used for top-level sequences of s-exprs except
;; that we allow such defines to shadow earlier bindings.)
;; We allow local bindings to shadow existing outside
;; bindings but NOT earlier local bindings, giving local
;; monotonicity.  The last form in a (begin ...) must
;; produce a regular value, i.e. not be a define form.
;; All but the last form must produce a side-effect,
;; which currently only define blocks do, otherwise
;; their evaluation makes no difference!
(: eval-begin (-> Envs (Listof Values) Values))
(define (eval-begin env expr-list)
  (let ( [value (eval-begin-seq env env expr-list)] )
    (if (extended-envs? value)
        (raise-result-error 'eval-begin-seq "block yielding a value" 2)
        value ) ) )

;; Like standard assoc only will only search back as far as env0
;; which should be a sublist of env, i.e. an earlier point.
(: key-env-env0->binding (-> Symbol Envs Envs Binding-Lookups))
(define (key-env-env0->binding key env env0)
  (cond [(or (null? env) (eq? env0 env)) #f]
        [(eq? key (vis-binding->key (car env))) (car env)]
        [else (key-env-env0->binding key (cdr env) env0)] ) )

;; Evaluate a single s-exp within a begin form
;; yielding either Values or extended-envs.
;; Complain if it's a local symbol redefinition.
;; env0 is the environment before evaluating anything
;; within the begin form.
;; return the value or extended-envs.
(: eval-begin-eval (-> Envs Envs Values Evaluations))
(define (eval-begin-eval env0 env expr)
  (let ( [x (env-eval env expr)] )
    (and
     (extended-envs? x)
     (let* ( [new-env (extended-envs-env x)]
             [new-binding (car new-env)]
             [key (vis-binding->key new-binding)]
             [earlier-binding (key-env-env0->binding key env env0)] )
       (when earlier-binding
         (raise-argument-error 'eval-begin-eval "rebinding attempt" 3) ) ) )
    x ) )

;; Evaluate a sequence of 1 or more s-expressions, aka "a block"
;; returning the value of the last expression.  Thread any local
;; non-conflicting defines into extensions of the local environment.
;; env0 is the environment at the beginning of the block, env is the
;; environment reflecting any extensions.
(: eval-begin-seq (-> Envs Envs (Listof Values) Evaluations))
(define (eval-begin-seq env0 env seq)
  (when (null? seq)
      (raise-argument-error 'eval-begin-seq "ValuePairs" 3) )
  (let ( [value1 (eval-begin-eval env0 env (car seq))] [tail (cdr seq)] )
    (if (null? tail)
        value1 ; any extended-envs here will be rejected!
        (eval-begin-seq
         env0
         (if (extended-envs? value1) (extended-envs-env value1) env)
         tail ) ) ) )

;; Some test code

;; yay metaprogramming!

;; (expect-equal? expected-value racket-expression)
(define-syntax-rule (expect-equal? expected-value expr)
  (let ( [value expr] )
    (when (not (equal? value expected-value))
      (raise-user-error 'expect-equal? "Error: ~a ⟶ ~a ⥇ ~a!\n" 'expr value expected-value) ) ) )

;; (vis-expect-equal? expected-value vis-scheme-expression)
(define-syntax-rule (vis-expect-equal? expected-value expr)
  (let ( [value (vis-eval 'expr *vis-top-level-environment*)] )
    (when (not (equal? value expected-value))
      (raise-user-error 'vis-expect-equal? "Error: ~a ⟶ ~a ≠ ~a!\n" 'expr value expected-value) ) ) )

;; (vis-expect-fail fallacious-vis-scheme-expression)
(define-syntax-rule (vis-expect-fail expr)
  (with-handlers ([exn:fail:contract? (λ (e) (void)) ]) ; do nothing on an expected contract violation
      (let ( [value (vis-eval 'expr *vis-top-level-environment*)] )
        (raise-user-error 'vis-expect-fail "Error: ~a ⟶ ~a should have failed!\n" expr value) ) ) )

(vis-expect-equal? 1 (if #t 1 2))
(vis-expect-equal? 2 (if #f 1 2))
(vis-expect-equal? 2 (+ 1 1))

(vis-expect-equal? 2 (begin (define one 1) (+ one one)))

(vis-expect-equal? "Hello world!" (string "Hello" " " "world" "!"))

;; Won't even type check:
; (vis-expect-fail (1 1))

(expect-equal? '( (foo . (1 2 3)) ) (env-parameters-values->env '() 'foo '(1 2 3)) )
(expect-equal? '( (a . 1) (b . 2) (c . 3) ) (env-parameters-values->env '() '(a b c) '(1 2 3)) )
(expect-equal? '( (a . 1) (b . (2 3)) ) (env-parameters-values->env '() '(a . b) '(1 2 3)) )

;; Question and Exercise Problems:
;; - What other kinds of side-effects do we wish to allow?
;; - How might we do I/O?
;;   - Hint: Look at functional language approaches!
;; - What is the cost of our implementation of environments?
;;   - Where could we "compile-away" that cost?
;; - Create a repl for a nice top level
;;   - Make sure that it properly handles (define ...) forms
;; - Create an interactive repl
;; - Create a non-interactive repl for files and input streams
;;   - Should this be separate from the interative repl?
;; - Where else should we check for errors?
;; - Where would it be useful to provide warnings?
;; - Is your code simple and clear enough that others can build
;;   their ideas on your work?
