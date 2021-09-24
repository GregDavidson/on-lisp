#lang racket

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
;;   - creates an environment from 0 or more (key . value) pairs
;; - (vis-eval symbolic-expression environment)
;;   - evaluates an s-exp in a given env, yielding a value
;; - (vis-apply closure arguments)
;;   - applies a closure to an argument list, yielding a value

;; Compared to the first Scheme Standard, VIS currently lacks
;; - many Scheme functions - we can just borrow them from Racket!
;; - a macro defining system - and macro-definable special forms!
;; - reified continuations
;; - side effects (except for monotonic define in s-exp sequences)
;; VIS procedures are FUNCTIONAL, i.e. they compute a single value
;; from their arguments without any side-effects.
;; VIS programs are MONOTONIC, i.e. the top level environment will grow as
;; a side-effect of any top-level (define ...) special forms.

;; symbols evaluate to their value in the environment
;; lists evaluate as forms
;; everything else evaluates to itself
(define (vis-eval s-exp env)
  (cond [(symbol? s-exp) (env-key->value env s-exp)]
        [(pair? s-exp) (env-form->value env s-exp)]
        [else s-exp] ) )

(define (vis-apply proc args)
  (if (procedure? proc)
      ; apply primitive Racket procedure to arguments
      (apply proc args)
      ; apply vis-closure to arguments
      (apply-vis-closure proc args) ) )

;; vis-env takes 0 or more bindings as (key . value) pairs, i.e.
;; as an association list.  This DOES NOT imply that association
;; lists are the implementation of bindings and environments -
;; although it currently IS *wink*
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

;; (define ...) forms don't return a regular value.  Since
;; Racket structures create a new type which is distinct from
;; all other types, they're perfect for this - otherwise we
;; would need to use, e.g. a tagged list.  And besides, Racket
;; (struct ...) writes our constructor, test and selector
;; functions for us - hurrah for metaprogramming!
;; So in our implementation define forms return a modified
;; environment (extended with the new binding) wrapped in a
;; structure, which is therefore NOT a VIS Scheme value!
;; Nothing should be printed by a repl.
;; It shall be an error if such an object is ever used as
;; a value!
(struct modified-env (env))

;; evaluating a define form will yield a modified-env structure!
(define (eval-value? x) (or (modified-env? x) (vis-value? x)))
;; vis-value? ideally would enumerate all of the allowed types!!
(define (vis-value? x) (nor (modified-env? x) (void? x)))

;; It's expensive to test if something is a proper list, so we'll
;; just test if it's empty or if it starts with a pair.  Only use
;; where we expect a proper list, though!
(define (listy? x) (or (null? x) (pair? x)))

;; We CURRENTLY represent
;; - bindings as (key . value) pairs
;; - environments as (lists of bindings)
;; These representations MAY CHANGE so do not expose
;; this implementation design to any semantic-level functions!
(define (vis-binding? x) (and (pair? x) (symbol? (car x)) (vis-value? (cdr x))))
(define (maybe-vis-binding? x) (or (false? x) (vis-binding? x))) ;; failed lookup returns #f
(define/contract (vis-binding key value)
  (-> symbol? vis-value? vis-binding?)
  (cons key value) )
(define/contract (vis-binding->key b) (-> vis-binding? symbol?) (car b))
(define/contract (vis-binding->value b) (-> vis-binding? vis-value?) (cdr b))

;; We're only checking the first element, if any!
(define (vis-env? x) (or (null? x) (and (pair? x) (vis-binding? (car x)))))
(define/contract (env-key->binding env key)
  (-> vis-env? symbol? maybe-vis-binding?)
  (assoc key env) )
(define/contract (env-key->value env key)
  (-> vis-env? symbol? vis-value?)
  (cdr (env-key->binding env key)) )
(define/contract (env-binding->env env binding)
  (-> vis-env? vis-binding? vis-env?)
  (cons binding env) )
(define/contract (env-key-value->env env key value)
  (-> vis-env? symbol? vis-value? vis-env?)
  (env-binding->env env (cons key value)) )

;; Match parameters with values to extend the environment.
;; Parameters can be a symbol, a list of symbols,
;; or an "inproper list" of symbols, ending in a symbol.
;; See the test examples at the end of this file!
(define (env-parameters-values->env env keys values)
  (cond [(symbol? keys) (cons (cons keys values) env)]
        [(and (null? keys) (null? values)) env]
        [(not (pair? keys)) (raise-argument-error 'env-parameters-values->env "list?" 2)]
        [(not (pair? values)) (raise-argument-error 'env-parameters-values->env "list?" 3)]
        [else (cons (vis-binding (car keys) (car values))
                    (env-parameters-values->env env (cdr keys) (cdr values)) )] ) )
 
(define (env-form->value env s-exp)
  (let ( [head (car s-exp)] [tail (cdr s-exp)] )
    (cond
      ; special forms
      [(eq? head 'quote) (car tail)]
      [(eq? head 'if) (eval-if env tail)]
      [(eq? head 'define) (eval-define-fancy env (car tail) (cadr tail))]
      [(eq? head 'begin) (eval-begin env tail)]
      [(or (eq? head 'λ) (eq? head 'lambda))
       (eval-lambda "" env (car tail) (cdr tail)) ]
      ; regular forms
      [else (let ( [values (map (λ (elem) (vis-eval elem env)) s-exp)] )
              (vis-apply (car values) (cdr values)) )] ) ) )

(define (check-env env [vis-value? (lambda (x) #t)])
  (map (lambda (binding)
         (let ([name (vis-binding->key binding)] [value (cdr binding)])
           (when (not (symbol? name))
             (raise-argument-error 'check-env "symbol?" 1) )
           (when (not (vis-value? value))
             (raise-argument-error 'check-env "vis-value?" 1) ) ) )) )

;; (if test true-expression false-expression)
(define (eval-if env args)
  (if (vis-eval (first args) env)
      (vis-eval (second args) env)
      (vis-eval (third args) env) ) )

(define (eval-define-fancy env head value)
  (cond [(symbol? head) (eval-define env head value)]
        [(and (pair? head) (pair? (car head)))
         (let ([name (car head)])
           (eval-define env name (eval-lambda name env (cdr head) value)) ) ] ) )
        
(define (eval-define env name value)
  (modified-env (env-key-value->env env name (vis-eval value env))) )

;; A closure could be represented as a list or an array of
;; four values, but let's make it simpler with a structure,
;; hurrah again for metaprogramming!
(struct vis-closure (name env params body))

;; A vis-closure contains a lambda procedure and the environment
;; which was current when the lambda procedure was created.
;; When we apply a closure to some arguments,
;; we extend the embedded environment by
;; binding the lambda parameters to the provided arguments
;; and evaluate the lambda expression in that environment.
(define/contract (apply-vis-closure closure args)
  (-> vis-closure? pair? vis-value?)
  (vis-eval
   (vis-closure-body closure)
   (env-parameters-values->env (vis-closure-env closure) (vis-closure-params) args) ) )

(define (vis-function? x) (or (procedure? x) (vis-closure? x)))

;; Note: VIS Lambda is currently functional
;; therefore body-list must be a singleton list
;; containing the Lambda body S-Expression!
;; ==> Rewrite the body as a BEGIN form!
(define (eval-lambda name env parameters body-list)
  (when (not (= 1 (length body-list)))
      (raise-argument-error 'eval-lambda "singleton-list?" 4) )
  (vis-closure name env parameters body-list) )

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
(define/contract (eval-begin env s-expr-list)
  (-> vis-env? pair? vis-value?)
  (let ( [value (eval-begin-seq env env s-expr-list)] )
    (if (modified-env? value)
        (raise-result-error 'eval-begin-seq "block yielding a value" 2)
        value ) ) )

;; Like standard assoc only will only search back as far as env0
;; which should be a sublist of env, i.e. an earlier point.
(define/contract
  (key-env-env0->binding key env env0)
  (-> symbol? vis-env? vis-env? maybe-vis-binding?)
  (cond [(or (null? env) (eq? env0 env)) #f]
        [(eq? key (vis-binding->key (car env))) (car env)]
        [else (key-env-env0->binding key (cdr env) env0)] ) )

;; Evaluate a single s-exp within a begin form
;; yielding either a vis-value or a modified-env.
;; Complain if it's a local symbol redefinition.
;; env0 is the environment before evaluating anything
;; within the begin form.
;; return the value or modified-env.
(define/contract
  (eval-begin-eval env0 env s-exp)
  (-> vis-env? vis-env? vis-value? eval-value?)
  (let ( [x (vis-eval s-exp env)] )
    (and
     (modified-env? x)
     (let* ( [new-env (modified-env-env x)]
             [new-binding (car new-env)]
             [key (vis-binding->key new-binding)]
             [earlier-binding (key-env-env0->binding key env env0)] )
       (when earlier-binding
         (raise-mismatch-error
          'eval-begin-eval "rebinding attempt "
          "key: " key "old: " (vis-binding->value earlier-binding) "new: " (vis-binding->value new-binding) ) ) ) )
    x ) )

;; Evaluate a sequence of 1 or more s-expressions, aka "a block"
;; returning the value of the last expression.  Thread any local
;; non-conflicting defines into extensions of the local environment.
;; env0 is the environment at the beginning of the block, env is the
;; environment reflecting any extensions.
(define (eval-begin-seq env0 env seq)
  (let ( [value1 (eval-begin-eval env0 env (car seq))] [tail (cdr seq)] )
    (when (and (not (pair? tail)) (not (null? tail)))
      (raise-argument-error 'eval-begin-seq "list?" 3) )
    (if (null? tail)
        value1 ; any modified-env here will be rejected!
        (eval-begin-seq
         env0
         (if (modified-env? value1) (modified-env-env value1) env)
         tail ) ) ) )

;; Some test code

;; yay metaprogramming!

;; (expect-equal? expected-value racket-expression)
(define-syntax-rule (expect-equal? expected-value exp)
  (let ( [value exp] )
    (when (not (equal? value expected-value))
      (raise-user-error 'vis-eval "Error: ~a ⟶ ~a ⥇ ~a!\n" 'exp value expected-value) ) ) )

;; (vis-expect-equal? expected-value vis-scheme-expression)
(define-syntax-rule (vis-expect-equal? expected-value exp)
  (let ( [value (vis-eval 'exp *vis-top-level-environment*)] )
    (when (not (equal? value expected-value))
      (raise-user-error 'vis-eval "Error: ~a ⟶ ~a ≠ ~a!\n" 'exp value expected-value) ) ) )

;; (vis-expect-fail fallacious-vis-scheme-expression)
(define-syntax-rule (vis-expect-fail exp)
  (with-handlers ([exn:fail:contract? (λ (e) (void)) ]) ; do nothing on an expected contract violation
      (let ( [value (vis-eval 'exp *vis-top-level-environment*)] )
        (raise-user-error 'vis-expect-fail "Error: ~a ⟶ ~a should have failed!\n" exp value) ) ) )

(vis-expect-equal? 1 (if #t 1 2))
(vis-expect-equal? 2 (if #f 1 2))
(vis-expect-equal? 2 (+ 1 1))

(vis-expect-equal? 2 (begin (define one 1) (+ one one)))

(vis-expect-equal? "Hello world!" (string "Hello" " " "world" "!"))

(vis-expect-fail (1 1))

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
