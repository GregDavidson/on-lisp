#lang racket

;;; Distinguishing Good Closures which behave like functional procedures
;;; from Bad Closures which retain state in an observable manner.

;; * Commentary

;; A "functional procedure" is a procedure which
;; (1) always returns the same result(s) given the same arguments
;; (2) doesn't have any "side-effects", i.e.
;;     no observable effect to the computation other than the value(s) returned
;;     side-effects include input/output or observable changes to global bindings
;;     or observable changes to mutable components of persistent data structures

;; A functional procedure can be treated as a black box.  One need only know
;; its specification and its inputs to understand its role in the system.
;; Any change in its implementation won't change the behavior of the system
;; other than timing and efficiency.

;; Loose languages, including almost all Lisps allow us to cheat a little
;; e.g. we can use an output stream for logging which we don't count as
;; a side effect.  Any such cheating needs scset!rutiny and documentation!

;; * The Code

; given a functional procedure (f x) -> y
; return a procedure which takes
; - an alist of previous values
; - an x input for f
; return (values avg alist) where
; - the alist is extended with (x . (f x))
; - avg is the average of the values of the new alist
(define (avg-val f)
  (λ (old-alist  x)
    (let* ( [f-of-x (f x)]
            [new-alist (cons (cons x f-of-x) old-alist)] )
      (define (avg-accum rest sum cnt)
        (if (null? rest)
            (/ sum cnt)
            (avg-accum (cdr rest) (+ sum (cdr (car rest))) (add1 cnt)) ) )
      (values (avg-accum new-alist 0 0) new-alist) ) ) )


; given a functional procedure (f x) -> a number y
; return a function which takes
; - an alist of previous values
; - an x input for f
; return (values f-of-x alist) where either
; - f-of-x was found on alist
; - f was computed using f and alist is extended with this new pair
(define (memoize f)
  (λ (alist  x)
    (let ( [val (assoc x alist)] )
      (if val
          (values (cdr val) alist)
          (let ( [f-of-x (f x)] )
            (values f-of-x (cons (cons x f-of-x) alist)) ) ) ) ) )

; given
; - a functional procedure (f x) -> a number y
; - a procedure (wierding state x) -> a number y, a ?new? state
; - (where state begins as an empty list)
; return a closure over (wierding f) and state which
; maps x to a value to return and a new state
; Note: the viewing window is a side effect for demo purposes
;       imagine trying to understand an arbitrary wierdo procedure
;       without the viewing window looking only at its mappings!
(define (wierdo f wierding)
  (let ( [state '()]
         [wierder (wierding f)] )
    (λ (x) (let-values ( [(f-of-x new-state) (wierder state x)] )
             ; expensive window for your viewing pleasure:
             (when (not (equal? state new-state)) (printf "~s ~~> new-state ~s\n" x new-state))
             (set! state new-state)
             f-of-x )) ) )

;; OK, let's create some examples

; First, a really boring functional procedure
(define (sqr x) (* x x))

(define curly (wierdo sqr avg-val))

(define moe (wierdo sqr memoize))

;; Experimentation

; Experiment with running curly and moe.
; They take the same argument that sqr
; would take.

; Call them multiple times, with the same(wierdo sqr memoize))

;; Experimentation

; Experiment with running curly and moe.
; They take the same argument that sqr
; would take.

; Call them multiple times, with the same
; argument and with different arguments.

;; Questions

; What kind of thing is curly?

; What kind of thing is moe?

; How could you specify each of them as a
; black box?

; How could you test that they were behaving
; according to their specification?

; When might weirdo + memoize be useful?
; How could you make it more efficient?

; In what situation might something like
; wierdo + avg-v
; argument and with different arguments.

;; Questions

; What kind of thing is curly?

; What kind of thing is moe?

; How could you specify each of them as a
; black box?

; How could you test that they were behaving
; according to their specification?

; When might weirdo + memoize be useful?
; How could you make it more efficient?

; In what situation might something like
; wierdo + avg-val be useful?

; How might you minimize the complexity impact of
; non-functional procedures in a system?

; What other questions and insights occur to you
; from this exploration?
