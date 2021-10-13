#lang racket

; given a nice normal function (f x) -> y
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
            (avg-accum (cdr rest) (+ sum (cdr (cdr rest)) (add1 cnt))) ) )
      (values (avg-accum new-alist 0 0) new-alist) ) ) )


; given a nice normal function (f x) -> a number y
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
; - a nice normal function (f x) -> a number y
; - a procedure (wierding state x) -> a number y, a ?new? state
; - (where state begins as an empty list)
; return a closure over (wierding f) and state which
; maps x to a value to return and a new state
(define (wierdo f wierding)
  (let ( [state '()]
         [wierder (wierding f)] )
    (λ (x) (let-values ( [(f-of-x new-state) (wierder state x)] )
             (set! state new-state)
             f-of-x )) ) )

;; OK, let's create some examples

; First, a really boring nice normal function
(define (sqr x) (* x x))

(define curly (wierdo sqr avg-val))

(define moe (wierdo sqr memoize))

;; Questions for you, dear reader:

; What kind of thing is curly?

; What kind of thing is moe?
