#lang slideshow

;; Lambda is often hiding under Syntactic Sugar!

;; There are 3 essential special forms in Lisp:
;;	lambda -- constructs procedures
;;	if (or cond) -- makes decisions
;;	quote (or ') -- handy for metaprogramming
;; The other special forms are syntactic sugar which make
;; code more natural, expressive and convenient for humans.
;; A good way to understand the other special forms is to see
;; the equivalent code without using that special form.

;; This little Racket program:

(define head (circle 10))
(define body (rectangle 10 20))
(hc-append head body)

;; Is equivalent to this program:

(let [ (head (circle 10))
       (body (rectangle 10 20)) ]
  (hc-append head body) )

;; Note: In Racket [ square brackets ] have the same meaning
;; as ( parentheses ) but must match their own kind.  More
;; syntactic sugar!

;; Which is equivalent to this program:

( (lambda (head body) (hc-append head body)) (circle 10) (rectangle 10 20) )

;; which shows that define and let aren't needed, you only need lambda!

;; What about when definitions use earlier definitions?

;; This little program:

(define eye (circle 10))
(define eyes (hc-append 30 eye eye))
(define figure (vc-append 6 eyes (rectangle 10 20)))
(hc-append 10 figure figure)

;; Is equivalent to this program:

(let* [ (eye (circle 10))
        (eyes (hc-append 30 eye eye))
        (figure (vc-append eyes (rectangle 10 20))) ]
  (hc-append 10 figure figure))

;; Which is equivalent to this program:

(let [ (eye (circle 10)) ]
  (let [ (eyes (hc-append 30 eye eye)) ]
    (let [ (figure (vc-append eyes (rectangle 10 20))) ]
      (hc-append 10 figure figure) ) ) )

;; let* forms can be decomposed into nested let forms
;; let forms can be expressed using lambda:

( (lambda (figure) (hc-append 10 figure figure))
  ( (lambda (eyes) (vc-append eyes (rectangle 10 20)))
    ( (lambda (eye) (hc-append 10 eye eye))
      (circle 10) ) ) )

;; In English:
;; Construct a picture out of a figure by horizontally appending two copies of a figure
;; Construct the figure given some eyes by vertically appending the eyes above a tall rectangle
;; Construct the eyes from an eye by horizontally appending two copies of the eye
;; Construct an eye by making a circle

;; Lambda is usually hiding in procedure (function) definitions

;; This bit of code:

(define (sqr x) (* x x))

;; is syntactic sugar for

(define sqr (lambda (x) (* x x)))

;; Syntactic sugar can only be implemented with regular procedures if all of the
;; arguments are evaluated normally, which is NOT the case in the examples
;; above. Any list which can be evaluated is a "form". Any form which requires
;; special evaluation is a "special form". The first element of a special form
;; is always a symbol which is either the name of a macro or the name of a
;; "reserved word" built into the lisp system.

;; Use common syntactic sugar to make your programs easier to read. Create new
;; syntactic sugar (by defining macros) sparingly, as anyone who reads your code
;; will need to be conversant with the special evaluation used by each macro.
