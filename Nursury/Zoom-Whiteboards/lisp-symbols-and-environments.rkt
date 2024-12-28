#lang slideshow

;; This example is intended to correspond to the
;; Zoom whiteboard of the same name.

;; Create a procedure of arity 1
;; Create an environment binding to it
(define (checkerboard2 p)
  (let* ( [p1 (colorize p "red")]
          [p2 (colorize p "blue")]
          [p12 (hc-append p1 p2)]
          [p21 (hc-append p2 p1)]
          [p4 (vc-append p12 p21)] )
    p4 ) )

;; Create two cons-cells linked together into a list
;; Create an environment binding for it
(define two-colors '("green" "blue"))

;; Create one cons-cell linked to an existing list
;; Create an environment binding for it
(define three-colors (cons "red" two-colors)

;; Create a circle object
;; Create an environment binding for it
(define my-circle (circle 30))
