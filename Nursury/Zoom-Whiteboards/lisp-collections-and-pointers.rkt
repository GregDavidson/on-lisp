#lang racket

;; This example is intended to correspond to the
;; Zoom whiteboard of the same name.

;; Create two cons-cells linked together into a list
(define two-colors '("green" "blue"))

;; Create one cons-cell linked to an existing list
(define three-colors (cons "red" two-colors))

;; Create a vector of 5 elements, contiguous in memory
(define five-element-vector (make-vector 5))

;; Create a string of 61 bytes, contiguous in memory
(define a-string "A string is a contiguous sequence of bytes representing text.")

(display (string-length a-string))
