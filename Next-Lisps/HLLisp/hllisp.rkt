#lang br

;; My ideal Language
;; pure - no side effects
;; high-level
;; - best :: streams
;; - next best :: sequences
;; - rare optimizations :: concrete datatypes

(define (_true then else) then)
(define (_false then else) else)
(define (_if test then else) ((test then else)))
