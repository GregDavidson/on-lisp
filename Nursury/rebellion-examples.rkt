#lang racket/base
(require racket/sequence)
(require rebellion/streaming/transducer)
(require rebellion/streaming/reducer)

;; Given a collection of some form of people's info including names and ages,
;; return the (up to n) names of those people who are at least min years old.
;; The names should be in the same order as in the original list.

;; ** Processing Sequences of name-age Pairs

(define (data-name data) (sequence-ref data 0))
(define (data-age data) (sequence-ref data 1))
(define ((over-age min-age) data) (>= (data-age data) min-age))

(define (filter-map-into people filter map into)
  (transduce people (filtering filter) (mapping map) #:into into) )

;; ** List of List Pairs --> List of Names

(require rebellion/collection/list)

(define list-data '((fred 10) (sue 18) (alice 17) (joe 22)))

(printf "2 names over 16: ~a\n"
        (filter-map-into list-data (over-age 16) data-name (reducer-limit into-list 2)) )
(printf "10 names over 16: ~a\n"
        (filter-map-into list-data (over-age 16) data-name (reducer-limit into-list 10)) )

;; ** Vector of Vector Pairs --> Vector of Names

(require rebellion/collection/vector)

(define vector-data '#(#(fred 10) #(sue 18) #(alice 17) #(joe 22)))

(printf "2 names over 16: ~a\n"
        (filter-map-into vector-data (over-age 16) data-name (into-vector #:size 2)) )
(printf "10 names over 16: ~a\n"
        (filter-map-into vector-data (over-age 16) data-name (into-vector #:size 10) ) )


;; ** Getting Cleaner with Records

(struct person (name age)) ; defines a Racket Record Type
(define ((person-over min-age) person) (>= (person-age person) min-age))

(define person-vector
  (vector (person 'fred 10) (person 'sue 18) (person 'alice 17) (person 'joe 22) ) )

(printf "2 names over 16: ~a\n"
        (filter-map-into person-vector (person-over 16) person-name (into-vector #:size 2)) )
        
(printf "10 names over 16: ~a\n"
        (filter-map-into person-vector (person-over 16) person-name (into-vector #:size 10)) )