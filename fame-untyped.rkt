#lang typed/racket
(require typed/racket/class)

jaz linh nguyen

;; * FAME: Functional And Monotonic Environments

;; ** About =fame%=s

;; A =fame%= is
;; - a mapping from symbols to values
;; - either immutable functional mappings
;; - or (optionally) monotonically extensible mappings
;; - along with other optional features including
;;   - =fame%=s can extend other =fame%=s
;;   - mappings can have attributes which are also =fame%=s

;; There are multiple implementations of =fame%=s
;; Base class features
;; - Construction: Specifying Symbols, Values, Attributes, Inheritance
;; - Lookup: Bindings, Values, Attributes
;; - Monotonic mutable extension where supported
;; Optional features (available via /Mixins?/)
;; - inheritance: This =fame%= extends 1 or more other =fame%=s
;; - Individual inheritance
;; - Monotonic mutable extension of bindings
;; - Monotonic mutable extension of attributes

(define-type Binding (U #f fame% (Pairof Any fame%)))
(define fame%-no-value-yet 'fame%-no-value-yet)
(define (empty-binding) fame%no-value-yet)
(define (null-binding f) )

(define fame% (class object%
                (init bindings)
                (: get-binding (Symbol -> Binding))
                (define (get-binding v)
                  (or (assq v bindings)
                      (for/or ([f base-fames]) (assq v f)) ) )
                (: bound? (Symbol -> Boolean))
                (define/public (bound? v) (not (not get-binding v)))
                (define/public binding id)
                (define/public value id)
                (super-new)
                (define bindings '())
                (define base-fames '())
                ))

(class* extendable-fame% fame% (extendable-fame%)

  )
