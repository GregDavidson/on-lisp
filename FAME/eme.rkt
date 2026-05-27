#lang typed/racket

;; * eme: Enriched Monotonic Environments for Racket

;; ** About the eme

;; An eme is
;; - a mapping from symbols to arbitrary Racket values
;; - mutable, but only monotonically so
;; - optionally enriched with attributes

;; Conceptually an eme is
;; - a mapping from symbols to eme-bindings
;; The representation of eme-bindings is hidden
;; but internally eme-bindings consist of:
;; - a symbol serving as a binding name, aka a key
;; - an optional, single-assignment value
;; - an optional list of attributes
;;  where attributes consist of
;; - a symbol serving as an attribute name
;; - an optional, single-assignment value
;; and attributes do NOT currently have
;; - inheritance from a super-attribute map
;; - their own attributes
;; - but it would be awe-fully easy to add such!

;; When bindings and/or attributes are named, we say that they are declared.
;; Bindings and attributes may be given a value once only, hence they are monotonic.
;; When bindings and/or attributes have a value, we say that they are defined.
;; Once a binding is defined, no changes may be made to its attributes!
;; - i.e. no new attributes can be declared, no attributes can be defined!
;; The names of the attributes of any binding must be unique.

;; An eme can contain local bindings or inherit its bindings from a base eme
;; The names of local bindings must be unique
;; Local bindings shadow (hide) any possible inherited binding with the same name

;; Some irrevocable options are available
;; open (default false) :: new local bindings can be created after eme is created
;; final (default false) :: bindings can be declared and then defined later

;; Copyright (c) J. Greg Davidson 2021
;; License: AGPLv3

;; ** Imports & Exports

;; ??? Do we need to explicitly export the eme type? ???
;; The only non-pure-value type needed to use the package is eme
;; ??? How do we expose it as an opaque type ???
;; ??? Maybe we just =provide= the constructor and recognizer ???

;; ** constructor function eme

(define-type nested-symbol-type (List Symbol))
(define-type symbol-value-type (List Symbol Any))
(define-type nested-symbol-maybe-value-type (U nested-symbol-type symbol-value-type))
(define-type symbol-maybe-value-type (U Symbol symbol-value-type))
(define-type binding-spec-type ; key might have a value
  (Listof
   (U symbol-maybe-value-type
      (Pairof nested-symbol-maybe-value-type (Listof symbol-maybe-value-type]) )l) )
(define-type template-spec-type ; key has no value
  (Listof
   (U symbol
      (Pairof nested-symbol-type (Listof symbol-maybe-value-type]) )l) )
(provide:
 [eme
  (->* ; fancy version of -> for defining procedures
   ()                 ; no mandatory arguments
   (#:open Boolean)   ; optional Boolean keyword argument
   (#:final Boolean)  ; optional Boolean keyword argument
   (#:inherit eme)    ; optional eme keyword argument
   #:rest binding-spec-type ; 0 or more additional binding-spec-type arguments
   eme ; return type
   ) ]
 [eme? (-> Any Boolean)] )
;; Examples:
#;(eme) ; create a new eme with no local or inherited bindings
;; Any new eme can inherit defined bindings from an existing eme
#;(eme #:inherit an-old-eme)
;; Local bindings shadow any inherited bindings.
;; New bindings in a base eme after inheritance are not seen
;; We can declare or define initial local bindings:
#;(eme key1 key2) ; two bindings declared but not defined yet
#;(eme key1 (v2 key2val)) ; one declaration, one definition
;; Declared or defined bindings can declare or define associated attributes
#;(eme key1 ((key2) attribute1 (attribute2 attribute2val)))
#;(eme key1 ((key2 key2val) attribute1 (attribute2 attribute2val)))
;; keys and attributes must be symbols
;; keys and attributes w/o values are declarations waiting for a value to be defined
;; a key with attributes must be in a sublist, with or without a value
;; defining attributes is closed once a binding is defined

;; ** monotonic function eme-add

;; eme-add adds binding(s) to an existing eme from specs
;; the eme is (monotonically) altered!  And returned.
(provide: eme-add (-> eme binding-spec-type * eme))
;; Keys and attributes can only be defined once
;; Any definitions must be compatible with any declarations
;; Any declarations must be compatible with any other declarations

;; ** eme selector functions

(: eme-key? (-> Symbol Boolean))
(provide eme-key?)
(: eme-value? (-> Symbol Boolean))
(provide eme-value?)
(: eme-value (-> Symbol Any))
(provide eme-value)
(: eme-lookup (-> Symbol (U symbol-maybe-value-type #f)))
(provide eme-lookup)

(: eme-attr? (-> Symbol Boolean))
(provide eme-attr?)
(: eme-attr (-> Symbol Any))
(provide eme-attr)
(: eme-attr-lookup (-> Symbol (U symbol-maybe-value-type #f)))
(provide eme-attr-lookup)

;; ** eme spec and template functions

(: eme-spec (-> Symbol binding-spec-type))
(provide eme-spec)
;; an eme-template could be useful for declaring a similar binding
(: eme-template (-> Symbol binding-template-type))
(provide eme-template)

;; ** Eme Closing, Freezing, Cleaning, Flattening

;; All of these features are questionable.  If the
;; functionality is adopted, maybe they should be
;; folded in as options for new-eme!

;; *** Closed emes cannot add new bindings or properties.

(: eme-open? (-> eme Boolean))
(provide eme-open?)
(: eme-closed? (-> eme Boolean))
(provide eme-closed?)
(define (eme-open? eme)
  (not (eme-open? eme)) )
(: eme-close (-> eme Void))
(provide eme-close)
(define (eme-open? eme)
  (eme-set-open eme #f)) ) ;; How do we do this???

;; *** Final emes cannot change.

(: eme-final? (-> eme Boolean))
(provide eme-final?)
(: eme-freeze (-> eme Void))
(provide eme-freeze)

;; *** Clean emes have nothing declared but undefined

;; Is this a good idea?  

(: eme-clean? (-> eme Boolean))
(provide eme-clean?)
(: eme-clean! (-> eme Void))
(provide eme-clean!)

;; *** Flattened emes have been deep copied
;; All unshadowed inherited bindings are now local

;; Is this a good idea?  

(: eme-flat? (-> eme Boolean))
(provide eme-flat?)
(: eme-flatten (-> eme Void))
(provide eme-flatten)

;; ** Implementation

;; An eme contains a local-eme and an inherited super-eme.
;; The super-eme will be searched only when a binding is not found in the local-eme.
;; There can be only one binding of a given symbol in any local-eme.
;; Local bindings are monotonic, once a value is established, it cannot be modified.

;; Bindings can have
;; - a value or no-eme-value-yet
;;   - once set, they can't change
;; - attributes
;;   - inherited-attributes
;;     - specified at definition time
;;   - local-attributes
;;     - monotonic and can't contradict inherited-attributes
;;   Attributes can only be set once

(struct eme-no-value ()) ; no value yet
(define-type eme-binding-assoc (Pairof Symbol (Pairof Any (Listof (Pairof Symbol Any)))))

(: binding-spec->assoc (-> binding-spec-type eme-binding-assoc))
;; Need to make sure that the binding keys are distinct!!!
;; Need to make sure that the attr keys for each bindings are distinct!!!
;; Maybe sort them???
(define (binding-spec->assoc spec)
  (cond [(symbol? spec) (list spec (eme-no-value))]
        [ (not (pair? spec)) (raise-argument-error 'binding-spec->assoc 'car 1) ]
        [ (and (symbol? (car spec)) (null? (cdr spec)))
          (list (car spec) (eme-no-value)) ]
        [ (and (symbol? (car spec)) (pair? (cdr spec)) (null? (cddr spec)))
          (list (car spec) (cadr spec)) ]
        [ (not (pair? (car spec))) (raise-argument-error 'binding-spec->assoc 'caar? 1) ]
        [ (and (symbol? (caar spec)) (null? (cdar spec)))
          (cons (list (caar spec) (eme-no-value)) (eme-attrs (cdr spec))) ]
        [ (and (symbol? (caar spec)) (pair? (cdar spec)) (null? (cddar spec)))
          (cons (list (caar spec) (cadar spec)) (eme-attrs (cdr spec))) ]
        [else (raise-argument-error 'binding-spec->assoc 'binding 1)] ) )

(: eme-attrs (-> (Listof symbol-maybe-value-type) eme-binding-assoc))
(define (eme-attrs specs) (map eme-attr specs) )
(: eme-attr (-> symbol-maybe-value-type (Pairof Symbol Any)))
(define (eme-attr spec)
  (cond [(symbol? spec) (cons spec (eme-no-value))]
        [ (not (pair? spec)) (raise-argument-error 'eme-attr 'car 1) ]
        [ (and (symbol? (car spec)) (null? (cdr spec)))
          (cons (car spec) (eme-no-value)) ]
        [ (and (symbol? (car spec)) (pair? (cdr spec)) (null? (cddr spec)))
          (cons (car spec) (cadr spec)) ]
        [else (raise-argument-error 'eme-attr 'cdr 1)] ) )

(: binding-specs->assocs (-> (Listof binding-spec-type) (Listof eme-binding-assoc)))
(define (binding-specs->assocs specs)
  (define (keys-unique? alist)
    (or (null? alist)
        (and (not (assoc (caar alist) (cdr alist)))
             (keys-unique? (cdr alist)) ) ) )
  (let ( [bindings (map binding-spec->assoc specs)] )
    (when (not (and (keys-unique? bindings)
                    (every (λ (binding) (keys-unique? (cddr binding)))) ))
      (raise-argument-error 'binding-specs-assocs 'keys-unique? 1) )
    bindings ) )

(struct no-eme ())
(struct eme ( [open : Boolean]
              [final : Boolean]
              [clean : Boolean]
              [packed : Boolean]
              [local-declarations : (Listof eme-binding-assoc)]
              [local-definitions : (Listof eme-binding-assoc)]
              [super-eme : eme] ) #:constructor-name 'make-full-eme )

(: declared-binding? (-> eme-binding-assoc Boolean))
(define (declared-binding? eba) (eme-no-value? (cadr eba)))
(: defined-binding? (-> eme-binding-assoc Boolean))
(define (defined-binding? eba) (not (declared? eba)))

(define (eme
         #:open [open #f]
         #:final [final #f]
;         #:clean [clean #f]
;         #:packed [packed #f]
         #:inherit [super-eme (no-eme)] . rest )
  (let ( [bindings (binding-specs->assocs rest)] )
    (make-full-eme
     open final #f #f
     (filter declared-binding? bindings)
     (filter defined-binding? bindings)
     super-eme ) ) )

;; Two eme-binding-assoc lists unify into a combined eme-binding-assoc
;; if they can be made equal by single assignments in places where one of
;; them is defined and the other is not.

(: unify-binding (->
                  eme-binding-assoc ; binding1
                  eme-binding-assoc ; binding2
                  (U #f eme-binding-assoc) ; the unification, if possible
                  ))
(define (unify-binding binding1 binding2)
  (let (
        [k1 (car binding1)] [v1 (cadr binding1)][props1 (cddr binding1)]
        [k2 (car binding2)] [v2 (cadr binding2)][props2 (cddr binding2)]
        (if (not (eq? k1 k2))
            #f
            (let ( [no-v1 (eme-no-value? v1)] [no-v2 (eme-no-value? v1)] )
                  (if (not (or (and no-v1 nov2) (equal? v1 v2)))
                      #f
                      (let ( [props (unify-props props1 props2)])
                        (and props
                            (cons k1 (cons (if no-v2 v1 v2) props)) ) ) ) ) ) )) )

(: unify-prop (-> eme-binding-assoc eme-binding-assoc (U #f eme-binding-assoc)))
(define (unify-prop prop1 prop2)
  (let (
        [k1 (car prop1)] [v1 (cdr prop1)]]
        [k2 (car prop2)] [v2 (cdr prop2)]]
        (if (not (eq? k1 k2))
            #f
            (let ( [no-v1 (eme-no-value? v1)] [no-v2 (eme-no-value? v1)] )
              (if (not (or (and no-v1 nov2) (equal? v1 v2)))
                  #f
                  (cons k1 (cons (if no-v2 v1 v2) props)) ) ) ) )) )

(: unify-alists
   (->
    (-> eme-binding-assoc eme-binding-assoc (U #f eme-binding-assoc))
    (Listof eme-binding-assoc) (Listof eme-binding-assoc)
    (U #f (Listof eme-binding-assoc)) ) )
;; the first parameter could be either unify-binding or unify-props
(define (unify-alists unify list1 list2)
  (: unify-accum
     (-> (Listof eme-binding-assoc) (Listof eme-binding-assoc) (U #f (Listof eme-binding-assoc))) )
  (define (unify-accum list1 list2 accum)
    (if (null? list1)
        (append list2 accum)
        (let* ( [x1 (car list1)] [x2 (assoc (car x1) list2)] )
          (if (null? x2)
              (unify-accum (cdr list1) list2 (cons x1 accum))
              (let ( [x12 (unify x1 x2)] )
                (and x12
                     (unify-accum
                      (cdr list1)
                      (filter (λ (x) (not (eq? x1 x))) list2)
                      (cons x12 accum) ) ) ) ) ) ) )
  (unify-accum list1 list2 '()) )

;; LEFT OFF HERE!!!

;; Need to rework the consistency code from above to use the unify machinery!

;; Need to actually present the failures!  Perhaps with values!

(: rare-eme-binding (-> Symbol Any eme-attributes eme-binding))
(define (rare-eme-binding name value attrs) ;; not quite raw
  ; check for duplicate names in the attributes!
  (when #f (raise-argument-error 'rare-eme-binding 'unique-keys 3))
  (raw-eme-binding name value attrs) )
(: eme-declare (-> Symbol Any eme-attributes eme-binding))
(define (eme-declare name . attrs) (rare-eme-binding name (eme-no-value) attrs))
(define (eme-define name value . attrs)
  (when (eme-no-value? value) (raise-argument-error 'eme-define 'eme-no-value? 2))
  (rare-eme-binding name value attrs) )
(define (eme-set eme-binding))

;; Should automatically get
(eme-binding? : -> Any Boolean) ;; tests if something is an eme-binding
;; eme-binding constructs an eme-binding structure from a Symbol, Any value and an attribute list
(eme-binding : -> Symbol Any eme-poperties eme-binding)

;the default eme-binding would look like this:
(define my-thing (eme-binding 'foo 'fubar (list 'color 'red 'size 'big)))
; I want this:
(define foo (eme-binding 'foo 'fubar 'color 'red 'size 'big))
(define foo (eme-binding-without-value 'foo 'color 'red 'size 'big))

;; default selectors
(eme-binding-symbol : -> eme symbol) ;; that's fine
(eme-binding-value : -> eme Any) ;; error if not set
;; Maybe I want:
(eme-binding-value? : -> eme Boolean) ;; tells me if it's set
;; default attributes selector:
(eme-binding-attributes eme) ;; not sure I want to be able to get all attributes in a big batch
;; what I want:
(eme-binding-attribute foo 'color) ;; should return red

(struct eme-pair ([local-eme : (Listof eme-binding)] [super-eme : eme]))
(define-type eme (U empty-eme eme-pair) #:constructor-name 'make-full-eme )
(define (declare-eme-binding ))

;; ** Ideas for the evolution of this package

;; Limitations which could be released or imposed
;; - eme-attributes are association lists vs. not recursive emes
;; - bindings can only be defined once

;; Declarations could be held in a separate list and then upgraded into definitions.
;; Advantages:
;; - No need for eme-no-value
;; - Bindings could be const - at expense of non-upgradable attribute lists
;; This system could also be used for attributes!
;; - This makes the idea of 

;; *** We could provide value guards
;; - via an attribute specifying a function which can
;;   - reject an unacceptable value
;;   - transform a raw value
;; - Could provide equal? and/or unify? methods for unifying (with) declarations

;; *** We could provide monotonic values

;; - Collections which can grow

;; Relational emes for the win!

;; *** Interfaces to other facilities

;; Integration with command-line parameters

;; Integration with threads

;; Integration with function calls

;; *** Efficiency

;; How can we compile out
;; - most of the attribute overhead
;; - the lookup overhead
;; - other overhead
