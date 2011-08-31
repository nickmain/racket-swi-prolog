#lang racket
(require "swipl-ffi.rkt")
(require ffi/unsafe) 
(require ffi/cvector)

(provide (all-defined-out)) ;TODO - refine the exports

;;--get a string from an atom in a term
(define (atom->string term)
  (let ([chars (make-cvector _string 1)])
    (PL_get_atom_chars term (cvector-ptr chars))
    (cvector-ref chars 0)))

;;-define a functor
(define (functor name arity) 
  (PL_new_functor (PL_new_atom (symbol->string name)) arity))
(define-syntax-rule [define-functor defnName name arity]
  (define defnName (functor name arity)))

;;-define a predicate
(define (predicate name arity) 
  (PL_predicate (symbol->string name) arity #f))
(define-syntax-rule [define-predicate defnName name arity]
  (define defnName (predicate name arity)))

;;--common predicates
(define-predicate pred-consult    'consult      1)
(define-predicate pred-make       'make         0)
(define-predicate pred-term->atom 'term_to_atom 2)

; queries

;;--make a set of term refs
(define (term-refs size)
  (if (= size 1) (PL_new_term_ref) (PL_new_term_refs size))) 

;;--access the nth term ref (zero based index)
(define (term-ref-at terms index)
  (if (= index 0) terms (ptr-add terms index)))
   
;; call a predicate with a set of terms - return the terms or false
(define (call-pred-with pred terms)
  (if (PL_call_predicate #f PL_Q_NORMAL pred terms)
      terms
      #f))

;; call a predicate with scheme object args - return the term refs or false
(define (call-predicate pred . args)
  (call-pred-with pred (objs->terms args)))

;;--common predicate calls
(define (consult path)
  (in-foreign-frame (call-predicate pred-consult (string->symbol path))))

(define (make)
  (in-foreign-frame (call-predicate pred-make)))

;;--Wrap code in a SWIPL foreign frame
(define-syntax-rule [in-foreign-frame code ...]
  (let ([frame-handle (PL_open_foreign_frame)])
    (begin code ...)
    (PL_close_foreign_frame frame-handle)))

;;--construct a Prolog term from a Scheme object - return the term ref
;;
;; symbol   -> atom
;; string   -> parsed as a term
;; number   -> number
;; vector   -> compound term  #(foo a b) -> foo(a,b)
;; list     -> list
;; boolean  -> variable
;; cpointer -> term_t
;; 
(define (set-term! term obj)
  (cond
    ((symbol?  obj) (PL_put_atom_chars term (symbol->string obj)))
    ((string?  obj) (PL_chars_to_term  obj  term))
    ((integer? obj) (PL_put_integer    term obj))
    ((number?  obj) (PL_put_float      term obj))
    ((boolean? obj) (PL_put_variable   term))
    
    ((list?    obj) (if (null? obj)
                        (PL_put_nil term)
                        (PL_cons_list term
                                      (obj->term (car obj))
                                      (obj->term (cdr obj)))))
    
    ((vector?  obj) (let* ([arity  (- (vector-length obj) 1)]
                           [args   (objs->terms (cdr (vector->list obj)))]
                           [functr (functor (vector-ref obj 0) arity)])
                      (PL_cons_functor_v term functr args)))
    
    ((cpointer? obj) (PL_put_term term obj)))
  
  term)

;;--create a new term-ref and set it to the given object
(define (obj->term obj)
  (set-term! (PL_new_term_ref) obj))
  
;;--make term refs from a list of objects
(define (objs->terms objs)
  (let* ([arity  (length objs)]
         [terms  (if (> arity 0) (term-refs arity) #f)])
    ;-fill in the term slots
    (if (> arity 0 )
        (do 
            ((i 0 (+ i 1)))
          ((>= i arity))
          
          (set-term! 
            (term-ref-at terms i) 
            (list-ref objs i)))
        #f)
    
    terms))

;;--convert a term to an atom and return the term-ref
(define (term->atom term)
  (let ([terms (term-refs 2)])
    (set-term! terms term)
    (call-pred-with pred-term->atom terms)
    (term-ref-at terms 1)))