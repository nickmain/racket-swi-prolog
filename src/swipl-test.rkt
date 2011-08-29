#lang racket

(require "swipl-ffi.rkt")
(require ffi/unsafe) 

(require ffi/cvector)

(define [test]
  (in-foreign-frame
    (letrec ([terms        (PL_new_term_refs 2)]
             [curr-pred    (PL_predicate "current_predicate" 1 #f)]
             [query        (PL_open_query #f PL_Q_NORMAL curr-pred terms)]
             [result       (PL_next_solution query)]
             [chars        (make-cvector _string 1)])
      (PL_cut_query query)
      (PL_call_predicate #f PL_Q_NORMAL (PL_predicate "term_to_atom" 2 #f) terms)
      (PL_get_atom_chars (ptr-add terms 1 ) (cvector-ptr chars))
      (write (cvector-ref chars 0)))))
  

;;(define [term->string term]