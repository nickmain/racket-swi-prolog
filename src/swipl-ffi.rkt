#lang racket
;----------------------------------------------------------------------------
; SWI Prolog native library and FFI functions
;----------------------------------------------------------------------------

(provide (all-defined-out))

(require ffi/unsafe) 
(require ffi/cvector)
(require "ffi-utils.rkt")

(define fid_t       _pointer) ;;handle to a foreign frame
(define atom_t      _pointer) ;;handle to an atom
(define term_t      _pointer) ;;handle to a term
(define predicate_t _pointer) ;;handle to a predicate
(define module_t    _pointer) ;;handle to a module
(define qid_t       _pointer) ;;handle to a query

(define PL_Q_NORMAL	2) ;;open-query flag

(native-lib 
  "/opt/local/lib/swipl-5.10.4/lib/i386-darwin10.7.0/libswipl"
  
  _int        PL_initialise          ( _int _pointer )
  _int        PL_halt                ( _int )
  fid_t       PL_open_foreign_frame  ()
  _void       PL_close_foreign_frame ( fid_t )
  term_t      PL_new_term_refs       ( _int )
  predicate_t PL_predicate           ( _string _int _string )
  qid_t       PL_open_query          ( module_t _int predicate_t term_t )
  _int        PL_next_solution       ( qid_t )
  _void       PL_cut_query           ( qid_t )
  _int        PL_call_predicate      ( module_t _int predicate_t term_t )
  _int        PL_get_atom_chars      ( term_t _pointer )
  atom_t      PL_new_atom            ( _string )
  
  _void       PL_register_atom       ( atom_t )
  _void       PL_unregister_atom     ( atom_t ))


(if (PL_initialise 1 (cvector-ptr 
                      (list->cvector '("racket" #f) _string/latin-1)))
    (begin (display "SWI Prolog initialized") (newline))
    (begin (display "SWI Prolog initialization failed") (newline)))

;;--Wrap code in a SWIPL foreign frame
(define-syntax-rule [in-foreign-frame code ...]
  (let ([frame-handle (PL_open_foreign_frame)])
    (begin code ...)
    (PL_close_foreign_frame frame-handle)))