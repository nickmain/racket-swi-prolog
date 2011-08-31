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
(define functor_t   _pointer) ;;handle to a functor
(define predicate_t _pointer) ;;handle to a predicate
(define module_t    _pointer) ;;handle to a module
(define qid_t       _pointer) ;;handle to a query
(define foreign_t   _pointer) ;;return from foreign preds

(native-lib 
  "/opt/local/lib/swipl-5.10.4/lib/i386-darwin10.7.0/libswipl"
  
  _int        PL_initialise          ( _int _pointer )
  _int        PL_halt                ( _int )
  fid_t       PL_open_foreign_frame  ()
  _void       PL_close_foreign_frame ( fid_t )
  term_t      PL_new_term_refs       ( _int )
  term_t      PL_new_term_ref        ()
  predicate_t PL_predicate           ( _string _int _string )
  qid_t       PL_open_query          ( module_t _int predicate_t term_t )
  _int        PL_next_solution       ( qid_t )
  _void       PL_cut_query           ( qid_t )
  _int        PL_call_predicate      ( module_t _int predicate_t term_t )
  _int        PL_get_atom_chars      ( term_t _pointer )
  _int        PL_put_atom_chars      ( term_t _string )
  atom_t      PL_new_atom            ( _string )
  _void       PL_register_atom       ( atom_t )
  _void       PL_unregister_atom     ( atom_t )
  functor_t   PL_new_functor         ( atom_t _int )
  _int        PL_put_integer         ( term_t _long )
  _int        PL_put_float           ( term_t _double )
  _void       PL_put_variable        ( term_t )
  _int        PL_put_string_chars    ( term_t _string )
  _int        PL_chars_to_term       ( _string term_t )
  _void       PL_put_term            ( term_t term_t )
  _void       PL_put_nil             ( term_t )
  _int        PL_cons_functor        ( term_t functor_t term_t term_t )
  _int        PL_cons_list           ( term_t term_t term_t )
  _int        PL_register_foreign    ( _string _int (_fun -> foreign_t) _int ))

(if (PL_initialise 1 (cvector-ptr 
                      (list->cvector '("racket" #f) _string/latin-1)))
    (begin (display "SWI Prolog initialized") (newline))
    (begin (display "SWI Prolog initialization failed") (newline)))

(define PL_Q_NORMAL	2) ;;open-query flag

(define PL_FA_NOTRACE          #x01) ;; foreign cannot be traced
(define PL_FA_TRANSPARENT      #x02) ;; foreign is module transparent
(define PL_FA_NONDETERMINISTIC #x04) ;; foreign is non-deterministic
(define PL_FA_VARARGS          #x08) ;; call using t0, ac, ctx
