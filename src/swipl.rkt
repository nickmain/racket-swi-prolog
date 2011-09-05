#lang racket
(require "swipl-ffi.rkt")
(require ffi/unsafe) 
(require ffi/cvector)

(provide (all-defined-out)) ;TODO - refine the exports

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
(define-predicate pred-call       'call         1)
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
;; char     -> integer code point
;; string   -> parsed as a term
;; number   -> number
;; vector   -> compound term  #(foo a b) -> foo(a,b)
;; pair     -> list (may be improper)
;; list     -> list
;; boolean  -> new variable
;; cpointer -> term_t
;; (box #f) -> variable, the same box yields the same variable and
;;             the box value is set to that common term (previous val is 
;;             overwritten)
;; (box ?)  -> recursive call to set contents 
;; 
(define (set-term! term obj)
  (cond
    ((symbol?  obj) (PL_put_atom_chars term (symbol->string obj)))
    ((char?    obj) (PL_put_integer    term (char->integer obj)))
    ((string?  obj) (PL_chars_to_term  obj  term))
    ((integer? obj) (PL_put_integer    term obj))
    ((number?  obj) (PL_put_float      term obj))
    ((boolean? obj) (PL_put_variable   term))
    
    ((null?    obj) (PL_put_nil term))
    ((pair?    obj) (PL_cons_list term
                                  (obj->term (car obj))
                                  (obj->term (cdr obj))))
  
    ((vector?  obj) (let* ([arity  (- (vector-length obj) 1)]
                           [args   (objs->terms (cdr (vector->list obj)))]
                           [functr (functor (vector-ref obj 0) arity)])
                      (PL_cons_functor_v term functr args)))
    
    ((cpointer? obj) (PL_put_term term obj))
    
    ((box? obj) (let ([val (unbox obj)])
                  (if (and (cpointer? val) val) ;not #f since that is cpointer
                      (PL_put_term term val)
                      (begin
                        (PL_put_variable term)                       
                        (set-box! obj term))))))
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

;;--parse a string to a term - return false if syntax error, with 
;;  the error in the term
(define (parse-term term-string term)
  (if (= 0 (PL_chars_to_term  term-string term))
      #f
      #t))

;;--build a comma term tree from a list of objects
(define (obj-list->comma-term objs)
  (cond
    ((null? objs) (obj->term null))
    ((= (length objs) 1) (obj->term (car objs)))
    (else (obj->term 
           (vector '|,| 
                   (obj->term (car objs))
                   (obj-list->comma-term (cdr objs)))))))
    
;;--convert a term to an atom and return the term-ref
(define (term->atom term)
  (let ([terms (term-refs 2)])
    (set-term! terms term)
    (call-pred-with pred-term->atom terms)
    (term-ref-at terms 1)))

;;--convert a term to a string
(define (term->string term)
  (let ([chars (make-cvector _string 1)])
    (PL_get_chars term 
                  (cvector-ptr chars)
                  (+ CVT_ATOM CVT_INTEGER CVT_FLOAT CVT_VARIABLE CVT_WRITE))
    (cvector-ref chars 0)))

;;--convert a char-code list to a string
(define (charlist-term->string term)
  (let ([chars (make-cvector _string 1)])
    (PL_get_chars term (cvector-ptr chars) CVT_LIST)
    (cvector-ref chars 0)))

;;--construct a Scheme object from a Prolog term - return the object
;;
;; atom     -> symbol
;; string   -> list of char codes - for normal strings
;; string   -> string - only for things such as syntax error strings
;; number   -> number
;; compound -> vector
;; list     -> list or improper list (if tail is not empty list)
;; variable -> mutable box containing var name symbol
;;
(define (term->obj term)
  (let ([type (PL_term_type term)])
    (cond 
      ((= type PL_VARIABLE) 
       (box (string->symbol (term->string term))))
      
      ((= type PL_STRING)
       (let ([int   (make-cvector _int 1)]
             [chars (make-cvector _string 1)])
         (PL_get_string term
                        (cvector-ptr chars)
                        (cvector-ptr int))
         (cvector-ref chars 0)))
      
      ((= type PL_ATOM)
       (let ([s (term->string term)])
         (if (string=? s "[]")  ; empty list ?
             null
             (string->symbol (term->string term)))))
      
      ((= type PL_INTEGER)  
       (let ([long (make-cvector _long 1)])
         (PL_get_long term (cvector-ptr long))
         (cvector-ref long 0)))
      
      ((= type PL_FLOAT)    
       (let ([double (make-cvector _double 1)])
         (PL_get_float term (cvector-ptr double))
         (cvector-ref double 0)))
      
      ((= type PL_TERM)
       (let ([atom-vec  (make-cvector atom_t 1)]
             [arity-vec (make-cvector _int 1)]
             [atom-term (term-refs 1)])
         (PL_get_name_arity term 
                            (cvector-ptr atom-vec) 
                            (cvector-ptr arity-vec))
         (PL_put_atom atom-term (cvector-ref atom-vec 0))
         (let ([functr   (term->obj atom-term)]
               [arity    (cvector-ref arity-vec 0)])
           
           (if (eqv? functr '|.|)
               
               ; recursive build of list or improper list
               (let ([arg-term1 (term-refs 1)]
                     [arg-term2 (term-refs 1)])
                 (PL_get_arg 1 term arg-term1)
                 (PL_get_arg 2 term arg-term2)
                 (cons (term->obj arg-term1)
                       (term->obj arg-term2)))
           
               ; else build vector for compound term
               (let ([arg-term (term-refs 1)]
                     [res-vec  (make-vector (+ arity 1))])
                 (vector-set! res-vec 0 functr)  ; functor at start of vector
                 (do ((i 1 (+ i 1))) ; loop over the args and insert into vector
                   ((> i arity))
                   (PL_get_arg i term arg-term)
                   (vector-set! res-vec i (term->obj arg-term)))
                 res-vec )))))
      
      (else #f))))

;;--load a theory with the given name-symbol (reloading that name will overwrite)
(define (load-theory name-symbol source-string)
  (let* ([stream (box #f)]
         [codes  (string->list source-string)]
         [query  (obj-list->comma-term 
                  (list (vector 'open_chars_stream codes stream)
                        (vector 'load_files name-symbol 
                                (list (vector 'stream stream)))))])
    (call-pred-with pred-call query)))
 
;;--macro for performing a query and executing a set of forms for
;;  each solution.
;;
;; (with-query [vars ...] query body ...)
;;
;; vars are symbols that may also appear in the query form - where they are
;; defined as boxes. The vars may also be referenced in the body where
;; they will be the unboxed result objects for each solution.
;;
(define-syntax with-query
  (λ (syn)
    (syntax-case syn ()
      ((_ [vars ...] query body ...)
       (let* ([var-names (syntax->datum #'(vars ...))]
              [vars-list (map (λ(v) (let ([vs (datum->syntax syn v)]) `[,vs (box #f)])) var-names)]
              [unbox-list (map (λ(v) (let ([vs (datum->syntax syn v)]) `[,vs (term->obj (unbox ,vs))])) var-names)])
         (with-syntax ((vars-list vars-list)
                       (unbox-list unbox-list))
           
         #'(let vars-list 
             (let* ([term    (obj->term query)]
                    [qhandle (PL_open_query #f PL_Q_NORMAL pred-call term)])
               (let loop ([result (PL_next_solution qhandle)])
                 (if (not (= result 0))
                     (let unbox-list
                       (begin body ...)
                       (loop (PL_next_solution qhandle)))
                     (PL_close_query qhandle)))))
         
           ))))))