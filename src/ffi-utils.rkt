#lang racket
(require ffi/unsafe) 
(require ffi/cvector)

(provide native-lib)

;;--create a define for a single native function
(define-for-syntax (proc-def proc-name ret-type arg-types lib-id)
  (with-syntax
      ((name-string (datum->syntax proc-name (symbol->string (syntax->datum proc-name))))
       ((arg-type-els ...) arg-types)
       (lib-id    lib-id)
       (ret-type  ret-type)
       (proc-name proc-name))
    #'(define proc-name 
        (get-ffi-obj name-string lib-id 
                     (_fun arg-type-els ... -> ret-type)))))    

;;--recursive building of list of defines
(define-for-syntax (def-proc lib-id proc-defs)
  (syntax-case proc-defs ()
    (() #'())
    ((ret-type proc-name arg-types . more)
     (with-syntax
         ((remaining-defs (def-proc lib-id #'more))
          (this-def       (proc-def #'proc-name #'ret-type #'arg-types lib-id)))
       (with-syntax 
           ((def-list (datum->syntax proc-defs (cons #'this-def #'remaining-defs))))
         #'def-list)))))

;;--wrap function defines in a begin
(define-for-syntax (def-procs lib-id procs)
  (with-syntax
      (((def-list ...) (def-proc lib-id procs)))
    #'(begin def-list ...)))

;;--define a native library and functions
(define-syntax native-lib
  (λ (syn)
    (syntax-case syn ()
      ((_ lib-path . procs)
       (with-syntax
           (((the-native-lib) (generate-temporaries #'(i))))
         (with-syntax
             ((proc-defs (def-procs #'the-native-lib #'procs)))
           #'(begin
               (define the-native-lib (ffi-lib lib-path))
               proc-defs)))))))
  
