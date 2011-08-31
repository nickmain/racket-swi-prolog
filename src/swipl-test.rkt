#lang racket

(require "swipl.rkt")
(require "swipl-ffi.rkt")

(define [test]
  (in-foreign-frame
    (let ([terms (term-refs 1)])      
      (call-pred-with (predicate 'current_predicate 1) terms)
      (write (atom->string (term->atom terms))))))

;;--note - registering a foreign predicate more than once will seg-fault the process
;(PL_register_foreign "racket_test" 0
;                     (λ () (write "hoot !!!")(newline) #f) 0)

;(consult "/Users/nmain/Desktop/test2")