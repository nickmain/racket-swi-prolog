#lang racket

(require "swipl.rkt")
(require "swipl-ffi.rkt")

(define [test]
  (in-foreign-frame
    (let ([terms (term-refs 3)])      
      (call-pred-with (predicate 'current_op 3) terms)
      (write (term->obj terms))
      (newline)
      (write (term->obj (term-ref-at terms 1)))
      (newline)
      (write (term->obj (term-ref-at terms 2))))))

;;--note - registering a foreign predicate more than once will seg-fault the process
;(PL_register_foreign "racket_test" 0
;                     (λ () (write "hoot !!!")(newline) #f) 0)

;(consult "/Users/nmain/Desktop/test2")

(define (test-box)
  (term->string (let ([var (box #f)])
                  (obj->term (vector 'foo var #f var)))))