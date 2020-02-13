#lang racket/base

(require racket/contract)

(provide celsius->fahrenheit)

(define/contract AbsoluteC real? -273.15)
(define/contract AbsoluteF real? -459.67)

(define/contract (celsius->fahrenheit c)
  (-> (and/c real? (>=/c AbsoluteC))
      (and/c real? (>=/c AbsoluteF)))

  (+ (* 9/5 c) 32))

(module+ test
  (require rackunit)
  (check-equal? (celsius->fahrenheit 35) 95)
  (check-equal? (celsius->fahrenheit -273.15) -459.67))
