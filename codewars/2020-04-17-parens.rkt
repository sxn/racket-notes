;; Write a function called that takes a string of parentheses, and determines if
;; the order of the parentheses is valid. The function should return `true` if
;; the string is valid, and `false` if it's invalid.
#lang racket

(define (validParens the-string)
  (define-values (valid pairs)
    (for/fold
        ([valid #t] [pairs 0])
        ([c (string->list the-string)])
      #:break (false? valid)
      (cond
        [(char=? c #\()
         (values #t (add1 pairs))]

        [(and (char=? c #\)) (positive-integer? pairs))
         (values #t (sub1 pairs))]

        [(and (char=? c #\)) (not (positive-integer? pairs)))
         (values #f (sub1 pairs))]

        [else (values valid pairs)])))

  (and valid (zero? pairs)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "Valid parentheses"
    (test-case "Valid"
      (begin
        (check-equal? (validParens "()") #t)
        (check-equal? (validParens "()()") #t)
        (check-equal? (validParens "()()") #t)
        (check-equal? (validParens "(text)(text)") #t)
        (check-equal? (validParens "(())") #t)))

    (test-case "Invalid"
      (begin
        (check-equal? (validParens ")") #f)
        (check-equal? (validParens ")(") #f)
        (check-equal? (validParens "text)(text)") #f)
        (check-equal? (validParens "()(") #f))))))
