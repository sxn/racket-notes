#lang racket
#|
    Write a function that when given a number >= 0, returns an Array of ascending length subarrays.

    pyramid(0) => [ ]
    pyramid(1) => [ [1] ]
    pyramid(2) => [ [1], [1, 1] ]
    pyramid(3) => [ [1], [1, 1], [1, 1, 1] ]

    Note: the subarrays should be filled with 1s
|#

(define (pyramid n [initial '()])
  (if (= n 0)
      initial
      (pyramid (sub1 n) (cons (make-list n 1) initial))))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define tests
    (test-suite
     "Pyramid"
     (test-case "0"
       (check-equal?
        (pyramid 0)
        '()))
     (test-case "1"
       (check-equal?
        (pyramid 1)
        '((1))))
     (test-case "2"
       (check-equal?
        (pyramid 2)
        '((1) (1 1))))
     (test-case "3"
       (check-equal?
        (pyramid 3)
        '((1) (1 1) (1 1 1))))))

  (run-tests tests))
