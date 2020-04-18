#lang racket


(define (wave s)
  (for/list ([i (string-length s)] #:when (not (char-whitespace? (string-ref s i))))
    (list->string
     (list-update (string->list s) i char-upcase))))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define tests
    (test-suite
     "Meetings"
     (test-case "Example"
       (check-equal? (wave "hello") '("Hello" "hEllo" "heLlo" "helLo" "hellO")))

     (test-case "Whitespace"
       (check-equal? (wave " ") '()))

     (test-case "Empty string"
       (check-equal? (wave "") '()))

     (test-case "Two words"
       (check-equal? (wave "hello there") '("Hello there" "hEllo there" "heLlo there" "helLo there" "hellO there" "hello There" "hello tHere" "hello thEre" "hello theRe" "hello therE")))))

  (run-tests tests))
