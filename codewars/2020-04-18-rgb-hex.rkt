;; The rgb() method is incomplete. Complete the method so that passing in RGB
;; decimal values will result in a hexadecimal representation being returned.
;; The valid decimal values for RGB are 0 - 255.  Any (r,g,b) argument values
;; that fall out of that range should be rounded to the closest valid value.
;; Note: Your answer should always be 6 characters long, the shorthand with 3
;; will not work here.
#lang racket

(define (rgb r g b)
  (define digits "0123456789ABCDEF")
  (define (byte->hex n)
    (string
     (string-ref digits (quotient n 16))
     (string-ref digits (remainder n 16))))

  (for/fold
      ([acc ""])
      ([n (in-list (list r g b))])
    (string-append
     acc
     (cond
       [(negative-integer? n) "00"]
       [(> n 255) "FF"]
       (else (byte->hex n))))))

(module+ test
  (require rackunit
           rackunit/text-ui)
  (run-tests
   (test-suite
    "Tests"

    (test-case
        "000000"
      (begin
        (check-equal? (rgb 0 -10 0) "000000")
        (check-equal? (rgb 0 0 0) "000000")))

    (test-case
        "9400D3"
      (check-equal? (rgb 148 0 211) "9400D3"))

    (test-case
        "FFFFFF"
      (begin
        (check-equal? (rgb 255 255 255) "FFFFFF")
        (check-equal? (rgb 255 555 255) "FFFFFF")))

    )))
