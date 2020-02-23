#lang racket/base

(require "../core/models.rkt")

(define (item-to-string item)
  (string-append
    (item-name item) ", "
    (number->string (item-sell-in item)) ", "
    (number->string (item-quality item))))

(display "actual")
(newline)

(let ((items (initialize-items!)) (days 31))
  (define (loop the-day the-items)
    (cond ((< the-day days)
           (display (string-append "-------- day " (number->string the-day) " --------"))
           (newline)
           (display "name, sell-in, quality")
           (newline)
           (for-each (lambda (item)
              (display (item-to-string item))
              (newline))
            the-items)
           (newline)
           (loop (+ the-day 1)
                 (update-quality the-items)))))

  (loop 0 items))

