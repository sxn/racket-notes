#lang br/quicklang

(provide + * / -)
(provide (rename-out (stackerizer-module-begin #%module-begin)))

(define-macro (stackerizer-module-begin EXPR)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR)))))

(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...))
         #'(list 'OP FIRST (OP NEXT (... ...)))]) ...))

(define-ops + * - /)
