#lang br/quicklang

(provide read-syntax)
(provide (rename-out [stacker-module-begin #%module-begin]))
(provide handle)
(provide + *)

(define (read-syntax path port)
  (define source-lines (port->lines port))
  (define source-datums (format-datums '(handle ~a) source-lines))
  (define module-datum `(module stacker-module "stacker.rkt"
                          ,@source-datums))
  (datum->syntax #f module-datum))

#|
   Within the expander, we have three basic techniques for adding bindings to code:
   1. define macros that rewrite certain code as other code at compile  :time
   2. define functions that are invoked at runtime
   3. import bindings from existing Racket modules, which can include both macros and functions
|#
(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #|
     This time, we’re using some new nota­tion—the prefix #'—to make code into a syntax object. It’s similar to the '
     prefix we’ve already used that makes code into a datum. But the #' prefix not only creates the datum, but also
     captures the current lexical context, and attaches that to the new syntax object. Lexical context is the fancy
     phrase for “a list of avail­able vari­ables”. In prac­tice, what it means is that a syntax object made with #'
     will be able to access all the vari­ables defined at that point in the code.
 :   |#
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))

(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle (arg #f))
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? - arg) (equal? * arg) (equal? / arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))
