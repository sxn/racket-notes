#lang racket/base

(require brag/support
         "parser.rkt")

(provide read-syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod brainfuck/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))

(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       [(char-set "><-.,+[]") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))

  next-token)
