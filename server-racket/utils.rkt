#lang racket/main

(provide bytes->string
         root-folder)

; bytes->string: bytes -> string or #false
; Assumes that the given input is a byte and tries to convert it to a string. If successful,
; it returns the resulting string. If not, it returns #false
(define (bytes->string b)
  (with-handlers ([exn:fail:contract? (const #false)])
    (bytes->string/utf-8 b)))

(define root-folder
  (simplify-path
    (build-path
      (syntax-source #'root-folder)
      "..")))
