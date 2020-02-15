#lang racket/base

(require web-server/dispatch
         web-server/http/response-structs
         web-server/http/xexpr
         web-server/servlet-env)

(define messages
  (hasheq 200 "OK"
          201 "Created"
          202 "Accepted"
          204 "No Content"

          300 "Multiple Choices"
          301 "Moved Permanently"
          302 "Found"
          303 "See Other"
          307 "Temporary Redirect"
          308 "Permanent Redirect"

          400 "Bad Request"
          401 "Unauthorized"
          403 "Forbidden"
          404 "Not Found"
          405 "Method Not Allowed"
          411 "Length Required"
          415 "Unsupported Media Type"
          422 "Unprocessable Entity"
          423 "Locked"

          500 "Internal Server Error"))

(define fallback-message
  "Unknown")

(define (message-for-code code)
  (hash-ref messages code fallback-message))

(define (minimal-response-for-code code)
  (response/full 
    code
    (string->bytes/utf-8 (message-for-code code))
    (current-seconds)
    #f
    (list)
    (list)))

(define (method-not-allowed req)
  (minimal-response-for-code 405))

(define (not-found req)
  (minimal-response-for-code 404))

(define greetings/hash
  (hash "en" "Hello!"
         "de" "Hallo!"
         "ro" "Salut!"
         "fr" "Bonjour!"
         "jp" ""))

(define languages (hash-keys greetings/hash))

(define num-languages (length languages))

(define (hello req)
  (response/xexpr
    `(html (body (h1 "Hello there!")))))

(define (hello+lang req lang)
  (define greeting (hash-ref greetings/hash lang #f))
  (cond ([string? greeting] (response/xexpr `(htm (body (h1 ,greeting)))))
        (else (not-found req))))

(define (hello+test req lang test)
  (define greeting (hash-ref greetings/hash lang #f))
  (cond ([string? greeting] (response/xexpr `(htm (body (h1 ,greeting) (p ,test)))))
        (else (not-found req))))

(define-values (dispatcher url-generator)
  (dispatch-rules
    [("hello") hello] ; matches `/hello`
    [("hello" (string-arg)) hello+lang] ; matches `/hello/<something>`
    [("hello") #:method (regexp ".*") method-not-allowed]))


(module+ main
  (serve/servlet
    dispatcher
    #:port 8000
    #:command-line? #f
    #:file-not-found-responder not-found
    #:launch-browser? #f
    #:servlet-regexp #rx""))
