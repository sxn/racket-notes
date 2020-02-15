#lang racket/base

(require gregor
         net/url-string
         web-server/dispatch
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

(define (not-found req)
  (minimal-response-for-code 404))

(define (internal-server-error url ex)
  (log-error "[~a] ~a ~~~~> ~a"
             (datetime->iso8601 (now/utc))
             (url->string url)
             (exn-message ex))
  (response/xexpr
    `(html ((lang "en"))
      (head (title "Ouch!"))
      (body (p "It seems that something went wrong. please come back shortly!")))))

(define (get-homepage req n)
  (response/xexpr
    `(html ((lang "en"))
      (head (title "Rational numbers are great"))
      (body (p ,(number->string (/ 1 n)))))))

(define-values (dispatcher url-generator)
  (dispatch-rules
    [[(integer-arg)] #:method "get" get-homepage]))

(module+ main
  (serve/servlet
    dispatcher
    #:port 8000
    #:command-line? #f
    #:file-not-found-responder not-found
    #:launch-browser? #f
    #:servlet-regexp #rx""
    #:servlet-responder internal-server-error))
