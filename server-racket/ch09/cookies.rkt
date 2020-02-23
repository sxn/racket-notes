#lang racket/base

(require net/cookies/server
         net/url-string
         racket/string
         web-server/dispatch
         web-server/http/cookie-parse
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/xexpr
         web-server/servlet-env
         web-server/templates
         "../utils.rkt")

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

(define (render-homepage req) 

  (define cookies (request-cookies req))
  (define theme-cookie
    (findf
      (lambda (c) (string=? "theme" (client-cookie-name c)))
      cookies))

  (displayln (client-cookie-value theme-cookie))

  (response/xexpr
    #:cookies (list (make-cookie "theme" "red"))
    `(html
      (head (title "🍪")
            (link ((rel "shortcut icon") (href "/favicon.png")))
            (link ((rel "stylesheet") (href "/styles.css") (type "text/css")))
      (body
        (div ((class "content"))
          (h1 "🍪")
          (p "Oh. You need a little dummy text for your mockup? How quaint.")))))))

(define-values (dispatcher url-generator)
  (dispatch-rules 
    [("") #:method get render-homepage]))

(serve/servlet dispatcher
  #:launch-browser? #t

  #:server-root-path (build-path root-folder "ch09")
  #:extra-files-paths (list (build-path root-folder "ch09" "static"))

  #:servlet-path "/"
  #:mime-types-path (build-path root-folder "ch09" "mime.types")

  #:file-not-found-responder not-found
  #:listen-ip #f
  #:port 8000)
