#lang racket/base

(require racket/path
         web-server/http
         web-server/servlet
         web-server/servlet-env)

;; request? -> request?
; Takes a request as input and returns a copy with the `method` field set to #"GET"
(define (head->get req)
  (struct-copy request
               req
               [method #"GET"]))

;; response? -> response?
(define (strip-body res)
  ;; output-port? -> exact-nonnegative-integer?
  ; Takes an output port as input and writes the empty byte string to it;
  (define (write-nothing port)
    (write-bytes #"" port))

  (struct-copy response
               res
               [output write-nothing]))

(define (dispatcher request)
  (define (response-generator embed/url)
    (response/xexpr
      `(html (head (title "HEAD to GET")
                   (link ((rel "shortcut icon") (href "/favicon.png")))
                   (link ((rel "stylesheet") (href "/styles.css") (type "text/css"))))
             (body (h1 "Hi!")))))
  
  (send/suspend/dispatch response-generator))

(define (start req)
  (if (bytes=? (request-method req) #"HEAD")
      ; if a HEAD request is received, turn it into a GET request, pass it to the dispatcher and
      ; discard the body of the response it returns..
      (strip-body (dispatcher (head->get req)))
      (dispatcher req)))

(define this-folder
  (simplify-path (build-path (syntax-source #'this-folder) "..")))

(serve/servlet start
  #:launch-browser? #t

  #:server-root-path (build-path this-folder)
  #:extra-files-paths (list (build-path this-folder "static"))

  #:servlet-path "/"
  #:mime-types-path (build-path this-folder "mime.types")

  #:listen-ip #f
  #:port 8000)
