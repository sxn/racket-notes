#lang racket/base

(require racket/path
         web-server/http
         web-server/servlet
         web-server/servlet-env
         web-server/templates
         "../utils.rkt")

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
    (response/xexpr (include-template/xml "templates.html")))
  
  (send/suspend/dispatch response-generator))

(serve/servlet (λ (req) (dispatcher req))
  #:launch-browser? #t

  #:server-root-path (build-path root-folder "ch06")
  #:extra-files-paths (list (build-path root-folder "ch06" "static"))

  #:servlet-path "/"
  #:mime-types-path (build-path root-folder "ch06" "mime.types")

  #:listen-ip #f
  #:port 8000)
