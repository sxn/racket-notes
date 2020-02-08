#lang racket/base

(require racket/contract
         web-server/servlet
         web-server/formlets
         "../core/models.rkt")

(provide/contract (start (-> request? response?)))

(define current-day 0)

(define (start request)
  (render-store-page (initialize-items!) request))

; render-store-page: items request -> doesn't return
; Produces an HTML page of the items available in the store.
(define (render-store-page the-items request)
  (define (response-generator embed/url)
    (response/xexpr
      `(html (head (title "Gilded Rose")
                   (link ((rel "stylesheet") (href "/styles.css") (type "text/css"))))
             (body 
              ,(render-masthead)
              ,(render-meta current-day)
              ,(render-items the-items)))))
             
  (send/suspend/dispatch response-generator))

; render-header: -> xexpr
(define (render-masthead)
  `(div ((class "masthead"))
        (h1 ((class "site-title")) "Gilded Rose")))

; render-meta: number -> xexpr
(define (render-meta the-day)
  `(div ((class "meta"))
        (span ((class "current-day")) 
              (span
                ,(string-append "It is now day: "
                               (number->string the-day))))))

; render-items: (listof item) -> xexpr
; Consumes a list of items, produces an xexpr fragment.
(define (render-items the-items)
  `(div ((class "items"))
        ,@(map render-item the-items)))

; render-item: item -> xexpr
; Consumes an item, produces an xexpr fragment of the item.
(define (render-item the-item)
  `(div ((class "item"))
    (h3 ,(item-name the-item))
    (p ,(number->string (item-quality the-item)))
    (p ,(number->string (item-sell-in the-item)))))

(require web-server/servlet-env)
(serve/servlet start
  #:launch-browser? #f
  #:quit? #f
  #:listen-ip #f
  #:port 8000
  #:extra-files-paths (list (build-path "web" "static")))
