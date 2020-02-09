#lang racket/base

(require racket/contract
         web-server/servlet-env
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
              ,(render-masthead embed/url the-items)
              ,(render-items the-items)))))
             
  (send/suspend/dispatch response-generator))

; render-header: -> xexpr
(define (render-masthead embed/url the-items)
  `(div ((class "masthead"))
        (h1 ((class "site-title")) "Gilded Rose")
            ,(render-meta embed/url current-day the-items)))

; render-meta: number -> xexpr
(define (render-meta embed/url the-day the-items)
  (define (next-day-handler request)
    (set! current-day (add1 current-day))
    (render-store-page (update-quality the-items) request))

  `(div ((class "meta"))
          (p ((class "current-day")) 
                (span
                  ,(string-append
                    "It's day "
                    (number->string the-day))))
          (a ((href ,(embed/url next-day-handler))) "next")))

; render-items: (listof item) -> xexpr
; Consumes a list of items, produces an xexpr fragment.
(define (render-items the-items)
  `(section ((class "store"))
    (div ((class "table"))
      (span ((class "cell")) "Name")
      (span ((class "cell")) "Quality")
      (span ((class "cell")) "Sell in")
      ,@(apply append (map render-item the-items)))))

; render-item: item -> (listof xexpr)
; Consumes an item, produces an xexpr fragment of the item.
(define (render-item the-item)
  `((span ((class "item-name cell")) ,(item-name the-item))
    (span ((class "cell")) ,(number->string (item-quality the-item)))
    (span ((class "cell")) ,(number->string (item-sell-in the-item)))))

(serve/servlet start
  #:launch-browser? #t
  #:quit? #f
  #:listen-ip #f
  #:port 8000
  #:extra-files-paths (list (build-path "web" "static")))
