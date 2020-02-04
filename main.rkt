#lang web-server/insta

(struct post (title body))

(define BLOG (list (post "First Post!" "This is my first post, la dee da")
                   (post "Second post!" "This is my second post?!")))

(define (start request)
  (define a-blog
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request))
                 BLOG)]
          [else
           BLOG]))
  (render-blog-page a-blog request))


(define (render-blog-page a-blog request)
  (display "request start")
  (display request)
  (display "request end")
  (response/xexpr
   `(html (head (title "My Blog"))
          (body (h1 "My Blog")
                ,(render-posts a-blog)
                (form
                 (input ((name "title")))
                 (input ((name "body")))
                 (input ((type "submit"))))))))
 
(define (render-post a-post)
  `(div ((class "post"))
        (h1 ,(post-title a-post))
        (p ,(post-body a-post))))

 
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))


(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))