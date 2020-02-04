#lang web-server/insta

(struct post (title body))

(define BLOG (list (post "First Post!" "This is my first post, la dee da")
                   (post "Second post!" "This is my second post?!")))

(define (start request)
  (render-blog-page BLOG request))


(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "My Blog"))
          (body (h1 "My Blog")
                ,(render-posts a-blog)))))
 
(define (render-post a-post)
  `(div ((class "post"))
    (h1 ,(post-title a-post))
    (p ,(post-body a-post))))

 
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))
