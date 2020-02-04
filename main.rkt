#lang web-server/insta

(define (start request)
  (response/xexpr
   '(html
     (head (title "HELLO"))
     (body (h1 "Under construction")))))


(struct post (title body))

(define BLOG (list (post "First Post!" "This is my first post, la dee da")
                   (post "Second post!" "This is my second post?!")))

(define (start request)
  (render-blog-page BLOG request))

(define (render-post a-post)
  `(div ((class "post"))
    (h1 ,(post-title a-post))
    (p ,(post-body a-post))))

 