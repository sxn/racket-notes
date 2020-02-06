#lang racket/base
(require racket/list db)


; A blog is a (blog db)
; where home is a string, posts is a (listof post)
(struct blog (db))

; and post is a (post title body comments)
; where title is a string, body is a string,
; and comments is a (listof string)
(struct post (blog id))
 
; initialize-blog! : path? -> blog
; Reads a blog from a path, if not present, returns default
(define (initialize-blog! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
      (string-append
        "create table posts "
        "(id integer primary key, title text, body text)"))
      (blog-insert-post! the-blog "First Post!" "this is my first post!")
      (blog-insert-post! the-blog "Second Post!" "this is my second post!"))
  (unless (table-exists? db "comments")
    (query-exec db
        "create table comments (pid integer, content text)")
    (post-insert-comment! the-blog (first (blog-posts the-blog)) "First!!!11")))

; blog-posts : blog -> (listof post?)
; Queries for the post ids
(define (blog-posts a-blog)
  (define (id->post an-id)
    (post a-blog an-id))
  (map id->post
    (query-list
      (blog-db a-blog)
      "select id from posts")))

; post-title : post -> string?
; Queries the DB for the title of a post
(define (post-title a-post)
  (query-value
    (blog-db (post-blog a-post))
    "select title from posts where id = ?"
    (post-id a-post)))

; post-body : post -> string?
; Queries the DB for the body of a post
(define (post-body a-post)
  (query-value
    (blog-db (post-blog a-post))
    "select body from posts where id = ?"
    (post-id a-post)))

; post-comments : post -> (listof string?)
; Queries the DB for all of a post's comments
(define (post-comments a-post)
  (query-list
    (blog-db (post-blog a-post))
    "select content from comments where pid = ?"
    (post-id a-post)))

; blog-insert-post!: blog? string? string? -> void
; Consumes a blog and a two strings, creates a post from the two strings and adds it to the top
; of the blog.
(define (blog-insert-post! a-blog title body)
  (query-exec
    (blog-db a-blog)
    "insert into posts (title, body) values (?, ?)"
    title body))
 
; post-insert-comment!: blog? post? string? -> void
; Consumes a blog, a post and a comment string. As a side-efect, 
; adds the comment to the bottom of the post's list of comments.
(define (post-insert-comment! a-blog a-post a-comment)
  (query-exec
    (blog-db a-blog)
    "insert into comments (pid, content) values (?, ?)"
    (post-id a-post) a-comment))

(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)
