#lang racket/base

(require db
         racket/path)

(define this-folder
  (simplify-path (build-path (syntax-source #'this-folder) "..")))

(define the-connection
  (virtual-connection
    (connection-pool
      #:max-connections 10
      #:max-idle-connections 5
      (lambda ()
        (displayln "connecting..")
        (sqlite3-connect
          #:mode 'create
          #:database (build-path this-folder "db.sqlite3"))))))

(query-value the-connection "select 1")

(sync/enable-break never-evt)
