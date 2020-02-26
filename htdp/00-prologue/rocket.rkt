#lang racket

(require 2htdp/universe 2htdp/image)

; properties of the "world" and the descending rocket
(define WIDTH  800)
(define HEIGHT  600)
(define V 3)
(define X (/ WIDTH 2))
 
; graphical constants 
(define EMPTY-SCREEN  (empty-scene WIDTH HEIGHT))
(define ROCKET (bitmap "graphics/rocket.png"))
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))
 
; functions
(define (picture-of-rocket t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET X (distance t) EMPTY-SCREEN)]
    [(> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET X ROCKET-CENTER-TO-TOP EMPTY-SCREEN)]))
 
(define (distance t)
  (* V t))

(animate picture-of-rocket)
