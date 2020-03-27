#lang racket/base

(require "parser.rkt")
(parse-to-datum "++++-+++-++-++[>++++-+++-++-++<-]>.")
