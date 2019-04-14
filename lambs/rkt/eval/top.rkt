#lang racket
(require "eval.rkt")
(require "../structs.rkt")

(define (update-defs defs x)
  (match x
    [(def id exp) (add-def defs id exp)]
    [(undef id) (remove-def defs id)]
    [_ defs]))

(define (add-def defs id exp)
  (match defs
    [(list (def cid cexp) xs ...)
     (if (equal? id cid)
         (cons (def id exp) xs)
         (cons (def cid cexp) (add-def xs id exp)))]
    ['() (list (def id exp))]))

(define (remove-def defs id)
  (match defs
    [(list (def cid cexp) xs ...)
     (if (equal? id cid)
         xs
         (cons (def cid cexp) (remove-def xs id)))]
    ['() '()]))