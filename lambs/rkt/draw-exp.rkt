#lang racket
(provide draw-exp)

(require "draw/draw.rkt"
         "draw/tree-structs.rkt"
         "structs.rkt"
         "unparse.rkt"
         2htdp/image)

(define (exp->tree x)
  (match x
    [(ref s) (tr x)]
    [(lam p b) (tr x (exp->tree b))]
    [(app f a) (tr x (exp->tree f) (exp->tree a))]))

(define (exp->img x)
  (define exp-str (unparse x))
  (define top-str
    (match x
      [(ref s) "(reference)"]
      [(lam p b) "(abstraction)"]
      [(app f a) "(application)"]))
  (above (draw-text top-str)
         (rectangle 0 3 'solid (color 0 0 0 0))
         (draw-text exp-str)))

(define draw-exp
  (compose draw
           (tree-map exp->img)
           exp->tree))

(module+ main
  (require "parse.rkt")
  (draw-exp (parse "(λn.λf.λx.f (n f x)) (λf.λx.f (f x))")))