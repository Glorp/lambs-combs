#lang racket
(provide subst)

(require "../structs.rkt")

(define (subst r)
  (match r
    [(redex p x a)
     
     (define (halp x)
       (match x
         [(ref s) (if (equal? s p)
                      a
                      (ref s))]
         [(lam lp b) (if (equal? lp p)
                         (lam lp b)
                         (lam lp (halp b)))]
         [(app f a) (app (halp f) (halp a))]
         [(other n x) (other n x)]
         [(other-fun n ari acc? f args) (other-fun n ari acc? f (map halp args))]))

     (halp x)]))