#lang racket
(provide combify)

(require "structs.rkt"
         "free-ids.rkt")

(define (not-in s x)
  (not (set-member? (free-ids x) s)))

(define (combify-halp t)
  (match t
    [(lam p b)
     #:when (not-in p b)
     (app (ref 'K) b)]
    [(lam p (ref x))
     #:when (equal? p x)
     (ref 'I)]
    [(lam p (app f (ref x)))
     #:when (and (equal? p x) (not-in p f))
     f]
    [(lam p (app f a))
     (cond [(not-in p f) (app (app (ref 'B) f) (lam p a))]
           [(not-in p a) (app (app (ref 'C) (lam p f)) a)]
           [else (app (app (ref 'S) (lam p f)) (lam p a))])]
    [_ #f]))

(define (combify-outer t)
  (or (combify-halp t)
      (match t
        [(lam p x)
         (cond [(combify-outer x) => (λ (res) (lam p res))]
               [else #f])]
        [(app f a)
         (cond [(combify-outer f) => (λ (res) (app res a))]
               [(combify-outer a) => (λ (res) (app f res))]
               [else #f])]
        [(ref x) #f])))

(define (combify t)
  (cond [(combify-outer t) => combify]
                    [else t]))
  