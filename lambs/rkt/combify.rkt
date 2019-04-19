#lang racket
(provide combify
         combify-ski
         combify-ski-eta)

(require "structs.rkt"
         "free-ids.rkt")

(define (not-in s x)
  (not (set-member? (free-ids x) s)))

(define (combify-halp rule t)
  (define (combify-inner t)
    (or (match t
          [(lam p x)
           (cond [(combify-inner x) => (λ (res) (lam p res))]
                 [else #f])]
          [(app f a)
           (cond [(combify-inner f) => (λ (res) (app res a))]
                 [(combify-inner a) => (λ (res) (app f res))]
                 [else #f])]
          [(ref x) #f])
        (rule t)))
  (define res (combify-inner t))
  (if res
      (combify-halp rule res)
      t))

(define (combify-ski t)
  (combify-halp
   (match-lambda
     [(lam p b)
      #:when (not-in p b)
      (app (ref 'K) b)]
     [(lam p (ref x))
      #:when (equal? p x)
      (ref 'I)]
     [(lam p (app f a))(app (app (ref 'S) (lam p f)) (lam p a))]
     [_ #f])
   t))

(define (combify-ski-eta t)
  (combify-halp
   (match-lambda
     [(lam p b)
      #:when (not-in p b)
      (app (ref 'K) b)]
     [(lam p (ref x))
      #:when (equal? p x)
      (ref 'I)]
     [(lam p (app f (ref x)))
      #:when (and (equal? p x) (not-in p f))
      f]
     [(lam p (app f a))(app (app (ref 'S) (lam p f)) (lam p a))]
     [_ #f])
   t))

(define (combify t)
  (combify-halp
   (match-lambda
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
     [_ #f])
   t))
