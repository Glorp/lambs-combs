#lang racket
(provide free-ids)

(require "structs.rkt")

(define (free-ids t [bound (set)])
  (match t
    [(ref s) (if (set-member? bound s)
                 (set)
                 (set s))]
    [(lam p b) (free-ids b (set-add bound p))]
    [(app f a) (set-union (free-ids f bound) (free-ids a bound))]
    [(other _ _) (set)]
    [(other-fun _ _ _ _ '()) (set)]
    [(other-fun _ _ _ _ args) (apply set-union (map (λ (x) (free-ids x bound)) args))]))