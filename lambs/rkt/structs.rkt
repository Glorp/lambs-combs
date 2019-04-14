#lang racket
(provide (struct-out ref)
         (struct-out lam)
         (struct-out app)
         (struct-out other)
         (struct-out other-fun)
         (struct-out def)
         (struct-out undef)
         (struct-out defcomb)
         (struct-out defs)
         exp?
         (struct-out redex)
         (struct-out other-redex)
         reducible?)

(struct lam (p x) #:transparent)
(struct app (f a) #:transparent)
(struct ref (s) #:transparent)
(struct other (name stuff) #:transparent)
(struct other-fun (name arity accept? function args) #:transparent)
(struct def (name exp) #:transparent)
(struct defs (defs others) #:transparent)

(struct defcomb (name params exp) #:transparent)

(struct undef (name) #:transparent)

(define (exp? x)
  (match x
    [(lam p x) (and (symbol? p) (exp? x))]
    [(app f a) (and (exp? f) (exp? a))]
    [(ref s) (symbol? s)]
    [(other _ _) #t]
    [(other-fun _ _ _ _ _) #t]
    [_ #f]))

(struct redex (param body arg) #:transparent)
(struct other-redex (other arg) #:transparent)

(define (reducible? x)
  (match x
    [(app (lam p x) a) (redex p x a)]
    [(app (other-fun n ari acc? f args) a)
     (and (acc? (length args) a)
          (other-redex (other-fun n ari acc? f args) a))]
    [_ #f]))