#lang racket
(require "subst.rkt"
         "../structs.rkt")

(provide update-defs
         subst-defs
         make-comb)

(struct comb-name (name) #:transparent)

(define (make-comb name params exp)
  (define comb-exp
    (for/fold ([x exp])
              ([id params])
      (subst (redex id x (ref (comb-name id))))))
  (other-fun name
             (length params)
             (λ (n x) #t)
             (λ (args)
               (for/fold ([x comb-exp])
                         ([id params] [arg args])
                 (subst (redex (comb-name id) x arg))))
             '()))

(define (update-defs ds x)
  (match ds
    [(defs dfs others)
     (match x
       [(def id exp) (defs (add-def dfs id exp) (remove-def others id))]
       [(defcomb id ps x) (defs (remove-def dfs id) (add-def others id (make-comb id ps x)))]
       [(undef id) (defs (remove-def dfs id) (remove-def others id))]
       [_ (defs dfs others)])]))

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


(define (subst-defs defs exp)
  (define (halp x)
    (define (subst-def d x)
      (match d
        [(def id a) (subst (redex id x a))]))
    (foldr subst-def x defs))
  
  (match exp
    [(def i x) (def i (halp x))]
    [(defcomb i ps x) (defcomb i ps (halp x))]
    [(undef i) (undef i)]
    [_ (halp exp)]))

