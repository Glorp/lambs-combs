#lang racket
(provide step
         (struct-out reduce)
         (struct-out rename)
         (struct-out normal)
         (struct-out skip))

(require "../structs.rkt"
         "subst.rkt"
         "expath.rkt"
         "conflict.rkt")

(struct reduce (from to) #:transparent)
(struct skip (from to) #:transparent)
(struct rename (from-exp from-name to-exp to-name) #:transparent)
(struct normal (exp) #:transparent)

(define (step exp)
  (define res
    (match (find-exp reducible? exp)
      
      [#f (normal exp)]
      
      [(found rpath (other-redex (other-fun n ari acc? f args) a))
       (define new-args (append args (list a)))
       (if (= (length new-args) ari)
           (reduce exp (expath->exp rpath (f new-args)))
           (skip exp (expath->exp rpath (other-fun n ari acc? f new-args))))]
      
      [(found rpath (redex rp rb ra))
     
       (match (find-conflict (redex rp rb ra))
       
         [#f
          (reduce exp
                  (expath->exp rpath (subst (redex rp rb ra))))]
       
         [(conflict cpath cp cb)
          (define new-name (unique-id cp exp))
          (define resolved (expath->exp cpath
                                        (lam new-name
                                             (subst (redex cp cb (ref new-name))))))
          (define res (expath->exp rpath (app (lam rp resolved) ra)))
          (rename exp cp res new-name)])]))
  (match res
    [(skip _ x) (step x)]
    [x x]))

(define (exec-exp exec)
  (match exec
    [(normal x) x]
    [(reduce _ x) x]
    [(rename _ _ x _) x]))

(module+ test
  (require "../parse.rkt"
           "../unparse.rkt"
           rackunit)
  (define (halp s)
    (unparse (exec-exp (step (parse s)))))

  (define (halpn n s)
    (for/fold ([x s])
              ([i (in-range n)])
      (halp x)))
  
  (check-equal? (halp "(λx.x) foo")
                "foo")

  (define plus "λa.λb.λf.λx.a f (b f x)")
  (define three "λf.λx.f (f (f x))")
  (define two "λf.λx.f (f x)")
  (define three-plus-two (format "(~a) (~a) (~a)" plus three two))
  (check-equal? (halpn 6 three-plus-two)
                "λf.λx.f (f (f (f (f x))))")
  
  (check-equal? (halp "λa.(λx.λy.x y y2) y foo")
                "λa.(λx.λy3.x y3 y2) y foo")

  (check-match (step (parse "λa.(λx.λy.x y y2) y foo"))
               (rename _ 'y _ 'y3)))
