#lang racket
(provide (struct-out conflict)
         find-conflict
         resolve-conflict
         unique-id)

(require "../structs.rkt"
         "expath.rkt"
         "subst.rkt")

(struct conflict (path param body) #:transparent)

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

(define (find-conflict r)
  (match r
    [(redex param body arg)
     (define free-in-arg (free-ids arg))
     (let halp ([path '()] [possible-conflict #f] [x body])
       (match x
         [(ref s) (and (equal? s param) possible-conflict)]
         [(other _ _) #f]
         
         [(lam p b)
          (define new-path (cons (lamstep p) path))
          (cond [(equal? p param) #f]

                [possible-conflict
                 (halp new-path possible-conflict b)]
                
                [(set-member? free-in-arg p)
                 (halp new-path (conflict path p b) b)]
                
                [else
                 (halp new-path #f b)])]

         [(other-fun n ari acc? f args)
          (let other-halp ([before '()] [after args])
            (match after
              ['() #f]
              [(list x xs ...)
               (or (halp (cons (otherstep n ari acc? f before xs) path) possible-conflict x)
                   (other-halp (cons x before) xs))]))]
          

         [(app f a)
          (or (halp (cons (funstep a) path) possible-conflict f)
              (halp (cons (argstep f) path) possible-conflict a))]))]))

(define (all-ids x)
  (match x
    [(ref s) (set s)]
    [(lam p b) (set-add (all-ids b) p)]
    [(app f a) (set-union (all-ids f) (all-ids a))]
    [(other-fun n _ _ _ a) (set-add (apply set-union (map all-ids a)) n)]
    [(other n _) (set n)]))

(define (unique-id s x)
  (define all (all-ids x))

  (define (id/num s)
    (match (regexp-match #rx"^(.*?)([0-9]*)$" (symbol->string s))
      [(list _ name "") (cons name 2)]
      [(list _ name num) (cons name (+ (string->number num) 1))]))
  
  (match (id/num s)
    [(cons name num)
     (define (halp n)
       (define new-id (string->symbol (~a name n)))
       (if (set-member? all new-id)
           (halp (+ n 1))
           new-id))
     
     (halp num)]))

(define (resolve-conflict c new-id)
  (match c
    [(conflict cpath cp cb)
     (expath->exp cpath
                  (lam new-id
                       (subst (redex cp cb (ref new-id)))))]))
