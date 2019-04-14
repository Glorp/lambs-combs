#lang racket
(provide find-exp
         expath->exp
         (struct-out found)
         (struct-out lamstep)
         (struct-out funstep)
         (struct-out argstep)
         (struct-out otherstep))

(require "../structs.rkt")

(struct lamstep (param) #:transparent)
(struct funstep (arg) #:transparent)
(struct argstep (fun) #:transparent)
(struct otherstep (name arity acc? function before after) #:transparent)

(struct found (path thing) #:transparent)

(define (find-exp pred exp)
  (let halp ([path '()] [x exp])
    (define current? (pred x))
    (cond [current? (found path current?)]
          [else
           (match x
             [(lam p b)
              (halp (cons (lamstep p) path) b)]
           
             [(app f a)
              (or (halp (cons (funstep a) path) f)
                  (halp (cons (argstep f) path) a))]

             [(other-fun n ari acc? f args)
              (let other-halp ([before '()] [after args])
                (match after
                  ['() #f]
                  [(list x xs ...)
                   (or (halp (cons (otherstep n ari acc? f before xs) path) x)
                       (other-halp (cons x before) xs))]))]

             [_ #f])]))) 

(define (expath->exp path exp)
  (define (step->exp step exp)
    (match step
      [(lamstep p) (lam p exp)]
      [(funstep a) (app exp a)]
      [(argstep f) (app f exp)]
      [(otherstep n ari acc? f before after)
       (other-fun n ari acc? f (append (reverse before) (list exp) after))]))
  
  (foldl step->exp exp path))

(module+ test
  (require rackunit
           "../parse.rkt"
           "../unparse.rkt")
  
  (define f
    (find-exp reducible? (parse "λf.λx.(λx.x) a b")))
  
  (define res
    (match f
      [(found p _) (unparse (expath->exp p (ref 'foo)))]))
    
  (check-equal? res "λf.λx.foo b"))

