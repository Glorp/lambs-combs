#lang racket
(provide parse-exp
         parse-top)

(require "../structs.rkt"
         "../opt.rkt"
         "str.rkt"
         "util.rkt")

(define (exp-list->exp exp-l)
  (left-assoc app exp-l))

(define (parse-top x)
  (define (def? x)
    (match x
      [(ref ':=) #t]
      [(ref '≜) #t]
      [_ #f]))

  (match (read-exp-list x)
    [#f #f]
    [l
     (define-values (first last) (splitf-at l (compose not def?)))
     (match* (first last)
       [(exp-l '()) (exp-list->exp exp-l)]
       [((list (ref id)) (list _)) (undef id)]       
       [((list (ref id)) (list _ exp-l ...)) (def id (exp-list->exp exp-l))]
       [((list (ref id) (ref params) ...) (list _ exp-l ...)) (defcomb id params (exp-list->exp exp-l))]
       [(_ _) #f])]))

(define (parse-exp x)
  (define res
    (opt>
     (read-exp-list x)
     [exp-l
      (exp-list->exp exp-l)]))
  (and (exp? res) res))

(define (read-exp-list y)
  (define ((add-exp x) rest-str)
    (opt> (read-exp-list rest-str)
          [rest (cons x rest)]))
  
  (define (halp y)
    (define x (skip-whites y))
    (cond [(str-empty? x) '()]
          
          [(equal? (str-current x) #\()
           (opt> ((split #\)) (next x))
                 [(cons inside rest)
                  (opts>
                   ((parse-exp inside) (halp rest))
                   [(a d) (cons a d)])])]
          
          [(equal? (str-current x) #\λ)
           (opt>
            (read-lambda x)
            [l (list l)])]

          [(equal? (str-current x) #\\)
           (opt>
            (read-lambda x)
            [l (list l)])]
             
          [else
           (match (read-word x)
             [#f #f]
             [(cons w rest)
              (opt>
               (halp rest)
               [rest-list (cons (ref (string->symbol w)) rest-list)])])]))
  (match (halp y)
    ['() #f]
    [x x]))

(define (read-lambda x)
  (opt>
   (or ((expect-string "λ") x) ((expect-string "\\") x))
   [lam-rest
    (opt> (read-word lam-rest)
    [(cons param param-rest)
     (opt>
      ((expect-string ".") param-rest)
      [body
       (opt>
        (parse-exp body)
        [l (lam (string->symbol param) l)])])])]))
