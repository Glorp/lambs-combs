#lang racket
(provide parse)
         

(require "parse/parse.rkt"
         "parse/str.rkt")

(define (remove-comment s)
  (match (regexp-match #rx"^(.*?)\\|.*$" s)
    [#f s]
    [(list _ new-s) new-s]))

(define parse (compose parse-top string->str remove-comment))

(module+ test
  (require rackunit
           "structs.rkt")
  
  (check-equal? (parse "asd (qwe (λx.x x)) erw")
                (app (app (ref 'asd) (app (ref 'qwe) (lam 'x (app (ref 'x) (ref 'x))))) (ref 'erw)))
  (check-equal? (parse "asd (qwe (λx.x x)) erw | asd qweqwe df")
                (app (app (ref 'asd) (app (ref 'qwe) (lam 'x (app (ref 'x) (ref 'x))))) (ref 'erw))))

