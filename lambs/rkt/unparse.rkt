#lang racket
(provide unparse)
(require "structs.rkt")

(define (paren s)
  (~a "(" s ")"))

(define (unother n [args '()])
  (foldr (λ (a b) (app b a)) (ref n) (reverse args)))

(define (unparse x)
  (match x
    [(lam p x) (format "λ~a.~a" p (unparse x))]
    [(app (lam p b) a) (format "~a ~a" (paren (unparse (lam p b))) (argstring a))]
    [(app f a) (format "~a ~a" (unparse f) (argstring a))]
    [(ref s) (symbol->string s)]
    [(other n _) (unparse (unother n))]
    [(other-fun n _ _ _ args) (unparse (unother n args))]))

(define (argstring x)
  (match x
    [(ref s) (symbol->string s)]
    [(other n _) (argstring (unother n))]
    [(other-fun n _ _ _ args) (argstring (unother n args))]
    [x (paren (unparse x))]))

