#lang racket
(require "structs.rkt"
         "unparse.rkt"
         "eval/eval.rkt"
         "eval/define.rkt")

(provide run1
         run
         rename-defs)

(define (write-line s)
  (write s)
  (newline)
  (flush-output))

(define (run1 top ds)
  (match-define (defs dfs others) ds)
  (define x (subst-defs others top))
  (define new-defs (update-defs ds x))
  (define res-s
    (match x
      [#f ":("]
      [(def id _) (format "~a is defined :)" id)]
      [(defcomb id _ _) (format "~a is defined :)" id)]
      [(undef id) (format "~a is undefined :(" id)]
      [_ (exec-str (step x))]))
  (write-line res-s)
  new-defs)

(define (run n start-exp ds)
  (match-define (defs dfs others) ds)
  (let halp ([n n] [exec (step (subst-defs others start-exp))])
    (write-line (exec-str exec))
    (define next-x (exec-next exec))
    (define next-exec (and next-x (step next-x)))
    (define next-next-x (and next-exec (exec-next next-exec)))
    (when (and (> n 0) next-next-x)
      (halp (- n 1) next-exec))))

(define (rename-defs exp ds)
  (match-define (defs dfs others) ds)
  (define res-s (unparse (subst-defs others (subst-defs dfs exp))))
  (write-line res-s))

(define (exec-next exec)
  (match exec
    [(normal x) #f]
    [(reduce _ x) x]
    [(rename _ _ x _) x]))

(define (exec-str exec)
  (match exec
    [(normal x) (unparse x)]
    [(reduce _ x) (unparse x)]
    [(rename _ old x new) (format "~a | [~a/~a]" (unparse x) new old)]))
