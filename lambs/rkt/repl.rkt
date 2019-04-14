#lang racket
(provide repl)

(require "run.rkt"
         "parse.rkt"
         "unparse.rkt"
         "combify.rkt"
         "structs.rkt"
         "draw-exp.rkt"
         "slides.rkt"
         (only-in 2htdp/image scale image? square))

(define (border n)
  (string-join (build-list n (λ (x) "=^..^="))
               "   "
               #:before-first " "
               #:after-last " "))


(define ((write-stuff img scal) x)
  (write 
   (match x
     [(? image?) (img (scale scal x))]
     [(static-image path) `(img ,path)]
     [(? string?) x]
     [_ (~a x)])))

(define (command/rest s)
  (match (regexp-match #rx"^[\t\r ]*(.*?)[\t\r ]+(.*)$" s)
    [(list _ "" c) (cons c "")]
    [(list _ c r) (cons c r)]
    [#f (cons s "")]))

(struct config (scale border-length) #:transparent)

(define (repl img [scal 3/2])
  ((write-stuff img scal) "Beep boop.")
  (let loop ([it ":)"] [conf (config scal 8)] [ds (defs '() '())])
    (match-define (config scal blen) conf)
    (define write (write-stuff img scal))
    
    (flush-output)
    (define s (read))
    (newline)
    
    (with-handlers
        ([(λ (_) #t)
          (λ (e)
            (write (exn-message e))
            (loop it conf ds))])
      
           
      (match (command/rest s)
        {(cons "" "")
         (write "beep boop")
         (loop it conf ds)}
        
        [(cons ":scale" "")
         (write (format ":scale ~a" scal))
         (loop it conf ds)]
        
        [(cons ":scale" new)
         (define new-scal (string->number new))
         ((write-stuff img new-scal) (draw-exp (lam 'x (ref 'x))))
         (loop it (config new-scal blen) ds)]

        [(cons ":border-length" "")
         (write (format ":border-length ~a" blen))
         (loop it conf ds)]
        
        [(cons ":border-length" new)
         (define new-blen (string->number new))
         (write (border new-blen))
         (loop it (config scal new-blen) ds)]

        [(cons ":it" "")
         (write it)
         (loop it conf ds)]

        [(cons ":defs" "")
         (write (format "~a" ds))
         (loop it conf ds)]

        [(cons ":slide" x)
         (write "")
         (write (border blen))
         (write "")
         (for ([y (hash-ref slides (string->symbol x))])
           (write y))
         (write "")
         (write (border blen))
         (write "")
         (loop it conf ds)]
               
        [(cons ":q" "")
         (write "byebyes")
         (void)]

        [(cons ":run1" top-s)
         (define top (parse top-s))
         (define new-ds
           (cond [top (run1 top ds)]
                 [else (write ":(")
                       ds]))
         (loop s conf new-ds)]

        [(cons ":run1000" exp-s)
         (define exp (parse exp-s))
         (if (exp? exp)
             (run 1000 exp ds)
             (write ":("))
         (loop exp-s conf ds)]

        [(cons ":combify" exp-s)
         (define exp (parse exp-s))
         (if (exp? exp)
             (write (unparse (combify exp)))
             (write ":("))
         (loop exp-s conf ds)]
        
        [(cons ":rename" exp-s)
         (define exp (parse exp-s))
         (if (exp? exp)
             (rename-defs exp ds)
             (write ":("))
         (loop exp-s conf ds)]

        [(cons ":draw" exp-s)
         (define exp (parse exp-s))
         (write (if (exp? exp)
                    (draw-exp exp)
                    ":("))
         (loop exp-s conf ds)]))))

