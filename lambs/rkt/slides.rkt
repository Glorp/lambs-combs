#lang at-exp racket
(provide (struct-out static-image)
         slides)

(struct static-image (path) #:transparent)

(define halp (list
              "C-d: ≜"
              "C-l: λ"
              ""
              "C-e: add definition or do one step of evaluation"
              ""
              "like, cursor somewhere on next line and do C-e:"
              "I ≜ λx.x"
              ""
              "C-S-e: evaluate until normal form, or like at least 1000 steps"
              "C-r: replace names of things you have defined with their definitions"
              ""
              "like, cursor on next line, C-r, then C-e or C-S-e:"
              "I I I I foo"
              ""
              "Renaming variables to avoid capture counts as one step:"
              "like,"
              "λx.(λy.λx.y) x"
              ""
              "Anyway blah blah combinators. So can instead do:"
              "I x ≜ x"
              "And so on."
              ""
              "..."
              ""
              "<C-return>: special command thing (lines that begin with :)"
              ""
              "some examples..."
              ":draw (λf.λx.f x) foo bar"
              ":scale"
              ":scale 2"
              ":draw (λx.x) foo"
              ":border-length"
              ":border-length 5"
              ":slide hello"))


(define hello (list "Hello?"
                    ""
                    "(some notes to a self:"
                    "border-length, textsize stuffs?"
                    "did I switch out \"temp\" with sth?)"))


(define lamb-rules (list
                    "Syntax:"
                    "Exp u ::= x         variable"
                    "          λx.u      abstraction"
                    "          u1 u2     application"
                    ""
                    "Computation rule:"
                    "(λx.u1) u2"
                    "[u2/x]u1"
                    ""
                    "e.g."
                    "(λx.x) foo"
                    ""
                    "(λa.λb.λc.a c c) foo bar quux"))

(define comb-rules (list
                    "But also syntax:"
                    "I x ≜ x"
                    ""
                    "And then e.g."
                    "I foo"))

(define churchnums (list "nums like:"
                         "zero ≜ λf.λx.x"
                         "one ≜ λf.λx.f x"
                         "two ≜ λf.λx.f (f x)"
                         "five ≜ λf.λx.f (f (f (f (f x))))"
                         "plus ≜ λa.λb.λf.λx.a f (b f x)"))


(define links? (list "code: https://github.com/Glorp/lambs-combs"))

(define slides
  `#hash((hello . ,hello)
         (halp . ,halp)
         (churchnums . ,churchnums)
         (links? . ,links?)))


