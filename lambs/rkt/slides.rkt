#lang at-exp racket
(provide (struct-out static-image)
         slides)

(struct static-image (path) #:transparent)

(define-syntax-rule (slds (name stuff ...) ...)
  (make-immutable-hash (list (cons 'name (list stuff ...)) ...)))
  
(define slides
  (slds (halp
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
         "renaming variables to avoid capture counts as one step:"
         "like,"
         "λx.(λy.λx.y) x"
         ""
         "anyway blah blah combinators. So can instead do:"
         "I x ≜ x"
         "and so on."
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
         ":slide hello")
        
        (hello
         "hello?"
         ""
         "(some notes to a self:"
         "border-length, textsize stuffs?"
         "did I switch out \"temp\" with sth?)")
        
        (lamb-rules
         "syntax:"
         "exp u ::= x         variable"
         "          λx.u      abstraction"
         "          u1 u2     application"
         ""
         "computation rule:"
         "(λx.u1) u2"
         "[u2/x]u1"
         ""
         "e.g."
         "(λx.x) foo"
         ""
         "(λa.λb.λc.a c c) foo bar quux")

        (churchnums
         "nums like:"
         "λf.λx.x"
         "λf.λx.f x"
         "λf.λx.f (f x)"
         "λf.λx.f (f (f (f (f x))))"
         ""
         "and like functions like successor"
         "λn.λf.λx.f (n f x)"
         "and plus"
         "λa.λb.λf.λx.a f (b f x)"
         "and stuff")

        (schon
         "  It is in the spirit of the axiomatic method as it has now received recognition,"
         "  chiefly through the work of Hilbert,"
         "  that we not only strive to keep the axioms as few and their content as limited as possible"
         "  but also attempt to make the number of fundamental undefined notions as small as we can;"
         "  we do this by seeking out those notions from which we shall best be able"
         "  to construct all other notions of the branch of science in question."
         ""
         "  Moses Schönhinkel, On the building blocks of mathematical logic")

        (combs
         "for like our purposes:"
         "a combinator is a function with no free variables"
         ""
         "e.g."
         "λx.x"
         "λa.λb.a"
         "λf.λx.f (f x)"
         ""
         "but not e.g."
         "λx.f x")
        
        (comb-rules
         "but also syntax:"
         "I x ≜ x"
         ""
         "And then e.g."
         "I foo")

        (ski
         "S x y z ≜ (x z) (y z)"
         "(or S ≜ λx.λy.λz.(x z) (y z))"
         ""
         "K x y ≜ x"
         "(or K ≜ λx.λy.x)"
         ""
         "I x ≜ x"
         "(or I ≜ λx.x)")

        (iks
         "I is the identity function"
         "we can use K to make constant functions"
         "S is like, stuff...")

        (try-to-ski
         "maybe doable:"
         "λx.x"
         "λf.λx.x"
         "λa.λb.λc.c"
         ""
         "maybe harder:"
         "λa.λb.a"
         "λa.λb.λc.c"
         "λf.λx.f x")

        (ski-rules
         "λx.x                    => I"
         "λx.u, if no free x in u => K u"
         "λx.u1 u2                => S (λx.u1) (λx.u2)")

        (free-bound
         "no free x in/all x-variables are bound"
         "λx.(foo x y z) (bar (λx.x) z y)"
         ""
         "there is a free x in the subexpression/the x is not bound"
         "(foo x y z)"
         ""
         "and no free x in subexpression/the x is bound"
         "(bar (λx.x) z y)"
         ""
         "like it's kind of like"
         "taking into account *this* much context"
         "this here x is free or is bound")

        (open-closed
         "(an expression with free variables is open."
         "an expression with no free variables is closed.)")

        (ski-eta-rules
         "λx.x                      => I"
         "λx.u, if no free x in u   => K u"
         "λx.u x, if no free x in u => u"
         "λx.u1 u2                  => S (λx.u1) (λx.u2)")
        
        (bc
         "B x y z ≜ x (y z)"
         "C x y z ≜ (x z) y")

        (skibc-rules
         "λx.x                         => I"
         "λx.u, if no free x in u      => K u"
         "λx.u x, if no free x in u    => u"
         "λx.u1 u2,"
         "          if no free x in u1 => B u1 (λx.u2)"
         "          if no free x in u2 => C (λx.u1) u2"
         "          else               => S (λx.u1) (λx.u2)")

        (hask
         "foo x y = (x + y) / 2")

        (link? "code: https://github.com/Glorp/lambs-combs")))
