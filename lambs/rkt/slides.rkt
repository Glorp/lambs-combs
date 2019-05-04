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
         "C-S-e: evaluate until normal form, or at least 1000 steps"
         "C-r: replace names of things defined with their definitions"
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

        (lambda-churchnums
         "nums like:"
         "l0 ≜ λf.λx.x"
         "l1 ≜ λf.λx.f x"
         "l2 ≜ λf.λx.f (f x)"
         "l3 ≜ λf.λx.f (f (f x))"
         "l5 ≜ λf.λx.f (f (f (f (f x))))"
         ""
         "and like functions like successor"
         "lsuc ≜ λn.λf.λx.f (n f x)"
         "and plus"
         "l+ ≜ λa.λb.λf.λx.a f (b f x)"
         "and stuff")

        (schon
         "  It is in the spirit of the axiomatic method as it has now"
         "  received recognition, chiefly through the work of Hilbert,"
         "  that we not only strive to keep the axioms as few and"
         "  their content as limited as possible but also attempt to"
         "  make the number of fundamental undefined notions as small"
         "  as we can; we do this by seeking out those notions from"
         "  which we shall best be able to construct all other notions"
         "  of the branch of science in question."
         ""
         "  On the building blocks of mathematical logic"
         "  Moses Schönhinkel")

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
         "BLAH x y z ≜ BLAH z y x"
         ""
         "and then e.g."
         "I foo"
         "BLAH 1 2 3"
         "BLAH 1 2")

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
         "I x ≜ x"
         "I is the identity function"         
         ""
         "K x y ≜ x"
         "we can use K to make constant functions"
         "e.g. (K 1) is the function that always returns 1"
         ""
         "S x y z ≜ (x z) (y z)"
         "S is like, oof, stuff...")

        (try-to-ski
         "maybe doable:"
         "λx.x"
         "λf.λx.x"
         "λa.λb.λc.c"
         ""
         "maybe harder:"
         "λf.λx.f x"
         "λa.λb.λc.b")

        (tried-to-ski
         "translate by gradually rewriting and testing with dummy args"
         ""
         "\"abstraction elimination\""
         ""
         "replace a lambda abstraction with a combinator that's been"
         "given all but one of its arguments"
         ""
         "I and K not so bad"
         "λx.x                    => I"
         "λx.u, if no free x in u => K u")
        
        (s
         "S x y z ≜ (x z) (y z)"
         ""
         "in translating we pass inn two arguments (all but one)"
         "so the z will kind of replace the original lambda-parameter"
         ""
         "function applications consists of two things"
         "function-part-thing and argument-part-thing"
         ""
         "the z will then be passed into both so _those_ need to be"
         "made into functions accepthing the original lambda parameter"
         ""
         "so:"         
         "λf.λx.f x"
         "λa.λb.λc.b")
        
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

        (eta
         "λx.f x is the same as f as long as there is no free x in x")
        
        (ski-eta-rules
         "λx.x                      => I"
         "λx.u, if no free x in u   => K u"
         "λx.u x, if no free x in u => u"
         "λx.u1 u2                  => S (λx.u1) (λx.u2)")
        
        (many-ks
         "while translating we often get stuff like"
         "λf.λx.K (f x)"
         "which we then turn into"
         "λf.S (K K) f"
         ""
         "and the K K bit seems maybe silly"
         "like there was nothing really important going on in there?"
         ""
         "would be nice if we could just like _not_ throw additional"
         "Ks onto uninteresting stuff")
        
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

        (fewer
         "we could do like S K K instead of I"
         "there's some stuff about")

        (hask
         "foo x y = (x + y) / 2")

        (link? "code: https://github.com/Glorp/lambs-combs")))
