#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     syntax/parse
                     syntax/parse/class/paren-shape
                     ))

@title{Paren-shape Pattern Expanders}

@defmodule[paren-shape/pattern-expander]{

@deprecated[#:what "library"
            @elem{the pattern expanders from
                  @racketmodname[syntax/parse/class/paren-shape]}]{	 	 	 	 
Specifically, this reprovides @racket[~parens], @racket[~brackets],
and @racket[~braces], three pattern expanders that match syntax
objects only when then @racket['paren-shape] property matches
the expected value.}

@examples[
  #:eval (make-base-eval)
  (require syntax/parse syntax/parse/class/paren-shape)
  (eval:alts
   (syntax-parse #'(1 2 3)
     [(~parens a b c) #'a])
   (syntax-parse (read-syntax 'eval (open-input-string "(1 2 3)"))
     [(~parens a b c) #'a]))
  (eval:alts
   (syntax-parse #'[1 2 3]
     [(~parens a b c) #'a])
   (eval:error
    (syntax-parse (read-syntax 'eval (open-input-string "[1 2 3]"))
      [(~parens a b c) #'a])))
  (eval:alts
   (syntax-parse #'[1 2 3]
     [(~brackets a b c) #'a])
   (syntax-parse (read-syntax 'eval (open-input-string "[1 2 3]"))
     [(~brackets a b c) #'a]))
  (eval:alts
   (syntax-parse #'(1 2 3)
     [(~brackets a b c) #'a])
   (eval:error
    (syntax-parse (read-syntax 'eval (open-input-string "(1 2 3)"))
      [(~brackets a b c) #'a])))
  (eval:alts
   (syntax-parse #'{1 2 3}
     [(~braces a b c) #'a])
   (syntax-parse (read-syntax 'eval (open-input-string "{1 2 3}"))
     [(~braces a b c) #'a]))
  (eval:alts
   (syntax-parse #'(1 2 3)
     [(~braces a b c) #'a])
   (eval:error
    (syntax-parse (read-syntax 'eval (open-input-string "(1 2 3)"))
      [(~braces a b c) #'a])))
]}


