#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     syntax/parse
                     paren-shape/pattern-expander
                     ))

@title{Paren-shape Pattern Expanders}

@defmodule[paren-shape/pattern-expander]{
This module provides syntax-parse pattern expanders for
@racket[~parens], @racket[~brackets], and @racket[~braces]. These only
match syntax objects when then @racket['paren-shape] property matches
the expected value.
}

@defform[(~parens . pattern)]{
This pattern is equivalent to @racket[pattern], except that it
only matches syntax objects that were written with @racket[()] round
parentheses, and not square brackets or curly braces.

@examples[
  #:eval (make-base-eval)
  (require syntax/parse paren-shape/pattern-expander)
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
]}

@defform[(~brackets . pattern)]{
Equivalent to @racket[pattern], except that it only matches syntax
objects that were written with @racket[[]] square brackets.

@examples[
  #:eval (make-base-eval)
  (require syntax/parse paren-shape/pattern-expander)
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
]}

@defform[(~braces . pattern)]{
Equivalent to @racket[pattern], except that it only matches syntax
objects that were written with @racket[{}] curly braces.

@examples[
  #:eval (make-base-eval)
  (require syntax/parse paren-shape/pattern-expander)
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


