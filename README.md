paren-shape
===
Racket pattern expanders for dealing with paren-shapes in syntax-parse macros

```racket
> (require syntax/parse paren-shape/pattern-expander)
> (syntax-parse #'(1 2 3)
    [(~parens a b c) #'a])
#<syntax 1>
> (syntax-parse #'(1 2 3)
    [(~brackets a b c) #'a])
;expected [ and ] in (1 2 3)
> (syntax-parse #'[1 2 3]
    [(~parens a b c) #'a])
;expected ( and ) in [a b c]
```
