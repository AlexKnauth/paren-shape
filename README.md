paren-shape
===
This library is deprecated; use the
[paren-shape pattern expanders][stxclass-paren-shape] from
syntax-classes-lib instead.

Racket pattern expanders for dealing with paren-shapes in syntax-parse macros

```racket
> (require syntax/parse paren-shape/pattern-expander)
> (syntax-parse #'(1 2 3)
    [(~parens a b c) #'a])
#<syntax 1>
> (syntax-parse #'(1 2 3)
    [(~brackets a b c) #'a])
;expected list or pair surrounded by square brackets in: (1 2 3)
> (syntax-parse #'[1 2 3]
    [(~parens a b c) #'a])
;expected list or pair surrounded by parentheses in: [1 2 3]
```

  [stxclass-paren-shape]: http://docs.racket-lang.org/syntax-classes/index.html#%28mod-path._syntax%2Fparse%2Fclass%2Fparen-shape%29
