#lang racket/base

(provide ~parens
         ~brackets
         ~braces
         ~paren-shape
         )

(require syntax/parse
         syntax/parse/define
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse))
(module+ test
  (require rackunit))

;; paren-shape : Stx -> Char
;; Where stx either doesn't have a 'paren-shape property, or it has
;; one with a Char. If it doesn't have the property, it is assumed to
;; be a normal paren
(define (paren-shape stx)
  (or (syntax-property stx 'paren-shape) #\( ))

(begin-for-syntax
  ;; close : Char -> Char
  ;; Given a paren-shape char, returns the char that should close it
  (define (close c)
    (match c
      [#\( #\)]
      [#\[ #\]]
      [#\{ #\}]
      )))

;; `(~paren-shape #\( a b c)`
;; matches only lists of three elements with a paren-shape of (a b c)
;; `(~paren-shape #\[ a b c)`
;; matches only lists of three elements with a paren-shape of [a b c]
;; `(~paren-shape #\{ a b c)`
;; matches only lists of three elements with a paren-shape of {a b c}
(define-syntax ~paren-shape
  (pattern-expander
   (syntax-parser
     [(_ c:char . pat)
      #:with tmp (generate-temporary)
      #:with fail-msg
      (format "expected ~a and ~a" (syntax-e #'c) (close (syntax-e #'c)))
      #'(~and tmp
              (~fail #:unless (char=? 'c (paren-shape #'tmp)) 'fail-msg)
              pat)])))

(define-simple-macro
  (define-partial-pattern-expander name:id base-expander:id arg)
  (define-syntax name
    (pattern-expander
     (syntax-parser
       [(_ . pat)
        #'(base-expander arg . pat)]))))

(define-partial-pattern-expander ~parens   ~paren-shape #\( )
(define-partial-pattern-expander ~brackets ~paren-shape #\[ )
(define-partial-pattern-expander ~braces   ~paren-shape #\{ )

;; -----------------------------------------------------------------------------

(module+ test
  (check-equal? (syntax-parse #'(1 2 3)
                  [(~parens a b c)
                   (syntax-e #'a)])
                1)
  (check-equal? (syntax-parse #'[1 2 3]
                  [(~brackets a b c)
                   (syntax-e #'a)])
                1)
  (check-equal? (syntax-parse #'{1 2 3}
                  [(~braces a b c)
                   (syntax-e #'a)])
                1)
  (check-equal? (syntax-parse #'[[1 2 3]]
                  [(~brackets (~brackets a b c))
                   (syntax-e #'a)])
                1)

  (check-exn #rx"expected \\( and \\)"
             (λ () (syntax-parse #'[1 2 3]
                     [(~parens a b c)
                      #'a])))
  (check-exn #rx"expected \\[ and \\]"
             (λ () (syntax-parse #'{1 2 3}
                     [(~brackets a b c)
                      #'a])))
  (check-exn #rx"expected \\{ and \\}"
             (λ () (syntax-parse #'(1 2 3)
                     [(~braces a b c)
                      #'a])))
  )
