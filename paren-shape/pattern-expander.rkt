#lang racket/base

(provide ~parens
         ~brackets
         ~braces
         ~paren-shape
         )

(require syntax/parse
         syntax/parse/class/paren-shape
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit))

;; `(~paren-shape #\( a b c)`
;; matches only lists of three elements with a paren-shape of (a b c)
;; `(~paren-shape #\[ a b c)`
;; matches only lists of three elements with a paren-shape of [a b c]
;; `(~paren-shape #\{ a b c)`
;; matches only lists of three elements with a paren-shape of {a b c}
(define-syntax ~paren-shape
  (pattern-expander
   (syntax-parser
     [(_ #\( . pat) #'(~parens . pat)]
     [(_ #\[ . pat) #'(~brackets . pat)]
     [(_ #\{ . pat) #'(~braces . pat)])))

;; -----------------------------------------------------------------------------

(module+ test
  ;; These use read-syntax to generate the syntax objects
  ;; so that they will have the correct syntax properties
  (define parens-123   (read-syntax 'test (open-input-string "(1 2 3)")))
  (define brackets-123 (read-syntax 'test (open-input-string "[1 2 3]")))
  (define braces-123   (read-syntax 'test (open-input-string "{1 2 3}")))
  (define double-brackets-123 (read-syntax 'test (open-input-string "[[1 2 3]]")))

  (check-equal? (syntax-parse parens-123
                  [(~parens a b c)
                   (syntax-e #'a)])
                1)
  (check-equal? (syntax-parse brackets-123
                  [(~brackets a b c)
                   (syntax-e #'a)])
                1)
  (check-equal? (syntax-parse braces-123
                  [(~braces a b c)
                   (syntax-e #'a)])
                1)
  (check-equal? (syntax-parse double-brackets-123
                  [(~brackets (~brackets a b c))
                   (syntax-e #'a)])
                1)

  (check-exn #rx"expected list or pair surrounded by parentheses"
             (λ () (syntax-parse brackets-123
                     [(~parens a b c)
                      #'a])))
  (check-exn #rx"expected list or pair surrounded by square brackets"
             (λ () (syntax-parse braces-123
                     [(~brackets a b c)
                      #'a])))
  (check-exn #rx"expected list or pair surrounded by curly braces"
             (λ () (syntax-parse parens-123
                     [(~braces a b c)
                      #'a])))
  )
