#lang info

(define collection 'multi)

(define deps
  '("base"
    ["syntax-classes-lib" #:version "1.1"]
    ))

(define build-deps
  '("rackunit-lib"
    "scribble-lib"
    "racket-doc"
    "syntax-classes-doc"
    ))

