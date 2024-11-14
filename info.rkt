#lang info
(define version "1.0")
(define collection 'multi)
(define deps (list "base" "rackunit"
  "https://github.com/cmsc430/a86.git?path=#main"))

(define test-omit-paths (list "knock-plus/test/compile.rkt"))
