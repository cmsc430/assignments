#lang info
(define version "1.0")
(define collection 'multi)
(define deps
  (list "base"
        "rackunit"
        "git://github.com/cmsc430/a86"))

(define test-omit-paths
  (list "a86-basics/"
        "racket-basics/"
        "blackmail-plus/test/run-interp-tests.rkt"
        "blackmail-plus/test/run-compile-tests.rkt"
        "dupe-plus/test/run-interp-tests.rkt"
        "dupe-plus/test/run-compile-tests.rkt"
        "dupe-plus-plus/test/run-interp-tests.rkt"
        "dupe-plus-plus/test/run-compile-tests.rkt"
        "extort-plus/test/run-interp-tests.rkt"
        "extort-plus/test/run-compile-tests.rkt"
        "fraud-plus/test/run-interp-tests.rkt"
        "fraud-plus/test/run-compile-tests.rkt"
        "hoax-plus/test/run-compile-tests.rkt"
        "iniquity-plus/test/run-compile-tests.rkt"
        "jig-plus/test/run-compile-tests.rkt"        
        "knock-plus/test/run-compile-tests.rkt"
        "hoax-plus/test/run-compile-tests.rkt"
        "loot-exceptions/test/run-compile-tests.rkt"
        "loot-bignums/test/run-compile-tests.rkt"))
