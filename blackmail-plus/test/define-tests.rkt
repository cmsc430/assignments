#lang racket
(provide test)
(require rackunit)

(define (test run)
  (begin ;; Abscond
    (check-equal? (run 7) 7)
    (check-equal? (run -8) -8))

  (begin ;; Blackmail
    (check-equal? (run '(add1 (add1 7))) 9)
    (check-equal? (run '(add1 (sub1 7))) 7))

  (begin ;; Blackmail+
    (check-equal? (run '(add2 1)) 3)
    (check-equal? (run '(add2 (add2 0))) 4)
    (check-equal? (run '(add2 (sub1 4))) 5)))

