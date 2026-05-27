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

  (begin ;; Con
    (check-equal? (run '(if (zero? 0) 1 2)) 1)
    (check-equal? (run '(if (zero? 1) 1 2)) 2)
    (check-equal? (run '(if (zero? -7) 1 2)) 2)
    (check-equal? (run '(if (zero? 0)
                            (if (zero? 1) 1 2)
                            7))
                  2)
    (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                            (if (zero? 1) 1 2)
                            7))
                  7))

  (begin ;; Dupe
    (check-equal? (run #t) #t)
    (check-equal? (run #f) #f)
    (check-equal? (run '(if #t 1 2)) 1)
    (check-equal? (run '(if #f 1 2)) 2)
    (check-equal? (run '(if 0 1 2)) 1)
    (check-equal? (run '(if #t 3 4)) 3)
    (check-equal? (run '(if #f 3 4)) 4)
    (check-equal? (run '(if  0 3 4)) 3)
    (check-equal? (run '(zero? 4)) #f)
    (check-equal? (run '(zero? 0)) #t))

  (begin ;; Dupe+
      (check-equal? (run '(not #t)) #f)
      (check-equal? (run '(not #f)) #t)
      (check-equal? (run '(not 7)) #f)
      (check-equal? (run '(cond [else #t])) #t)
      (check-equal? (run '(cond [(not #t) 2] [else 3])) 3)
      (check-equal? (run '(cond [(if #t #t #f) 2] [else 3])) 2)
      (check-equal? (run '(cond [(zero? 1) 2] [(if (not (zero? (sub1 2))) #t #f) 4] [else 3])) 4)
      (check-equal? (run '(cond [#t 1] [else 2])) 1)
      (check-equal? (run '(cond [1 1] [else 2])) 1))

  (begin ;; Dupe++
      (check-equal? (run '(case 2 [else 1])) 1)
      (check-equal? (run '(case 2 [() 3] [else 1])) 1)
      (check-equal? (run '(case 2 [(2) 3] [else 1])) 3)
      (check-equal? (run '(case 4 [(2) 3] [else 1])) 1)
      (check-equal? (run '(case 2 [(7 2) 3] [else 1])) 3)
      (check-equal? (run '(case 4 [(7 2) 3] [else 1])) 1)
      (check-equal? (run '(case 2 [(7 2 #t) 3] [else 1])) 3)
      (check-equal? (run '(case 4 [(7 2 #t) 3] [else 1])) 1)
      (check-equal? (run '(case #t [(7 2 #t) 3] [else 1])) 3)
      (check-equal? (run '(case #f [(7 2 #t) 3] [else 1])) 1)))

