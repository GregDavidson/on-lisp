#!/usr/bin/guile
!#
(define (fizzbuzz i-max fizz-init buzz-init)
  (define (bump n init) (if (= 0 n) init (- n 1)))
  (define (iter i fizz buzz)
    (if (<= i i-max)
	(begin
	  (if (= 0 fizz) (display 'fizz))
	  (if (= 0 buzz) (display 'buzz))
	  (if (and (> fizz 0) (> buzz 0)) (display i))
	  (newline)
	  (iter (+ i 1)
		(bump fizz fizz-init)
		(bump buzz buzz-init)) ) ) )
  (iter 1 fizz-init buzz-init) )
(fizzbuzz 100 2 4)


