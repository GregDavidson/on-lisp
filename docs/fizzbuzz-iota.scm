#!/usr/bin/guile
!#
(define (cycler count at-count otherwise)
	(let ( (n 1) )
		(lambda ()
			(set! n (if (= n count) 1 (1+ n)))
			(if (= n 1) at-count otherwise) ) ) )

(define (fizzbuzz cyclers count)
	(define (step i)
		(let ( (str (string-join (map (lambda (f) (f)) cyclers) "")) )
			(display (if (string-null? str) (number->string i) str))
			(newline) ) )
	(map step (iota count)) )

(fizzbuzz (list (cycler 3 "fizz" "") (cycler 5 "buzz" "")) 20)
