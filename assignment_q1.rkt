#lang racket

; cons pair of two numbers
(cons 1 2)

; A list of 3 numbers, using only the cons function
(cons 3 (cons 2 (cons 1 empty)))

; A list containing a string, a number and a nested list of three numbers, using only the cons function.
(cons '(1 2 3) (cons 2 (cons "banana" empty)))

; A list containing a string, a number and a nested list of three numbers, using only the list function
(list '(123) 2 "banana")

; A list containing a string, a number and a nested list of three numbers, using only the append function
(append '(1 2 3) '(2) (cons "banana" '()))
(append '(1 2 3) '(2) '("banana"))