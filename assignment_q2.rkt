#lang racket

; not too sure if I need to check null el or empty lists?
(define (ins_beg el lst)
  (cons el lst))

(define (ins_end el lst)
  (append lst (cons el '())))

; counts the number of top-levels items in a list
(define (cout_top_level lst)
  (if (empty? lst) 0 (+ 1 (cout_top_level (cdr lst)))))

; non-tail recursive function: counts the number of times an item occurs in a list of items
(define (count_instances el lst)
  (cond [(empty? lst) 0]
    [(equal? (car lst) el) (+ 1 (count_instances el (cdr lst)))] ; add one to the outcome of recursion
    [else (count_instances el (cdr lst))])) ; else just a regular element so move on

; tail recursive function: counts the number of times an item occurs in a list of items
(define (count_instances_tr_running el lst) (count_instances_tr el lst 0))
(define (count_instances_tr el lst total) ; introduce idea of running total
  (cond [(empty? lst) total]
    [(equal? (car lst) el) (count_instances_tr el (cdr lst) (+ 1 total))]
    [else (count_instances_tr el (cdr lst) total)]))

; Have tail and normal recursion done here.
; tail recursion version
(define (count_instances_deep_tr_running el lst) (count_instances_deep_tr el lst 0))
(define (count_instances_deep_tr el lst total)
  (cond [(empty? lst) total]
        [(list? (car lst)) (count_instances_deep_tr el (cdr lst) (+ total (count_instances_deep_tr el (car lst) total)))]
        [(equal? (car lst) el) (count_instances_deep_tr el (cdr lst) (+ 1 total))]
        [else (count_instances_deep_tr el (cdr lst) total)]))

; no tail recursion
; had trouble here initially as was not adding the two recursions
; i.e. the main list recursion, and then any sublist recursion
(define (count_instances_deep el lst)
  (cond [(empty? lst) 0]
        [(list? (car lst)) (+ (count_instances_deep el (cdr lst))(count_instances_deep el (car lst)))]
        [(equal? (car lst) el) (+ 1 (count_instances_deep el (cdr lst)))]
        [else (count_instances_deep el (cdr lst))]))

; examples for part A
(display "Output part A")(newline)
(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))

; examples for part B
(display "Output part B")(newline)
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))

; example for part C
(display "Output part C")(newline)
(cout_top_level '(2 4 5 6 '(3 4 5) 7 8 9))

; example for part D
(display "Output part D")(newline)
(count_instances 'a '(a a a b c d s a))

; example for part E
(display "Output part E") (newline)
(count_instances_tr_running 'a '(a a a b c d s a))

; example for part F (two versions for this as above)
(display "Output part F")
(count_instances_deep 'a '( a b c '(a b a) c))

