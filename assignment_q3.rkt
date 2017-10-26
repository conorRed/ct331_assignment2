#lang racket
; tree that will be used to test functions
(define tree '(((() 4 ()) 5 (() 6 ())) 7 ((() 8 ()) 9 (() 11 ()))))

; "Display in sorted order the contents of a binary search tree"

; simple traversal had to use begin because we want it to backtrack
; so it needs to go down right and left children
(define (traverse tree)
  (if (empty? tree) tree
             (begin(traverse (left_child tree))
                    (display (value tree)) (newline)
                    (traverse (right_child tree)))))


; "Return #t or #f if a given item is present or absent in a tree or not.
; The function should take the item and a list representing a tree."

; cant backtrack here so have to go down the correct path, thats why
; I check if value is less than or greater than.
(define (is_present? tree el)
  (cond [(empty? tree) #f]
        [(equal? (cadr tree) el) #t]
        [(< el (value tree)) (is_present? (left_child tree) el)] 
        [(> el (value tree)) (is_present? (right_child tree) el)]))


; "Insert an item correctly into a list representing a binary search tree.
; Your function should take an item and a tree as inputs."

; Originally had problems with this but eventually discovered
; that rebuilding tree with new item is most elegant. Whatever path you
; take you are developing the tree as you go down it
(define (insert_el tree el)
  (cond[(null? tree) (append tree (create_node '() el '()))]
       [(is_present? tree el) (display "element already there!")]
       [(< el (value tree)) (create_node (insert_el (left_child tree) el) (value tree) (right_child tree))]
       [(> el (value tree)) (create_node (left_child tree) (value tree) (insert_el (right_child tree) el))]))

; Take a list of items and insert them into a binary search tree.
(define (insert_list tree list) (if (empty? list) tree (insert_list (insert_el tree (car list)) (cdr list))))

; Implement a tree-sort algorithm. Your function should take a list of items and display them in sorted order
(define (tree_sort list) (traverse (insert_list '() list)))

; "Implement a higher order version of the tree-sort function that takes a
; list and a function that determines the sorted order. For example, write
; a version that sorts the list in ascending, descending and ascending based on last digit."

; Was a but confused with this question. Was not too sure about passing functions as parameters.
; Also, I struggled to develop a descending order function. I think I got the general Idea that
; a function is passed to a function in order to modulate the code just struggled to implement
(define (tree_sort_option list order_function) (order_function (tree_sort (insert_list list '())) null ))


; Helper functions ------------------------------------------
(define (create_node left value right) (list left value right))

(define (value node) (cadr node))

(define (right_child node) (caddr node))

(define (left_child node) (car node))

(define (last_element list)
  (cond ((null? (cdr list)) (car list))
        (else (last_element (cdr list)))))

; attempt at descending order function
; attempting to reverse order of list already sorted
; in ascending order from tree sort by placing into
; new list starting from last element
(define (descending_order list new_list)(
        cond
          [(empty? (cdr list)) new_list]
          [else (descending_order (cdr list) new_list)]))

(define (last_digit x y)(
; Would compare two numbers by both of there remainders to modulo 10
; in the case of the two digit numbers in this code's tree 
)

(define (ascending_order list new_list) list)

; -------------------------------------------------------------

; Examples
(display tree)(newline)
; Part A
(display "Output for part A traversal")(newline)
(traverse tree)

; Part B
(display "Output for part B checking for value 11 in tree")(newline)
(is_present? tree 11)

; Part C
(display "Output for part C inserting 12 then 1")(newline)
(insert_el tree 12)
(insert_el tree 1)

; Part D
(display "Output for part D insertinf list of 12 13 and 14")(newline)
(insert_list tree '(12 13 14))

; Part E
(display "Output for part E sorting list")(newline)
(tree_sort '(3 4 6 2 1 12 13 14))

; Part F
(display "Output for part F sorting list")(newline)
(tree_sort_option '(3 4 6 2 1 12 13 14) ascending_order)