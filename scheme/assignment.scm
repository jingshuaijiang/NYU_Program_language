;;; Base Case: L is empty, return 0
;;; Assumption: (count-numbers M) returns a count of the numbers in M, for
;;; 			any list M smaller than L (including (car L) and (cdr L)).
;;; Step: If (car L) is a list, then return the sum of the count of the
;;; 	numbers in (car L) and the count of the numbers in (cdr L).
;;; 	If (car L) is a number, return 1 plus the count of the numbers in
;;; 	(cdr L). Otherwise, return the count of the numbers in (cdr L).
(define (count-numbers L)
    (cond((null? L)0)
         ((list?(car L)) (+ (count-numbers (car L)) (count-numbers (cdr L))))
         ((number?(car L)) (+ 1 (count-numbers (cdr L))))
         (else (count-numbers (cdr L)))))


;;; Base Case: L is empty, return (x)
;;; Assumption: (insert x L) returns the right order of an array consists of x and all the elements in L
;;; Step: If L is empty,then return a list with only one element x
;;;		  If x < car L, then insert the x to the first place of L
;;;		  If x > car L, then the right place for x should be in cdr L. We return the car L and the x inserted cdr L.
(define (insert x L)
    (cond ((null? L) (cons x L))
          ((< x (car L)) (cons x L))
          ((> x (car L)) (cons (car L) (insert x (cdr L))))))





;;; Base Case: L is empty, return M
;;; Assumption: (insert-all L M) returns a array which contains all the elements in L and M and are sorted in increasing order.
;;; Step: If L is empty,then return M
;;;		  else insert the car L element into an array which contains all the elements from M and cdr L
(define (insert-all L M)
    (cond ((null? L) M)
          (else (insert (car L) (insert-all (cdr L) M)))))






;;; Base Case: L is empty, return L
;;; Assumption: (sort L) using the insertion sort above to insert all elements from L to an empty list one by one. Since we can
;;; not use the existing functions we use letrec and lambda expressions to define insert-all and insert in the sort function.
;;; Step: If L is empty,then return L
;;;		  else define the (insert-allf) function which insert all elements from list A to list B in increasing order, then define the (insertf)
;;;		  function that insert an element into a list C. And then using (insert-allf L '()) to get the final list

(define (sort L)
    (cond ((null? L) L)
          (else (letrec ((insert-allf (lambda (A B)
                                       (cond ((null? A) B)
                                             (else (insertf (car A) (insert-allf (cdr A) B))))))
                         (insertf (lambda (x C)
                                    (cond ((null? C)(cons x C))
                                          ((< x (car C))(cons x C))
                                          ((> x (car C))(cons (car C) (insertf x (cdr C))))))))
                  (insert-allf L '())))))






;;; This is not a recursive function.
;;; Asumption: we just need to test the equality of symbols and return the related operation.

(define (translate op)
    (cond ((eq? op '+) +)
          ((eq? op '-) -)
          ((eq? op '*) *)
          ((eq? op '/) /)))




;;; Base Case: the first and the second element are both numbers just calculate their result and return.
;;; Assumption: (postfix-eval) recursively calculate  arg1 arg2, get their values and calculate the final result.
;;; 
;;; Step: If first and the second element are both numbers just calculate their result and return.
;;;		  else if the first element is a number then recursively calculate arg2 to make it to be a number and then calculate their result.
;;;		  else if the second element is a number then recursively calculate arg1 to make it to be a number and then calculate their result.
;;;		  else if the both of arg1 and arg2 are expressions, we recursively calculate them to make them to be numbers and then calculate their result.

(define (postfix-eval exp)
    (cond ((and (number? (car exp))(number? (cadr exp))) ((translate (caddr exp)) (car exp) (cadr exp)))
          ((number? (car exp)) ((translate (caddr exp)) (car exp) (postfix-eval (cadr exp))))
          ((number? (cadr exp)) ((translate (caddr exp)) (postfix-eval (car exp)) (cadr exp)))
          (else ((translate (caddr exp)) (postfix-eval (car exp))(postfix-eval (cadr exp))))))


;;; Base Case: L is empty, return the set containing the empty
;;; set, i.e. ’(()).
;;; Assumption: (powerset M) returns the powerset of M, for any set M
;;; smaller than L (including (cdr L)).
;;; Step: If L is empty return the set containing the empty
;;; 	  else insert (car L) into (powerset(cdr L)) and append them with (powerset(cdr L))

(define (powerset L)
    (cond ((null? L) '(()))
          (else (append (powerset (cdr L))
                        (map (lambda (subset) (cons (car L) subset))
                             (powerset (cdr L)))))))