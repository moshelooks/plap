#| Copyright 2008 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Author: madscience@google.com (Moshe Looks)

This defines the basic language used to represent evolved programs, along with
type inference and evaluation code. Valid types are: any, bool, num,
nil, (list t), (tuple t1 ... tN), (fun out in1 ... inN), (enum name),
and (act-result name), where name may be any symbol. |#
(in-package :plop)

;; a total ordering on all plop expressions
;; returns less, nil, or greater, with the important property that (not symbol)
;; is always ordered immediately after symbol
(defun total-cmp (l r) 
  (flet ((elem-cmp (l r)
	   (if (numberp l)
	       (if (numberp r)
		   (if (= l r) nil (if (< l r) 'less 'greater))
		   'greater)
	       (if (numberp r)
		   'less
		   (if (eql l r) nil (if (string< l r) 'less 'greater))))))
    (if (consp l)
	(if (and (eq (car l) 'not) (consp (cdr l)) (not (consp (cadr l))))
	    (aif (total-cmp (cadr l) r) it 'greater)
	    (if (consp r)
		(aif (total-cmp (car l) (car r))
		     it 
		     (total-cmp (cdr l) (cdr r)))
		'greater))
	(if (consp r) 
	    (if (and (eq (car r) 'not) (consp (cdr r)) (not (consp (cadr r))))
		(aif (elem-cmp l (cadr r)) it 'less)
		'less)
	    (elem-cmp l r)))))
(defun total-order (l r)
  (eq (total-cmp l r) 'less))
(define-test total-order
  (assert-equal '(1 2 3) (sort '(3 1 2) #'total-order))
  (assert-equal '((a 1) (a 1 1) (a 2)) 
		(sort '((a 2) (a 1) (a 1 1)) #'total-order))
  (assert-equal 
   '(a b c nil 1 2 (a 1) (a 2) (b b) (b b b))
   (sort '(2 b 1 a c (a 2) (a 1) (b b) (b b b) nil) #'total-order))
  (assert-equal
   '((a (a (b c))) (a (a (b c)) b) (a (a (b c d))))
   (sort '((a (a (b c))) (a (a (b c)) b) (a (a (b c d)))) #'total-order))
  (assert-equal
   '(a (not a) b (not b) c)
   (sort '((not a) (not b) c b a) #'total-order)))

(defun commutativep (x)
  (matches x (and or)))
(defun associativep (x)
  (matches x (and or)))
(defun identityp (x)
  (matches x (and or)))

(defun expr-size (expr) 
  (if (consp expr)
      (reduce #'+ (mapcar #'expr-size (cdr expr)) :initial-value 1) 
      1))

(defun expr-depths (expr)
  (if (consp expr)
      (mapcar #'1+ (mapcan #'expr-depths (cdr expr)))
      (list 0)))
(define-test expr-depths
  (assert-equal '(1 2 2 1) (expr-depths '(and x (or y z) q)))
  (assert-equal '(0) (expr-depths 'x)))

(defun equalp-to-eq (expr)
  (mapl (lambda (expr1) (if (consp (car expr1))
			 (mapl (lambda (expr2)
				 (if (equalp (car expr1) (car expr2))
				     (setf (car expr2) (car expr1))))
			       (cdr expr1))))
	expr))
(define-test equalp-to-eq
  (let* ((foo '(and (or x y) (or x y) (or x y)))
	 (goo (copy-tree foo)))
    (equalp-to-eq foo)
    (assert-eq (second foo) (third foo) (fourth foo))
    (assert-equal foo goo)))


; general utility for decomposing an expr by type and contents
(defmacro decompose ((expr-name &optional (type (expr-type expr-name)))
		     &body type-clauses)
  (labels ((dosub (condition sc) `(,condition ,@sc))
	   (boolproc (sub)
	     (dosub (case (car sub)
		      (literal `(literalp ,expr-name))
		      (junctor `(matches (car ,expr-name) (and or)))
		      ((t) t)
		      (t `(,(car sub) ,expr-name)))
		    (cdr sub))))
    `(ecase ,type
       ,@(mapcar (lambda (clause)
		   `(,(car clause) 
		      (cond ,@(ecase (car clause)
				     (bool (mapcar #'boolproc 
						   (cdr clause)))))))
		 type-clauses))))
(define-test decompose-bool
  (flet ((dectest (expr)
	   (decompose (expr 'bool)
	     (bool
	      (literal 'literal)
	      (junctor 'junctor)
	      (t 'other)))))
    (assert-equal 'literal (dectest 'x))
    (assert-equal 'literal (dectest '(not x)))
    (assert-equal 'junctor (dectest '(and x y)))
    (assert-equal 'other (dectest '(foo bar baz)))))

;(defun mapcar-expr (fn expr)
	     


;idea - make tuples arrays

;; (defun type-union (&rest types)
;;   (flet ((pairwise-union (x y)
;; 	   (cond ((equal x y) x)
;; 		 ((and (consp t) (consp y)
;; 		       (eq (car t) (car y)) (same-length x y))
;; 		  (case (car t)
;; 		    (list `(list ,(type-union (cadr t) (cadr y))))
;; 		    (tuple (cons 'tuple (mapcar #'type-union (cdr t) (cdr y))))
;; 		    (fun (aif (mapcar-until nil #'type-intersection 
;; 					    (cddr t) (cddr y))
;; 			      `(fun ,(type-union (cadr t) (cadr y)) ,@it)))
;; 		    (t 'any)))
;; 		 (t 'any))))
;;     (reduce-until 'any #'pairwise-union types)))
;; (define-all-equal-test type-union
;;     `((bool ((bool) (bool bool)))
;;       (num ((num) (num num)))
;;       (nil ((nil) (nil nil)))
;;       ((list num) ((list num) nil (list num))))
;;   (lambda (args) (apply #'type-union args)))

;; (let (fun2type 
;;       (init-hash-table
;;        `((and (fun bool (list bool)))
;; 	 (or (fun bool (list bool)))
;; 	 (not (fun bool bool))
;; 	 (true bool)
;; 	 (false bool)
	 
;; 	 (+ (fun num (list num)))
;; 	 (* (fun num (list num)))
;; 	 (/ (fun num num num))
	 
;; 	 (< (fun bool any any)) ;fixme < and = just on nums
;; 	 (= (fun bool any any))

;; 	 (cons (fun (list any) any (list any)))
;; 	 (append (fun (list any) (list (list any))))
;; 	 (car (fun any (list any)))
;; 	 (cdr (fun (list any) (list any)))
;; 	 (length num)
;; 	 (accumulate (fun any (fun any any any) (list any) any))
;; 	 (mapcar (fun (list any) (fun any any) (list any)))

;; 	 (progn (act-result any),type-of-first)
;; 	  (andseq ,type-of-first)
;; 	  (orseq ,type-of-first)))))

;; 	   (cons ,(lambda (x l) (type-union `(list ,(tree-type x))
;; 					    (tree-type l))))
;; 	   (append ,(lambda (&rest ls) (apply #'type-union 
;; 					      (mapcar '#tree-type ls))))
;; 	   (car ,(lambda (l) (if (eq (car l) 'list)
;; 				 (tree-type (car l))
;; 				 (cadr (tree-type l)))))
;; 	   (cdr ,(lambda (l) (if (eq (car l) 'list)
;; 				 (tree-type `(list ,(cddr l)))
;; 				 (tree-type l))))
;; (type-of-first (lambda (&rest l) (tree-type (car l))))

;;   (defun fun-type (f) 
;;     (multiple-value-bind (v exists) (gethash f fun2type)
;;       (assert exists)
;;       v)))
;; x
