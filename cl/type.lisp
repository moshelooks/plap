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

Author: madscience@google.com (Moshe Looks) |#
(in-package :plop)

#|
types are as follows:
 * nil
 * bool
 * num
 * (list type)
 * (tuple type1 type2 type3 .... typeN) with N>1
 * (fn (arg-type1 arg-type2 ... arg-typeN) value-type) with N>=0
 * t
Note that the type nil corresponds to the empty set (no values), whereas t 
corresponds to the universal set (all values). The type of nil is (list nil).
|# 

;;; for convenience
(defvar bool 'bool)
(defvar num 'num)

;;; is every x a y? note that this is a partial ordering
(defun isa (x y)
  (if (and (consp x) (consp y))
      (let ((op1 (car x))
	    (op2 (car y)))
	(and (eq op1 op2)
	     (case op1
	       (list (isa (cadr x) (cadr y)))
	       (tuple (and (eql (length x) (length y))
			   (every #'isa (cdr x) (cdr y))))
	       (fn (and (isa (caddr x) (caddr y))
			(eql (length (cadr x)) (length (cadr y)))
			(every #'isa (cadr y) (cadr x)))))))
      (or (eq x y) (eq y t) (eq x nil))))
;	  (and (eq 'nil x) (eq 'list (car y)))
;	  (or (eq x y) (eq y t)))))
(define-test isa
  (let ((types '(t
		 (num)
		 (bool)
		 ((list t)
		  ((list num))
		  ((list (list t))
		   ((list (list num)))
		   ((list (list (tuple bool bool))))))
		 ((tuple t t)
		  ((tuple bool t)
		   ((tuple bool (list t)))
		   ((tuple bool bool)))
		  ((tuple t num)))
		 ((fn (num t) t)
		  ((fn (num t) bool)))
		 ((fn (bool bool) t)
		  ((fn (bool t) t)
		   ((fn (t t) t)
		    ((fn (t t) num))
		    ((fn (t t) (list t)))))))))
    (map-internal-nodes (lambda (type)
			  (assert-true (isa type t) type)
			  (assert-equal (eq type t) (isa t type) type)
			  (assert-true (isa type type) type))
			types)
    (map-subtrees 
     (lambda (tree)
       (map-subtrees
	(lambda (subtree)
	  (assert-true (isa (car subtree) (car tree))
		       (car subtree) (car tree))
	  (assert-equal (eq tree subtree) (isa (car tree) (car subtree))
			(car tree) (car subtree)))
	tree)
       (mapc (lambda (subtree)
	       (mapc (lambda (other)
		       (unless (eq subtree other)
			 (map-subtrees (lambda (subtree2)
					 (assert-false
					  (isa (car subtree) (car subtree2))
					  (car subtree) (car subtree2)))
				     other)))
		     (cdr tree)))
	     (cdr tree)))
     types))
  (assert-true (isa '(list nil) '(list bool)))
  (assert-false (isa '(list bool) '(list nil)))
  (assert-false (isa '(list (list nil)) '(list bool)))
  (assert-true (isa '(list (list nil)) '(list (list bool))))
  (assert-false (isa '(list bool) '(list (list nil)))))

(defun union-type (x y)
  (cond
    ((equal x y) x)
    ((and (consp x) (consp y) (eq (car x) (car y)))
     (case (car x)
       (list `(list ,(union-type (cadr x) (cadr y))))
       (tuple (if (same-length-p x y)
		  `(tuple ,@(mapcar #'union-type (cdr x) (cdr y)))
		  'any))
       (function (blockn `(function ,(mapcar (lambda (x y)
					       (or (type-intersection x y)
						   (return 'any)))
					     (cadr x) (cadr y))
				    ,(union-type (caddr x) (caddr y)))))))
    (t 'any)))

;;     (reduce-until 'any #'pairwise-union types)))
;; (define-all-equal-test type-union
;;     `((bool ((bool) (bool bool)))
;;       (num ((num) (num num)))
;;       (nil ((nil) (nil nil)))
;;       ((list num) ((list num) nil (list num))))

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
  (defun fun-type (f) 
    (multiple-value-bind (v exists) (gethash f fun2type)
      (unless exists 
	(warn (concatenate 'string "couldn't find a type for " (string f))))
      v)))
(defun atom-type (x)
  (cond ((or (eq x 'true) (eq x 'false)) 'bool)
	((numberp x) 'num)
	((null x) nil)
	(t (fun-type x))))
(let ((type-finders 
       (init-hash-table
	`((car ,(lambda (args) (cadr (expr-type (car args)))))
	  (cdr ,(lambda (args) (cadr (expr-type (car args)))))
	  (list ,(lambda (args) (if args
				    (list 'list
					  (let ((type nil))
					    (dolist (arg args type)
					      (aif (expr-type arg)
						   (if type 
						       (if (not (eq type it))
							   (setf type 'any))
						       (setf type it)))))))))
	  (append ,(lambda (args) (dolist (arg args)
				    (aif (expr-type (arg))

					      
					      
(defun expr-type (expr &optional bindings)
  (if (consp expr)
      (case (car expr)
	((and or not) 'bool)
	((+ - * / exp log sin) 'num)
	(tuple (cons 'tuple (mapcar #'expr-type (cdr expr))))
	(t (aif (gethash (type-finders (car expr)))
		(funcall it (cdr expr))
		`(list ,(expr-type (car expr))))))
      (if expr
	  (aif (and bindings (gethash expr bindings)) it (atom-type expr)))))
(define-all-equal-test expr-type
    `((bool (true false (and true false) (not (or true false))))
      (num  (1 4.3 ,(/ 1 3) ,(sqrt -1) (+ 1 2 3) (* (+ 1 0) 3)))
      ((fun bool (list bool)) (and or))
      ((fun bool bool) (not))
      ((fun num (list num)) (+ *))
      ((fun bool any any) (< =))
      (bool ((< 2 3) (< true false) (= (= 2 2) 7)))))



(defun tuple-type-p (type) (and (consp type) (eq (car type) 'tuple)))

;;; a typemap is a hashtable of types mapping to hashset of var names
(defun make-type-map () (make-hash-table))
(defun init-type-map (contents) 
  (init-hash-table (mapcar (bind #'list (car /1) (init-hash-set (cadr /1)))
			   contents)))

;; (defun is-type-p (type) 
;;   (if (consp type)
;;       (matches (car type) (list tuple fun))
;;       (matches type (bool num))
