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
 * (function (arg-type1 arg-type2 ... arg-typeN) value-type) with N>=0
 * (enum name) where name may be any symbol
 * (act-result name) where name may be any symbol
 * t
Note that the type nil corresponds to the empty set (no values), whereas t 
corresponds to the universal set (all values). The type of nil is (list nil).
|# 

;;; for convenience
(defvar bool 'bool)
(defvar num 'num)

(defun list-type-p (type) (eq (acar type) 'list))
(defun tuple-type-p (type) (eq (acar type) 'tuple))
(defun function-type-p (type) (eq (acar type) 'function))

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
	       (function (and (isa (caddr x) (caddr y))
			      (eql (length (cadr x)) (length (cadr y)))
			      (every #'isa (cadr y) (cadr x)))))))
      (or (eq x y) (eq y t) (eq x nil))))
;;; the union type of x and y is the smallest (most specific) type z such that
;;; (and (isa x z) (isa y z)) holds
(defun union-type (x y)
  (cond
    ((eq x y) x)
    ((and (consp x) (consp y) (eq (car x) (car y)))
     (ecase (car x)
       (list `(list ,(union-type (cadr x) (cadr y))))
       (tuple (if (same-length-p x y)
		  `(tuple ,@(mapcar #'union-type (cdr x) (cdr y)))
		  t))
       (function `(function ,(mapcar (lambda (x y)
				       (or (intersection-type x y)
					   (return-from union-type t)))
				     (cadr x) (cadr y))
			    ,(union-type (caddr x) (caddr y))))))
    ((eq x nil) y)
    ((eq y nil) x)
    (t t)))
;;; the intersection type of x and y is the largest (most general) type z
;;; such that (and (isa z x) (isa z y)) holds
(defun intersection-type (x y)
  (cond
    ((eq x y) x)
    ((and (consp x) (consp y) (eq (car x) (car y)))
     (ecase (car x)
       (list `(list ,(intersection-type (cadr x) (cadr y))))
       (tuple (if (same-length-p x y)
		  `(tuple ,@(mapcar (lambda (x y)
				      (or (intersection-type x y)
					  (return-from intersection-type)))
				    (cdr x) (cdr y)))))
       (function `(function ,(mapcar #'union-type (cadr x) (cadr y))
			    ,(or (intersection-type (caddr x) (caddr y))
				 (return-from intersection-type))))))
    ((eq x t) y)
    ((eq y t) x)))
;;; test for isa, union-type, and intersection-type
(define-test type-comparison
  (let ((types '(t
		 (num)
		 (bool)
		 ((list t)
		  ((list num))
		  ((list (list t))
		   ((list (list num)))
		   ((list (list (tuple bool bool))))))
		 ((tuple t t t)
		  ((tuple num t bool)))
		 ((tuple t t)
		  ((tuple bool t)
		   ((tuple bool (list t)))
		   ((tuple bool bool)))
		  ((tuple t num)))
		 ((function (num t) t)
		  ((function (num t) bool)))
		 ((function (bool bool) t)
		  ((function (bool t) t)
		   ((function (t t) t)
		    ((function (t t) num))
		    ((function (t t) (list t)))))))))
    ;; test isa
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
     types)
    (assert-true (isa '(list nil) '(list bool)))
    (assert-false (isa '(list bool) '(list nil)))
    (assert-false (isa '(list (list nil)) '(list bool)))
    (assert-true (isa '(list (list nil)) '(list (list bool))))
    (assert-false (isa '(list bool) '(list (list nil))))
    ;; test union-type and intersection-type
    (map-subtrees 
     (lambda (tree)
       (map-subtrees
	(lambda (subtree)
	  (assert-equal (car tree) (union-type (car tree) (car subtree))
			(car subtree))
	  (assert-equal (car tree) (union-type (car subtree) (car tree))
			(car subtree))
	  (assert-equal (car subtree)
			(intersection-type (car tree) (car subtree))
			(car tree))
	  (assert-equal (car subtree)
			(intersection-type (car subtree) (car tree))
			(car tree)))
	tree))
     types)
    (assert-equal nil (intersection-type '(function (num) bool) 
					 '(function (bool) num)))
    (assert-equal t (union-type '(function (num) num) '(function (bool) num)))
    (assert-equal '(function ((function (t) num)) num)
		  (union-type '(function ((function (num) num)) num)
			      '(function ((function (bool) num)) num)))
    (assert-equal t (union-type '(function ((function (num) bool)) num)
				'(function ((function (bool) num)) num)))
    (assert-equal bool (intersection-type bool t))))

;; look in svn for function-type code

(defun atom-type (x) ; returns nil iff no type found
  (cond ((or (eq x true) (eq x false)) bool)
	((numberp x) num)
	((null x) '(list nil))))

(defun value-type (expr) ; returns nil iff no type found
  (cond
    ((consp expr) `(list ,(reduce #'union-type (cdr expr) :key #'value-type)))
    ((arrayp expr) `(tuple ,@(map 'list #'value-type expr)))
    (t (atom-type expr))))

(let ((type-finders 
       (init-hash-table
	`((car ,(lambda (fn args) (cadr (funcall fn (car args)))))
	  (cdr ,(lambda (fn args) (cadr (funcall fn (car args)))))
	  (list ,(lambda (fn args) 
			 `(list ,(reduce #'union-type args :key fn))))
	  (append ,(lambda (fn args) (reduce #'union-type args :key fn)))
	  (tuple ,(lambda (fn args) `(tuple ,@(mapcar fn args))))))))
  (defun expr-type (expr &optional bindings)
    (if (consp expr)
	(case (car expr)
	  ((and or not <) bool)
	  ((+ - * / exp log sin) num)
	  (t (aif (gethash (car expr) type-finders)
		  (funcall it (bind #'expr-type /1 bindings) (cdr expr))
		  (error "unrecognized function ~S" (car expr)))))
	(or (atom-type expr) 
	    (aif (lookup-bindings expr bindings) ;fixme
		 (value-type it))))))
(define-all-equal-test expr-type
    `((bool (true false (and true false) (not (or true false))))
      (num  (1 4.3 ,(/ 1 3) ,(sqrt -1) (+ 1 2 3) (* (+ 1 0) 3)))
      ;((function (list bool) bool) (and or))
      ;((function bool bool) (not))
      ;((function (list num) num) (+ *))
      (bool ((< 2 3)))))
(define-test expr-type-with-bindings
  (assert-equal bool (expr-type 'x (init-hash-table '((x true)))))
  (assert-equal num (expr-type 'x (acons 'x 42 nil)))
  (assert-equal '(list num) (expr-type '(list x) (acons 'x 3.3 nil)))
  (assert-equal num (expr-type '(car (list x)) (init-hash-table '((x 0))))))

;;; contextually determines the types for the children
(defun arg-types (expr type)
  (assert (not (eq 'lambda (acar expr))))
  (case (car expr)
    (< '(num num)) ;fixme
    (list (mapcar (bind #'identity (cadr type)) (cdr expr)))
    (split `((tuple ,@()) (function (
    (t (mapcar (bind #'identity type) (cdr expr))))) ; works for most things
    

;;; a typemap is a hashtable of types mapping to hashset of var names
(defun make-type-map () (make-hash-table))
(defun init-type-map (contents)
  (init-hash-table (mapcar (bind #'list (car /1) (init-hash-set (cadr /1)))
			   contents)))

(defun default-value (type)
  (ecase (icar type)
    (bool true)
    (num 0)
    (function (assert nil));`(lambda ,(
    (list nil)))

(defun genname (type)
  (gensym (symbol-name type)))


;; (defun is-type-p (type) 
;;   (if (consp type)
;;       (matches (car type) (list tuple fun))
;;       (matches type (bool num))

;		     or atom-type? what about (1 2 3)???
;		     add the quote operator??