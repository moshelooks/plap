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
 * (function (arg-type1 arg-type2 ... arg-typeN) result-type) with N>=0
 * lambda-list
 * (enum name) where name may be any symbol
 * (act-result name) where name may be any symbol
 * t
Note that the type nil corresponds to the empty set (no values), whereas t 
corresponds to the universal set (all values). The type of nil is (list nil).

as of 10/10/08, enum and act-result types are not yet implemented
|# 

;;; for convenience
(define-constant bool 'bool)
(define-constant num 'num)
(define-constant lambda-list 'lambda-list)

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

;;; a list all types T that generalize the given type such that there exists no
;;; type both more general than the given type and less general than T
;;; note that (next-most-general-types nil) is an error since this is infinite
(defun next-most-general-types (type)
  (assert type)
  (cond ((atom type) (unless (eq type t) (list t)))
	((tuple-type-p type)
	 (or (mapcon (lambda (xs &aux (tmp (car xs)))
		       (prog1 (mapcar (lambda (x)
					(rplaca xs x)
					(copy-list type))
				      (next-most-general-types tmp))
			 (rplaca xs tmp)))
		     (cdr type))
	     (list t)))
	((list-type-p type) (aif (next-most-general-types (cadr type))
				 (mapcar (lambda (x) (list 'list x)) it)
				 (list t)))
	(t (assert nil () "unrecognized type ~S" type))))
(define-test next-most-general-types
  (assert-equal '(t) (next-most-general-types 'bool))
  (assert-equal '((list t)) (next-most-general-types '(list bool)))
  (assert-equal '(t) (next-most-general-types '(list t)))
  (assert-equal nil (next-most-general-types t))
  (assert-equal '((tuple (tuple t bool) num bool)
		  (tuple (tuple bool t) num bool)
		  (tuple (tuple bool bool) t bool)
		  (tuple (tuple bool bool) num t))
		(next-most-general-types '(tuple (tuple bool bool) num bool))))

(defun lookup-atom-type (x) ; returns nil iff no type found
  (cond ((or (eq x true) (eq x false)) bool)
	((numberp x) num)
	((lambda-list-p x) lambda-list)
	((null x) '(list nil))))
(defun atom-type (x) ; error if no type found
  (assert (lookup-atom-type x) () "no type found for atom ~S" x)
  (lookup-atom-type x))

;fixme - it would be nice to do clever type inference to find the arguments
;types for lambda-exprs, but this isn't strictly needed
(defun lookup-lambda-type (args body)
  (when (emptyp (lambda-list-argnames args)) `(function () ,(expr-type body))))
(defun lambda-type (args body)
  (assert (lookup-lambda-type args body) 
	  () "no type found for lambda ~S ~S" args body)
  (lookup-lambda-type args body))

(defun lookup-value-type (value) ; returns nil iff no type found
  (cond
    ((lambdap value) (lookup-lambda-type (arg0 value) (arg1 value)))
    ((consp value) `(list ,(reduce #'union-type value :key #'value-type)))
    ((arrayp value) `(tuple ,@(map 'list #'value-type value)))
    (t (lookup-atom-type value))))
(defun value-type (value) ; error if no type found
  (assert (lookup-value-type value) () "no type found for value ~S" value)
  (lookup-value-type value))
  
(let ((type-finders 
       (init-hash-table
	`((car ,(lambda (fn args) (cadr (funcall fn (car args)))))
	  (cdr ,(lambda (fn args) (cadr (funcall fn (car args)))))
	  (list ,(lambda (fn args) 
			 `(list ,(reduce #'union-type args :key fn))))
	  (append ,(lambda (fn args) (reduce #'union-type args :key fn)))
	  (tuple ,(lambda (fn args) `(tuple ,@(mapcar fn args))))
	  (lambda ,(lambda (fn args)
		     (declare (ignore fn))
		     (lambda-type (car args) (cadr args))))
	  (if ,(lambda (fn args) (union-type (funcall fn (cadr args))
					     (funcall fn (caddr args)))))))))
  (flet ((lookup-fn (fn args lookup)
	   (case fn
	     ((and or not <) bool)
	     ((+ - * / exp log sin abs) num)
	     (t (assert (gethash fn type-finders)
			() "can't find a type for fn ~S" fn)
		(funcall (gethash fn type-finders) lookup args)))))
    (defun funcall-type (fn args) (lookup-fn fn args #'value-type))
    (defun expr-type (expr &optional context)
      (if (consp expr)
	  (lookup-fn (fn expr) (args expr) (bind #'expr-type /1 context))
	  (or (lookup-atom-type expr) (gettype expr context))))))
(define-all-equal-test expr-type
    `((bool (true false ,%(and true false) ,%(not (or true false))))
      (num  (1 4.3 ,(/ 1 3) ,(sqrt -1) ,%(+ 1 2 3) ,%(* (+ 1 0) 3)))
      (bool (,%(< 2 3)))))
(define-test expr-type-with-bindings ;fixme instead of init-hash mkcontext
  (assert-equal bool (expr-type 'x (init-context '((x true)))))
  (assert-equal num (expr-type 'x (init-context '((x 42)))))
  (assert-equal '(list num) (expr-type %(list x) (init-context '((x 3.3)))))
  (assert-equal num (expr-type %(car (list x)) (init-context '((x 0))))))

;;; determines the types for the children based on the structure of expr and
;;; its type, given the bindings in context
(defun arg-types (expr context type)
  (acase (fn expr)
    (< (let ((type (reduce #'union-type (args expr)
			   :key (bind #'expr-type /1 context))))
	 `(,type ,type)))
    (list (ntimes (arity expr) (cadr type)))
    (split (let ((ttypes (mapcar (bind #'expr-type /1 context) 
				 (cdr (args expr)))))
	     (assert (every (compose (bind #'eq 'tuple /1) #'car) ttypes))
	     (assert (every (compose (bind #'eq 'list /1) #'caadr) ttypes))
	     (assert (every #'eq 
			    (mapcar #'cadadr ttypes) (mapcar #'caddr ttypes)))
	     (cons (list 'function (mappend #'cdr ttypes) type) ttypes)))
    (lambda (assert (eq 'function (car type)))
	    `(lambda-list ,(caddr type)))
    (tuple (assert (eql (length (cdr type)) (arity expr)) 
		   () "tuple length type mismatch - ~S for ~S" type expr)
	   (cdr type))
    (if (list bool type type))
    (t (if (closurep it) 
	   (ntimes (arity expr) type)
	   (let ((fn-type (gettype it context)))
	     (assert (function-type-p fn-type)
		     () "can't infer arg types for ~S" expr)
	     (assert (eql (length (cadr fn-type)) (arity expr))
		     () "arg length mismatch - ~S vs. ~S" fn-type expr)
	     (cadr fn-type))))))
(define-test arg-types
  (assert-equal '((function ((list num) num) bool) (tuple (list num) num))
		(arg-types %(split (lambda (x y) (< (car x) y))
				   (tuple (list 1 2) 3))
			   *empty-context* bool))
  (assert-equal '((function ((list num) num (list bool) bool) bool) 
		  (tuple (list num) num) (tuple (list bool) bool))
		(arg-types %(split (lambda (x y z q) (< (car x) y))
				   (tuple (list 1 2) 3)
				   (tuple (list true true) false))
			   *empty-context* bool)))

(defun default-expr (type)
  (ecase (icar type)
    (bool false)
    (num 0)
    (tuple (cons 'tuple (mapcar #'default-expr (cdr type))))
    (list nil)))

(defun genname (type)
  (declare (ignore type))
  (gensym)); (symbol-name type)))
