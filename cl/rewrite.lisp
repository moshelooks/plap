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

(defun upwards (rule expr)
  (if (consp expr)
      (let ((subexprs (mapcar (lambda (subexpr) (upwards rule subexpr))
			      (cdr expr))))
	(funcall rule (blockn (mapc (lambda (x y) 
				      (unless (eq x y)
					(return (cons (car expr) subexprs))))
				    (cdr expr) subexprs)
			      expr)))
      expr))
(defun downwards (rule expr)
  (if (consp expr)
      (let ((result (funcall rule expr)))
	(if (consp result)
	    (let ((subresults (mapcar (bind #'downwards rule /1) 
				      (cdr result))))
	      (if (and (eq result expr) (every #'eq subresults (cdr expr)))
		  expr
		  (cons (car result) subresults)))
	    result))
      expr))

;;; returns nil for (next-most-general any)
;;; note that (next-most-general nil) is any
(defun next-most-general (type) 
  (assert (not (tuple-type-p type)) () "tuples not yet supported here")
  (if (consp type)
      (aif (next-most-general (cadr type)) (list (car type) it) 'any)
      (case type (any nil) (t 'any))))
(define-test next-most-general
  (assert-equal 'any (next-most-general 'bool))
  (assert-equal '(list any) (next-most-general '(list bool)))
  (assert-equal 'any (next-most-general '(list any)))
  (assert-equal nil (next-most-general 'any)))

(defvar *type-to-reductions* (make-hash-table))
(defvar *reduction-prerequisites* (make-dag))
(defmacro define-reduction (name (expr-name) &key
			    (type 'any) prerequisites
			    (condition t) action order cleanups)
  (assert (and condition action)
	  () "Condition and action required for reduction")
  `(let ((fn (defun ,name (,expr-name) ; create the reduction function
	       (flet ((call (,expr-name)
			(if (and (isa (expr-type ,expr-name) ',type)
				 ,condition)
			    ,action
			    ,expr-name)))
		 (let ((,expr-name (reduce-with ,prerequisites ,expr-name)))
		   (if (consp ,expr-name)
		       (let ((result ,(if order
					  `(funcall #',order #'call ,expr-name)
					  `(call ,expr-name))))
			 (if (not (eq result ,expr-name))
			     (reduce-with ,cleanups result)
			     result))
		       ,expr-name))))))

     (dag-insert-node fn *reduction-prerequisites*) ; update the prerequisites
     ;(print "inserted node")(print fn)
     (dolist (prereq ,prerequisites)		    ; dag
      ; (print "adding edge")
       ;(print (symbol-function prereq))
       ;(print fn)
       (dag-insert-edge prereq fn *reduction-prerequisites*))
     
     (unless (gethash ',type *type-to-reductions*) ; make sure there's a type
       (setf (gethash ',type *type-to-reductions*) ; entry in the table
	     (labels ((find-and-copy-hash (type)
			(let ((next (next-most-general type)))
			  (if next
			      (aif (gethash next *type-to-reductions*)
				   (copy-hash-table it)
				   (find-and-copy-hash next))))))
	       (find-and-copy-hash ',type))))
	   
     (maphash (lambda (t2 rules)       ; insert the rule in all matching type
		(if (isa t2 ',type) ; entries, respecting prereq ordering
		    (setf rules 
			  (dag-order-insert fn rules 
					    *reduction-prerequisites*))))
	      *type-to-reductions*)
     
     fn)) ;returns the function for the reduction rule we've defined

(defun reduce-with (rules expr)
  (let ((result (blockn (reduce (lambda (expr rule) 
				  (if (consp expr)
				      (funcall rule expr)
				      (return expr)))
				rules :initial-value expr))))
    (if (equalp expr result) expr (reduce-with rules result))))
(defun getreductions (type)
  (aif (gethash type *type-to-reductions*) it 
       (gethash (next-most-general type) *type-to-reductions*)))
(defun full-reduce (expr &key (type (expr-type expr)))
  (reduce-with (getreductions type) expr))

;;;; general-purpose reductions are defined here

(define-reduction sort-commutative (expr)
  :condition (commutativep (car expr))
  :action (let ((sorted (nondestructive-sort (cdr expr) #'total-order)))
	    (if (eq sorted (cdr expr)) expr (cons (car expr) sorted)))
  :order upwards)

(define-reduction flatten-associative (expr)
  :condition (associativep (car expr))
  :action 
  (labels ((try-flatten (subexprs)
	     (blockn (mapl (lambda (rest)
			     (if (and (consp (car rest))
				      (eq (caar rest) (car expr)))
				 (return (nconc (copy-range subexprs rest)
						(copy-list (cdar rest))
						(try-flatten (cdr rest))))))
			   subexprs))))
    (let ((subexprs (try-flatten (cdr expr))))
      (if (eq subexprs (cdr expr)) expr (cons (car expr) subexprs))))
  :order upwards)
(define-test flatten-associative
  (assert-equal '(and x y (or q w)) 
		(flatten-associative '(and x (and y (or q w))))))

(define-reduction compress-identical-subtrees (expr)
    :action (equalp-to-eq expr)
    :order upwards)

