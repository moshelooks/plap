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

;;; this song-and-dance is to avoid consing and/or removing the simp tag
;;; unnessecarily

simp itself is the sum of the rules applied - when does a applying a new rule
invalidate the simp status of an old on? in general we need to a concept
of munging that encompasses both rewrites and knobs - knobs are reversible
right now, need they be?
bleh on me for perpetuating the distinction left over from cpp

(defun upwards (rule expr)
  (labels ((proc (args)
	     (when args
	       (let ((first (upwards rule (car args)))
		     (rest (proc (cdr args))))
		 (if (and (eq first (car args)) (eq rest (cdr args)))
		     args
		     (cons first rest))))))
  (if (consp expr)
      (let ((args (proc (args expr))))
	(if (eq args (args expr))
	    (let ((result (funcall rule expr)))
	      (if (eq result expr)
		  expr
		  (unmark expr simp)))
	    (unmark (funcall rule (cons (fn expr) args))
      expr)))

	(if (eq args (args expr))
      (dolist ((args expr) subexpr)
	(let ((new (upwards rule subexpr)))
	  (if (eq new subseq)
      (let ((subexprs (mapcar (lambda (subexpr) (upwards rule subexpr))
			      (args expr))))
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


(defvar *type-to-reductions* (make-hash-table))
(defvar *reduction-prerequisites* (make-dag))
(defmacro define-reduction (name (expr-name) &key
			    (type t) prerequisites
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

(define-reduction sort-commutative (fn args markup)
  :condition (and (commutativep fn) (not (sortedp args #'total-order)))
  :action (pcons fn (sort (copy-list args) #'total-order) markup)
  :order upwards
  :preserves all)
(define-reduction flatten-associative (fn args markup)
  :condition (and (associativep fn) (find fn args :key ifn))
  :action (pcons fn
		 (mappend (lambda (arg) 
			    (if (eq (ifn arg) fn) (args arg) `(,arg)))
			  args)
		 markup)
  :order upwards)
(define-test flatten-associative
  (assert-equal '(and x y (or q w)) 
		(flatten-associative '(and x (and y (or q w))))))
