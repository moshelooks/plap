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

;; simp itself is the sum of the rules applied - when does a applying a new rule
;; invalidate the simp status of an old on? in general we need to a concept
;; of munging that encompasses both rewrites and knobs - knobs are reversible
;; right now, need they be?
;; bleh on me for perpetuating the distinction left over from cpp

;; (defun upwards (rule expr)
;;   (labels ((proc (args)
;; 	     (when args
;; 	       (let ((first (upwards rule (car args)))
;; 		     (rest (proc (cdr args))))
;; 		 (if (and (eq first (car args)) (eq rest (cdr args)))
;; 		     args
;; 		     (cons first rest))))))
;;   (if (consp expr)
;;       (let ((args (proc (args expr))))
;; 	(if (eq args (args expr))
;; 	    (let ((result (funcall rule expr)))
;; 	      (if (eq result expr)
;; 		  expr
;; 		  (unmark expr simp)))
;; 	    (unmark (funcall rule (cons (fn expr) args))
;;       expr)))

;; 	(if (eq args (args expr))
;;       (dolist ((args expr) subexpr)
;; 	(let ((new (upwards rule subexpr)))
;; 	  (if (eq new subseq)
;;       (let ((subexprs (mapcar (lambda (subexpr) (upwards rule subexpr))
;; 			      (args expr))))
;; 	(funcall rule (blockn (mapc (lambda (x y) 
;; 				      (unless (eq x y)
;; 					(return (cons (car expr) subexprs))))
;; 				    (cdr expr) subexprs)
;; 			      expr)))
;;       expr))

(defun mapargs (fn expr)
  (mapl (lambda (args)
	  (let ((result (funcall fn (car args))))
	    (unless (eq result (car args))
	      (return-from mapargs (pcons (fn expr) 
					  (nconc (copy-range (args expr) args)
						 (ncons result)
						 (mapcar #'fn (cdr args)))
					  (markup expr))))))
	(args expr))
  expr)
(defun mapargs-with-types (fn expr types)
  (mapl (lambda (args types)
	  (let ((result (funcall fn (car args) (car types))))
	    (unless (eq result (car args))
	      (return-from mapargs 
		(pcons (fn expr)
		       (nconc (copy-range (args expr) args)
			      (ncons result)
			      (mapcar #'fn (cdr args) (cdr types)))
		       (markup expr))))))
	(args expr) types)
  expr)
				     

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

under what circumstances will a reduction need a context argument??




(defun reduce-with (rules expr)
  (let ((result (blockn (reduce (lambda (expr rule) 
				  (if (consp expr)
				      (funcall rule expr)
				      (return expr)))
				rules :initial-value expr))))
    (if (equalp expr result) expr (reduce-with rules result))))

(defun full-reduce (expr &key (type (expr-type expr)))
  (reduce-with (getreductions type) expr))


(let ((type-to-reductions (make-hash-table))
      (dependencies (make-dag)))
  ;;; important note - register-reduction does not do any handling of markup,
  ;;; which must be created/removed by the reduction function (define-reduction
  ;;; can autogenerate this code
  (defun register-reduction (type reduction assumes)
    (dag-insert-node reduction dependencies) ;; first, update dependencies
    (mapc (bind #'dag-insert-edge /1 reduction dependencies) assumes)
    (or (gethash type type-to-reductions) ;; second, update the type index
	(setf (gethash type type-to-reductions) nil))
    (dfs (lambda (type)	;; and all subtype indices
	   (awhen (gethash type type-to-reductions)
	     (dag-order-insert reduction it dependencies)))
	 #'next-most-general-types type))
  (defun reductions (type)
    (or (gethash type type-to-reductions)
	(setf (gethash type type-to-reductions)
	      (delete-duplicates (mappend #'reductions 
					  (next-most-general-types type))))))
  (defun full-reduce (expr context type &aux (reductions (reductions type)))
    (labels ((reduce-subtypes (expr)
	       (cond ((atom expr) expr)
		     ((closurep (fn expr)) (mapargs #'reduce-subtypes expr))
		     (t (mapargs-with-types (bind #'full-reduce /1 context /2) 
					    expr 
					    (arg-types expr context type))))))
      (fixed-point (lambda (expr)
		     (reduce-subtypes 
		      (reduce (lambda (expr reduction)
				(if (atom expr)
				    (return-from full-reduce expr)
				    (funcall reduction expr)))
			      reductions :initial-value expr)))
		   expr))))
    
      
      
	


;; ,(if order
;; 					  `(funcall #',order #'call ,expr-name)
;; 					  `(call ,expr-name))))
;; 			 (if (not (eq result ,expr-name))
;; 			     (reduce-with ,cleanups result)
;; 			     result))
;; 		       ,expr-name))))))

;;      (dag-insert-node fn *reduction-prerequisites*) ; update the prerequisites
;;      ;(print "inserted node")(print fn)
;;      (dolist (prereq ,prerequisites)		    ; dag
;;       ; (print "adding edge")
;;        ;(print (symbol-function prereq))
;;        ;(print fn)
;;        (dag-insert-edge prereq fn *reduction-prerequisites*))
     
;;      (unless (gethash ',type *type-to-reductions*) ; make sure there's a type
;;        (setf (gethash ',type *type-to-reductions*) ; entry in the table
;; 	     (labels ((find-and-copy-hash (type)
;; 			(let ((next (next-most-general type)))
;; 			  (if next
;; 			      (aif (gethash next *type-to-reductions*)
;; 				   (copy-hash-table it)
;; 				   (find-and-copy-hash next))))))
;; 	       (find-and-copy-hash ',type))))
	   
;;      (maphash (lambda (t2 rules)       ; insert the rule in all matching type
;; 		(if (isa t2 ',type) ; entries, respecting prereq ordering
;; 		    (setf rules 
;; 			  (dag-order-insert fn rules 
;; 					    *reduction-prerequisites*))))
;; 	      *type-to-reductions*)
     
;;      fn)) ;returns the function for the reduction rule we've defined

;; reduce-with should iterate

(defmacro define-reduction 
    (name (&rest args) 
     &key (type t) assumes (condition t) action (order 'funcall) preserves 
     &aux (expr (if (= (length args) 1)
		    (car args)
		    (prog1 (gensym)
		      (setf condition `(dexpr ,it ,args ,condition))
		      (setf action `(dexpr ,it ,args ,action)))))
     (original (gensym)))
  (assert action () "action key required for a reduction")
  (assert (or (= (length args) 1) (= (length args) 3)) ()
	  "argument list for reduction must be have 1 or 3 element(s)")
  `(defun ,name (,expr &aux (,original ,expr))
     (setf ,expr (reduce-with ,assumes ,expr))
     (setf ,expr (,order (lambda ,expr
			   (if (and (isa (expr-type ,expr) ',type) ,condition)
			       ,action
			       ,expr))
			 ,tmp-expr))
     ,(unless (eq preserves all)
	      `(unless (eq ,expr ,original)
		 (update-simps (get-simp-markup (markup ,expr)) ,preserves)))
     ,expr))




  (unless (gethash ',type
  
     



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

(define-reduction eval-const (expr)
  :condition (and (not (matches (fn expr) (list tuple lambda)))
		  (purep (fn expr)))
  :action
  (cond ((commutativep fn)
	 (bind-collectors (constants others)
	     (mapc (lambda (arg) 
		     (if (const-value-p arg) (constants arg) (others arg)))
		   args)
	   (if others 
	       (if constants
		   (pcons fn 
			  (cons (peval (pcons fn constants) *empty-context*) 
				others)
			  markup)
		   expr)
	       (peval expr *empty-context*))))
	((every #'const-value-p args) (peval expr *empty-context*))
	(t expr))
  :order upwards)
