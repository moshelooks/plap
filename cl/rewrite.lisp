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

(macrolet 
    ((mapargs-gen (name arg-names types)
       `(defun ,name (fn expr ,@types)
	  (mapl (lambda ,arg-names
		  (let ((result (funcall fn ,@(mapcar (bind #'list 'car /1)
						      arg-names))))
		    (unless (eq result (car args))
		      (return-from ,name
			(values (pcons (fn expr) 
				       (nconc (copy-range (args expr) args)
					      (ncons result)
					      (mapcar #'fn 
						      ,@(mapcar
							 (bind #'list 'cdr /1)
							 arg-names)))
				       (markup expr))
				t)))))
		(args expr) ,@types)
	  (values expr nil))))
  (mapargs-gen mapargs (args) nil)
  (mapargs-gen mapargs-with-types (args types) (types)))

(defun apply-to (rule name expr preserves)
  (when (not (simpp expr name))
    (aprog1 (funcall rule expr)
      (when (and (not (eq it expr)) (not (eq 'all preserves)))
	(clear-simp expr preserves))
      (mark-simp it name))))

(defun upwards (rule name expr preserves)
  (when (not (simpp expr name))
    (aprog1 (funcall rule 
		     (mapargs (bind #'upwards rule name /1 preserves) expr))
      (when (not (eq it expr))
	(clear-simp it preserves)))))
	     

;;      (update (if (eq preserves 'all)
;; 		 `(remove-simp ',name ,expr)
;; 		 `(remove-ordered-simps 
;; 		   ,(sort (list ',name ,@preserves) #'string<) ,expr)))
;;      (reduce-and-update 
;;       `((setf ,expr (,order (lambda ,expr
;; 			     (if (and (isa (expr-type ,expr) ',type)
;; 				      ,condition)
;; 				 ,action
;; 				 ,expr))
;; 			    ,expr)
;; 			   ,tmp-expr))
;; 	(unless (eq ,expr ,tmp-expr)


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


				     

;; (defun downwards (rule expr)
;;   (if (consp expr)
;;       (let ((result (funcall rule expr)))
;; 	(if (consp result)
;; 	    (let ((subresults (mapcar (bind #'downwards rule /1) 
;; 				      (cdr result))))
;; 	      (if (and (eq result expr) (every #'eq subresults (cdr expr)))
;; 		  expr
;; 		  (cons (car result) subresults)))
;; 	    result))
;;       expr))


;; under what circumstances will a reduction need a context argument??


;;   (let ((result (blockn (reduce (lambda (expr rule) 
;; 				  (if (consp expr)
;; 				      (funcall rule expr)
;; 				      (return expr)))
;; 				rules :initial-value expr))))
;;     (if (equalp expr result) expr (reduce-with rules result))))


(defmacro reduce-from (fn reductions expr)
  `(reduce (lambda (expr reduction)
	     (if (atom expr) (return-from ,fn expr) (funcall reduction expr)))
	   ,reductions :initial-value ,expr))

(let ((type-to-reductions (make-hash-table))
      (reduction-to-assumes (make-hash-table))
      (dependencies (make-dag)))
  ;;; important note - register-reduction does not do any handling of markup,
  ;;; which must be created/removed by the reduction function (define-reduction
  ;;; can autogenerate this code
  (defun register-reduction (type reduction assumes)
    (dag-insert-node reduction dependencies) ;; first, update dependencies
    (mapc (bind #'dag-insert-edge /1 reduction dependencies) assumes)
    (setf (gethash reduction reduction-to-assumes) assumes)
    (or (gethash type type-to-reductions) ;; second, update the type index
	(setf (gethash type type-to-reductions) nil))
    (dfs (lambda (type)	;; and all subtype indices
	   (awhen (gethash type type-to-reductions)
	     (setf (gethash type type-to-reductions)
		   (dag-order-insert reduction it dependencies))))
	 #'next-most-general-types :root type))
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
		     (reduce-subtypes (reduce-from full-reduce 
						   reductions expr)))
		   expr)))
 ;;; returns the rules and their assumptions, sorted by dependency
  (defun integrate-assumptions (rules &aux assumptions)
    (dfs (lambda (rule)
	   (setf assumptions (dag-order-insert rule assumptions dependencies)))
	 (bind #'gethash /1 reduction-to-assumes) :roots rules)
    assumptions))

(defmacro define-reduction 
    (name (&rest args) 
     &key (type t) assumes (condition t) action (order 'apply-to) preserves 
     &aux (expr (if (= (length args) 1)
		    (car args)
		    (aprog1 (gensym)
		      (setf condition `(dexpr ,it ,args 
					 (declare (ignorable ,@args))
					 ,condition))
		      (setf action `(dexpr ,it ,args 
				      (declare (ignorable ,@args))
				      ,action)))))
     (assumes-calls (integrate-assumptions assumes))
     (order-call (list order action expr
		       (if (eq preserves 'all) 
			   'all
			   (sort (copy-list preserves) #'string<)))))
  (assert action () "action key required for a reduction")
  (assert (or (= (length args) 1) (= (length args) 3)) ()
	  "argument list for reduction must be have 1 or 3 element(s)")
  (setf condition `(and (not (simpp ,expr ,name)) ,condition))
  `(progn
     (defun ,name (,expr)
       (setf ,expr (fixed-point (lambda (expr) 
				  (reduce-from ,name ,assumes-calls expr))
				,expr))
       (when (and (consp ,expr) (isa (expr-type ,expr) ',type) ,condition)
	 ,order-call))
     (register-reduction ,type (lambda (,expr) (when ,condition ,order-call))
			 ,assumes)))

;;;; general-purpose reductions are defined here

(define-reduction sort-commutative (fn args markup)
  :condition (and (commutativep fn) (not (sortedp args #'total-order)))
  :action (pcons fn (sort (copy-list args) #'total-order) markup)
  :order upwards
  :preserves all)
(define-test sort-commutative
  (assert-equal %(and x y z (or a b)) 
		(sort-commutative %(and y (or b a) z x)))
  (assert-equal %(foo zaa baa (or a b))
		(sort-commutative %(foo zaa baa (or b a)))))
  
(define-reduction flatten-associative (fn args markup)
  :condition (and (associativep fn) (find fn args :key #'afn))
  :action (pcons fn
		 (mappend (lambda (arg) 
			    (if (eq (afn arg) fn) (args arg) `(,arg)))
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
  (dexpr expr (fn args markup)
    (cond ((and (identityp fn) (unary-expr-p expr)) (arg0 expr))
	  ((commutativep fn)
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
	  (t expr)))
  :order upwards)
(define-test eval-const
  (assert-equal 42 (eval-const %(+ 1 (* 1 41))))
  (assert-equal 42 (eval-const %(funcall (lambda (x) (+ x (* x 41))) 1)))
  (assert-equal %(foo 42)
		(eval-const %(foo (funcall (lambda (x) (+ x (* x 41))) 1))))
  (assert-equal %(+ 1 x) (eval-const %(+ 1 -2 x 2))))
