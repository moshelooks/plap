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
			(pcons (fn expr) 
			       (nconc (copy-range (args expr) args)
				      (ncons result)
				      (mapcar fn 
					      ,@(mapcar
						 (bind #'list 'cdr /1)
						 arg-names)))
			       (markup expr))))))
		(args expr) ,@types)
	  expr)))
  (mapargs-gen mapargs (args) nil)
  (mapargs-gen mapargs-with-types (args types) (types)))
(define-test mapargs
  (let ((expr %(and x y z)))
    (assert-eq expr (mapargs #'identity expr))))

(macrolet ((mkorder (ord-name transformer)
	     `(defun ,ord-name (rule name expr preserves)
		(if (and (consp expr) (not (simpp expr name)))
		    (aprog1 ,transformer
		      (when (and (not (eq it expr)) (not (eq 'all preserves)))
			(clear-simp expr preserves))
		      (when (consp it) (mark-simp it name)))
		    expr))))
  (mkorder apply-to (funcall rule expr))
  (mkorder upwards (funcall rule (mapargs 
				  (bind #'upwards rule name /1 preserves)
				  expr)))
  (mkorder downwards (mapargs 
		      (bind #'downwards rule name /1 preserves)
		      (funcall rule expr))))
(define-test upwards
  (let ((expr %(and x y z (or p d q))))
    (assert-eq expr (upwards #'identity 'identity expr nil))))

(defmacro reduce-from (fn reductions expr)
  `(reduce (lambda (expr reduction)
	     (if (atom expr) (return-from ,fn expr) (funcall reduction expr)))
	   ,reductions :initial-value ,expr))

(let ((type-to-reductions (make-hash-table))
      (reduction-to-assumes (make-hash-table))
      (dependencies (make-dag))
      (names-to-reductions (make-hash-table)))
  ;;; important note - register-reduction does not do any handling of markup,
  ;;; which must be created/removed by the reduction function (define-reduction
  ;;; can autogenerate this code
  (defun register-reduction (name type reduction assumes)
    (print* 'registering name)
    ;; get the actual reductions for all of the assumptions and update the map
    (setf assumes (mapcar (bind #'gethash /1 names-to-reductions) assumes))
    (setf (gethash name names-to-reductions) reduction)
    (dag-insert-node reduction dependencies) ;; then update dependencies
    (mapc (bind #'dag-insert-edge /1 reduction dependencies) assumes)
    (setf (gethash reduction reduction-to-assumes) assumes)
    (or (gethash type type-to-reductions) ;; then update the type index
	(setf (gethash type type-to-reductions) nil))
    ;; and all subtype indices
    (dfs (lambda (type)
	   (mvbind (list exists) (gethash type type-to-reductions)
	     (when exists 
	       (setf (gethash type type-to-reductions)
		     (dag-order-insert reduction list dependencies)))))
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
		     (t (mapargs-with-types (bind #'type-check /1 /2) expr 
					    (arg-types expr context type)))))
	     (type-check (subexpr subexpr-type)
	       (if (or (atom subexpr) (isa subexpr-type type))
		   subexpr
		   (full-reduce subexpr context subexpr-type))))
      (fixed-point (lambda (expr)
		     (reduce-subtypes (reduce-from full-reduce 
						   reductions expr)))
		   expr)))
  (defun reduciblep (expr context type)
    (labels ((subtypesp (expr)
	       (cond ((atom expr) nil)
		     ((closurep (fn expr)) (some #'subtypesp (args expr)))
		     (t (some (bind #'reduciblep /1 context /2)
			      (args expr) (arg-types expr context type))))))
      (or (some (lambda (rule) (not (eq (funcall rule expr) expr)))
		(reductions type))
	  (subtypesp expr))))
 ;;; returns the rules and their assumptions, sorted by dependency
  (defun integrate-assumptions (rule-names &aux assumptions)
    (print* 'integrating rule-names 'given dependencies)
    (dfs (lambda (rule)
	   (setf assumptions (dag-order-insert rule assumptions dependencies)))
	 (bind #'gethash /1 reduction-to-assumes)
	 :roots (mapcar (bind #'gethash /1 names-to-reductions) rule-names))
    assumptions))

(defmacro define-reduction 
    (name (&rest args) 
     &key (type t) assumes (condition t) action (order 'apply-to) preserves 
     &aux (assumes-calls (gensym)) 
     (has-decomp (ecase (length args) (3 t) (1 nil)))
     (expr (if has-decomp (gensym) (car args)))
     (call-body `(aif ,condition ,action ,expr))
     (order-call `(,order (lambda (,expr)
			    ,(if has-decomp 
				 `(dexpr ,expr ,args ,call-body)
				 call-body))
			  ',name ,expr
			  ,(if (eq preserves 'all)
			       ''all
			       (sort (copy-list preserves) #'string<)))))
  (assert action () "action key required for a reduction")
  `(let ((,assumes-calls (integrate-assumptions ',assumes)))
     (defun ,name (,expr)
       (setf ,expr (fixed-point (lambda (expr) 
				  (reduce-from ,name ,assumes-calls expr))
				,expr))
       ,order-call)
     (register-reduction ',name ,type (lambda (,expr) ,order-call) ',assumes)))

;;;; general-purpose reductions are defined here

(define-reduction sort-commutative (fn args markup)
  :condition (and (commutativep fn) (not (sortedp args #'total-order)))
  :action (pcons fn (sort (copy-list args) #'total-order) markup)
  :order upwards
  :preserves all)
(define-test sort-commutative
  (assert-equal '((and simp (sort-commutative)) 
		  x y z ((or simp (sort-commutative)) a b))
		(sort-commutative %(and y (or b a) z x)))
  (assert-equal '((foo simp (sort-commutative)) 
		  zaa baa ((or simp (sort-commutative)) a b))
		(sort-commutative %(foo zaa baa (or b a))))
  (let ((expr %(and x y z)))
    (assert-eq expr (sort-commutative expr))))
  
(define-reduction flatten-associative (fn args markup)
  :condition (and (associativep fn) (find fn args :key #'afn))
  :action (pcons fn
		 (mappend (lambda (arg) 
			    (if (eq (afn arg) fn) (args arg) `(,arg)))
			  args)
		 markup)
  :order upwards)
(define-test flatten-associative
  (assert-equal '((and simp (flatten-associative)) 
		  x y ((or simp (flatten-associative)) q w))
		(flatten-associative %(and x (and y (or q w))))))

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
  (assert-equal '((+ simp (eval-const)) 3 x ((* simp (eval-const)) 41 x))
		(eval-const %(+ x (* x 41 1) 1 2)))
  (assert-equal true (eval-const %(and (or) (or))))
  (assert-equal '((foo simp (eval-const)) 1 2 x 42)
		(eval-const %(foo 1 2 x (+ 2 40))))
  (assert-equal '((+ simp (eval-const)) 1 x) 
		(eval-const %(+ 1 -2 x 2))))
