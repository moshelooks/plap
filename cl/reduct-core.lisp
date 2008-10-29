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
		    (unless (eql result (car args))
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
	     `(defun ,ord-name (rule name expr preserves cleanup) fixme...
		(fixed-point
		 (lambda (expr)
		   (if (and (consp expr) (not (simpp expr name)))
		       (let ((res ,transformer))
			 (when (consp res)
			   (if (eql res expr)
			       (mark-simp expr name)
			       (progn (unless (eq 'all preserves) 
					(clear-simp res preserves))
				      (mark-simp res name)
				      (setf res (funcall cleanup res)))))
			 (print* 'rule-is name 'res-is res)
			 res)
		       expr))
		 expr))))
  (mkorder apply-to (labels ((rec (x) 
			       (when (and (consp x) (not (fully-reduced-p x)))
				 (mapc #'rec (args x))
				 (mark-simp x name))))
		      (aprog1 (funcall rule expr) (rec it))))
  (mkorder upwards (funcall rule (mapargs (bind #'upwards rule name /1
						preserves cleanup)
					  expr)))
  (mkorder downwards (mapargs (bind #'downwards rule name /1 preserves cleanup)
			      (aprog1 (funcall rule expr)
				      (when (atom it)
					(return-from downwards it))))))
(define-test upwards
  (let ((expr %(and x y z (or p d q))))
    (assert-eq expr (upwards #'identity 'identity expr nil #'identity))))

(defun apply-rules (rules expr)
  (reduce (lambda (x fn) 
	    (if (atom x) (return-from apply-rules x) (funcall fn x)))
	  rules :initial-value expr))

(defun reduce-range (begin end expr)
  (if (or (eq begin end) (atom expr)) expr 
      (reduce-range (cdr begin) end (funcall (car begin) expr))))


(defmacro reduce-from (fn reductions expr)
  `(progn 
     (mapl (lambda (end)
	     (setf ,expr (fixed-point (bind #'reduce-range ,reductions end /1)
				      ,expr))
	     (setf ,expr (if (atom ,expr) (return-from ,fn ,expr)
			     (funcall (car end) ,expr))))
	   ,reductions)
     ,expr))

(let ((type-to-reductions (make-hash-table :test 'equal))
      (reduction-to-assumes (make-hash-table))
      (dependencies (make-dag))
      (names-to-reductions (make-hash-table)))

  ;;; important note - register-reduction does not do any handling of markup,
  ;;; which must be created/removed by the reduction function (define-reduction
  ;;; can autogenerate this code
  (defun register-reduction (name type reduction assumes obviates)
    ;; get the actual reductions for all of the assumptions/obviations
    (setf assumes (mapcar (bind #'gethash /1 names-to-reductions) assumes))
    (setf obviates (mapcar (bind #'gethash /1 names-to-reductions) obviates))
    (setf (gethash name names-to-reductions) reduction)  ;; update the map
    (dag-insert-node reduction dependencies) ;; then update dependencies
    (mapc (bind #'dag-insert-edge /1 reduction dependencies) assumes)
    (setf (gethash reduction reduction-to-assumes) assumes)
    (reductions type)               ;; then update the type index
    (maphash (lambda (type2 list)   ;; and all subtype indices
	       (when (isa type2 type)
		 (setf (gethash type2 type-to-reductions)
		       (dag-order-insert reduction
					 (delete-if (bind #'member /1 obviates)
						    list)
					 dependencies))))
	     type-to-reductions))
  (defun reductions (type)
    (or (gethash type type-to-reductions)
	(setf (gethash type type-to-reductions)
	      (reduce (lambda (x y) (delete-duplicates (nconc x y)))
		      (if (function-type-p type)  ;;terribly inefficient..
			  (collecting (maphash (lambda (type2 rules)
						 (when (isa type type2)
						   (collect rules)))
					       type-to-reductions))
			  (mapcar #'reductions (next-most-general-types type)))
		      :key #'copy-list :initial-value nil))))
 ;;; returns the rules and their assumptions, sorted by dependency
  (defun integrate-assumptions (rule-names &aux assumptions)
    (dfs (lambda (rule)
	   (setf assumptions (dag-order-insert rule assumptions dependencies)))
	 (bind #'gethash /1 reduction-to-assumes)
	 :roots (mapcar (bind #'gethash /1 names-to-reductions) rule-names))
    assumptions))

(defun reduct (expr context type)
  (assert (not (canonp expr)) () "can't reduct canonized expr ~S" expr)
  (labels ((reduce-subtypes (expr)
	     (cond 
	       ((atom expr) expr)
	       ((closurep (fn expr)) (mapargs #'reduce-subtypes expr))
	       ((eq 'lambda (fn expr))
		(with-bound-types context (fn-args expr) (cadr type)
		  (let ((body (reduct (fn-body expr) context (caddr type))))
		    (if (eql body (fn-body expr)) expr 
			(mklambda (fn-args expr) body (markup expr))))))
	       (t (mapargs-with-types (lambda (x type2)
					(if (or (atom x) (equal type type2)) x
					    (reduct x context type2)))
				      expr (arg-types expr context type))))))
    (if (or (atom expr) (eq (car (mark simp expr)) fully-reduced)) expr
	(aprog1 (cummulative-fixed-point 
		 (cons #'reduce-subtypes (reductions type)) expr)
	  (push fully-reduced (mark simp it))))))
(define-test reduct
  (with-bound-types *empty-context* '(f g) 
      '((function (num num) bool) (function (bool) num))
    (let* ((expr (copy-tree %(and (f 42 (+ (g (or a b)) m)) (or x y))))
	   (r (reduct expr *empty-context* bool))
	   (bool-subexprs (list r (arg0 r) (arg1 r)
				(arg0 (arg0 (arg1 (arg0 r))))))
	   (num-subexprs (list (arg1 (arg0 r)) (arg0 (arg1 (arg0 r)))))
	   (subexprs (append bool-subexprs num-subexprs)))
	     
      (assert-equal (p2sexpr expr) (p2sexpr r))
      (assert-equal expr r)
      (assert-eq expr r)
      (assert-for-all (bind #'exact-simp-p /1 'flatten-associative) subexprs)
      
      (assert-for-all (bind #'exact-simp-p /1 'push-nots) bool-subexprs)
      (assert-for-all (bind #'exact-simp-p /1 'sort-commutative) bool-subexprs)
      (assert-for-none (bind #'exact-simp-p /1 'maxima-reduce) bool-subexprs)

      (assert-for-none (bind #'exact-simp-p /1 'push-nots) num-subexprs)
      (assert-for-none (bind #'exact-simp-p /1 'sort-commutative) num-subexprs)
      (assert-for-all (bind #'exact-simp-p /1 'maxima-reduce) num-subexprs))))

;; for convenience
(defun qreduct (expr) 
  (if (atom expr) expr 
      (reduct expr *empty-context* (expr-type expr *empty-context*))))

(defun reductsp (expr context type)
  (labels ((subtypesp (expr)
	     (cond ((atom expr) nil)
		   ((closurep (fn expr)) (some #'subtypesp (args expr)))
		   (t (some (bind #'reduciblep /1 context /2)
			    (args expr) (arg-types expr context type))))))
    (or (some (lambda (rule) (not (eql (funcall rule expr) expr)))
	      (reductions type))
	(subtypesp expr))))

(defmacro define-reduction (name &rest dr-args)
  (dbind ((&rest args) &key (type t) assumes obviates (condition t)
	  action (order 'apply-to) preserves 
	  &aux (assumes-calls (gensym)) 
	  (has-decomp (ecase (length args) (3 t) (1 nil)))
	  (expr (if has-decomp (gensym) (car args)))
	  (ccore `(aif ,condition ,action ,expr))
	  (call-body (if has-decomp `(dexpr ,expr ,args ,ccore) ccore))
	  (preserves-list (if (eq preserves 'all) 'all
			      (sort (copy-list preserves) #'string<)))
	  (cleanup `(lambda (expr) 
		      (cummulative-fixed-point ,assumes-calls expr)))
	  (order-call `(,order (lambda (,expr) ,call-body)
			       ',name ,expr ',preserves-list ,cleanup)))dr-args
    (assert action () "action key required for a reduction")
    `(let ((,assumes-calls (integrate-assumptions ',assumes)))
       (defun ,name (,expr)
	 (setf ,expr (fixed-point (lambda (expr) 
				    (reduce-from ,name ,assumes-calls expr))
				  ,expr))
	 ,order-call)
       (register-reduction ',name ',type 
			   (lambda (,expr) (block ,name ,order-call)) 
			   ',assumes ',obviates))))))
(define-test reduction-registry
  (assert-equal (remove-duplicates (mapcar #'car *reduction-registry*))
		(mapcar #'car *reduction-registry*)))
