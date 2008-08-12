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

(defun canonize-children (expr context type)
  (if (atom expr) expr
      (cons (car expr) (mapcar (bind #'canonize /1 context /2)
			       (cdr expr) (arg-types expr context type)))))

(defdefbytype defcanonizer canonize)
(defun qcanonize (expr) ;;;useful for testing
  (canonize expr *empty-context* (expr-type expr *empty-context*)))

(defun structure-bool (op children context)
  (cons
   (list op)
   (mapcar (lambda (expr)
	     (cons op (decompose-bool expr
			(literal (ncons expr))
			(junctor (structure-bool (bool-dual op) (cdr expr)
						 context))
			(t (ncons (canonize-children expr context 'bool))))))
	   children)))
(defcanonizer bool (expr context &aux 
		    (op (cond ((junctorp expr) (car expr))
			      ((eq expr true) 'or)
			      (t 'and)))
		    (dual (bool-dual op)))
  `(,dual (,op) ,(cons op 
		       (decompose-bool expr
			 (literal (ncons expr))
			 (constant nil)
			 (junctor (structure-bool dual (cdr expr) context))
			 (t (ncons (canonize-children expr context 'bool)))))))
(define-test canonize-bool
  (assert-equal '(or (and) (and)) (qcanonize false))
  (assert-equal '(and (or) (or)) (qcanonize true))
  (assert-equal '(or (and) (and x1))
		(canonize 'x1 *empty-context* 'bool))
  (assert-equal '(or (and) (and (or) (or x1) (or (not x4)))) 
		(qcanonize '(and x1 (not x4))))
  (assert-equal '(and (or) (or (and) (and x1) (and (not x4))))
		(qcanonize '(or x1 (not x4))))
  (assert-equal  '(and (or) (or (and) (and (or) (or x1) (or x2)) (and x3)))
		 (qcanonize '(or (and x1 x2) x3))))

(defcanonizer num (expr context &aux 
		   (ops '(exp log sin)) (op-offsets '(0 1 0)))
  (labels
      ((sub-product (op offset)
	 (list '* 0 (list op
			  (nconc (list '+ offset)
				 (mapcar 
				  (bind #'list '* 0 (list /1 (list '+ /2)))
				  ops op-offsets)))))
       (dual-assemble (op ops-terms splitter builder o ws ts &optional top)
	 (nconc (list op o) ops-terms
		(cond 
		  ((or top (longerp ws 1))
		   (nconc (list (funcall builder (identity-elem op) nil nil))
			  (mapcar (lambda (weight term)
				    (mvbind (o ws ts) (funcall splitter term)
				      (declare (ignore o))
				      (funcall builder weight ws ts)))
				  ws ts)))
		  (ws (list 
		       (list (dual-num-op op) (car ws)
			     (canonize-children (car ts) context 'num)))))))
       (sum-of-products (o ws ts &optional top)
	 (dual-assemble 
	  '+ (mapcar #'sub-product ops op-offsets)
	  #'split-product-of-sums #'product-of-sums o ws ts top))
       (product-of-sums (o ws ts &optional top)
	 (dual-assemble 
	  '* (collecting
	       (mapc (lambda (op offset)
		       (unless (find op ts :key #'acar)
			 (collect (list '+ 1 (sub-product op offset)))))
		     ops op-offsets))
	  #'split-sum-of-products #'sum-of-products o ws ts top)))
    (dbind (splitter builder)
	(if (and (eq (acar expr) '+)
		 (longerp expr (if (numberp (cadr expr)) 3 2)))
	    `(,#'split-product-of-sums ,#'product-of-sums)
	    `(,#'split-sum-of-products ,#'sum-of-products))
      (mvbind (o ws ts) (funcall splitter expr)
	(funcall builder o ws ts t)))))

(define-test canonize-numeric
  (let* ((exp-block '(* 0 (exp (+ 0
				(* 0 (exp (+ 0)))
				(* 0 (log (+ 1)))
				(* 0 (sin (+ 0)))))))
	 (log-block '(* 0 (log (+ 1
				(* 0 (exp (+ 0)))
				(* 0 (log (+ 1)))
				(* 0 (sin (+ 0)))))))
	 (sin-block '(* 0 (sin (+ 0
				(* 0 (exp (+ 0)))
				(* 0 (log (+ 1)))
				(* 0 (sin (+ 0)))))))
	 (add-blocks `(,exp-block ,log-block ,sin-block))
	 (mult-block `(* 0
			 (+ 1 ,exp-block)
			 (+ 1 ,log-block)
			 (+ 1 ,sin-block))))
    (assert-equal `(or (and) (and (< (+ 2 ,@add-blocks ,mult-block) 
				     (+ 3 ,@add-blocks ,mult-block))))
		  (qcanonize '(< 2 3)))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block)
		  (qcanonize 0))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block
		      (* 1 ,@(cddr mult-block) (+ 0 x)))
		  (canonize 'x *empty-context* 'num))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block
		      (* 1
			 (+ 1 ,exp-block)
			 (+ 1 ,log-block)
			 (+ 0 (sin (+ 0 ,@add-blocks ,mult-block
				      (* 1 ,@(cddr mult-block) (+ 0 x)))))))
		  (qcanonize '(sin x)))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block
		      (* 1 
			 ,@(cddr mult-block) 
			 (+ 1 ,@add-blocks)
			 (+ 0 ,@add-blocks (* 1 x))
			 (+ 0 ,@add-blocks (* 1 y))))
		  (qcanonize '(* x y)))
    (assert-equal `(* 1 ,@(cddr mult-block) (+ 1 ,@add-blocks)
		      (+ 0 ,@add-blocks ,mult-block
			 (* 1 ,@(cddr mult-block) (+ 0 x))
			 (* 1 ,@(cddr mult-block) (+ 0 y))))
		  (qcanonize '(+ x y)))
    (assert-equal `(+ 1 ,@add-blocks ,mult-block
		      (* 1 ,@(cddr mult-block) (+ 0 x)))
		  (qcanonize '(+ 1 x)))
    (assert-equal `(* 1 ,@(cddr mult-block) (+ 1 ,@add-blocks)
		      (+ 1 ,@add-blocks ,mult-block
			 (* 1 ,@(cddr mult-block) (+ 0 x))
			 (* 1 ,@(cddr mult-block) (+ 0 y))))
		  (qcanonize '(+ 1 x y)))
    ;; f is a (function (bool (list bool)) num)
    (let ((lhs `(+ 0 ,@add-blocks ,mult-block
		   (* 1 ,@(cddr mult-block) 
		      (+ 0 (split 
			    (tuple ,(canonize 'l *empty-context*
					      '(list bool))
				   ,(qcanonize true))
			    (lambda (? ?)
			      (+ 0 ,@add-blocks ,mult-block
				 (* 1 ,@(cddr mult-block) 
				    (+ 0 (f (or (and) (and ?))
					    (append 
					     (if false 
						 (list (or (and) (and)))
						 nil)
					     (if true ? nil)
					     (if false
						 (list (or (and) (and)))
						 nil))))))))))))
	  (rhs (canonize '(split (tuple l true) f)
			 (init-context '((l (list true))))
			 'num)))
      (assert-true (tree-matches lhs rhs) lhs rhs))))
		   

(defcanonizer tuple (expr context type)
  (decompose-tuple expr
    (tuple (cons 'tuple (mapcar (bind #'canonize /1 context /2)
				(cdr expr) (cdr type))))
    (t (canonize-children expr context type))))
(define-test canonize-tuple
  (assert-equal '(tuple) (qcanonize '(tuple)))
  (assert-equal `(tuple (and (or) (or)) ,(qcanonize 42))
		(qcanonize '(tuple true 42))))

(defun structure-list (list elem context type)
  (cons 'append
	(interleave elem (mapcar (lambda (x) 
				   `(if true 
					,(canonize-children x context type) 
					nil))
				 list))))

(defcanonizer list (expr context type)
  (let* ((subtype (cadr type))
	 (elem `(if false
		    (list ,(canonize (default-value subtype) context subtype))
		    nil)))
    (structure-list (decompose-list expr (append (cdr expr)) (t `(,expr)))
		    elem context type)))

(define-test canonize-list
  (assert-equal `(append (if false (list ,(qcanonize 0)) nil)
			 (if true (list ,(qcanonize 42)) nil)
			 (if false (list ,(qcanonize 0)) nil))
		(qcanonize '(list 42))))

(defcanonizer function (expr context type)
  (cons 
   'lambda
   (dbind (arg-types return-type) (cdr type)
     (decompose-function expr
       (lambda
	(dbind (arg-names body) (cdr expr)
	  ; first, bind arg names to their types in the context
	  (mapcar (bind #'bind-type context /1 /2) arg-names arg-types)
	  (prog1
	      (list arg-names		; now build the body of the lambda
		    (if (find-if #'list-type-p arg-types)
			(cons 'split 
			      (if (not (eq (acar body) 'split))
				  `((tuple) (lambda () 
					      ,(canonize body context 
							 return-type)))
				  (dbind (tuple function) (cdr body)
				    (dbind (ttype ftype)
					(arg-types body context return-type)
				      `(,(canonize tuple context ttype)
					,(canonize function context ftype))))))
			(canonize body context return-type)))
	    (mapcar (bind #'unbind-type context /1) arg-names))))
       (t (let ((arg-names (mapcar #'genname arg-types)))
	    (assert (not (consp expr)))
	    (list arg-names 
		  (let ((subexpr (cons expr
				       (mapcar (bind #'canonize /1 context /2)
					       arg-names arg-types))))
		    (if arg-names
			(nsubst subexpr (car arg-names)
				(canonize (car arg-names) 
					  context return-type))
			subexpr)))))))))

(define-test canonize-function
  (assert-equal '(lambda (l x) (split (tuple) (lambda () 
						(or (and) (and x)))))
		(canonize '(lambda (l x) x) *empty-context*
			  '(function ((list bool) bool) bool)))
  (assert-equal `(lambda (first rest)
		   (split (tuple) (lambda ()
				    (or (and) (and (or) (or first)
						   (or x))))))
		(canonize '(lambda (first rest) (and first x))
			  *empty-context* '(function (bool (list bool))
					    bool)))
  (assert-equal `(lambda (l x)
		  (split (tuple ,(canonize 'l *empty-context* '(list bool))
				(and (or) (or)))
			 (lambda (first rest)
			   (split (tuple) (lambda ()
					    (or (and) (and (or) (or first)
							   (or x))))))))
		(canonize '(lambda (l x)
			    (split (tuple l true)
			     (lambda (first rest) (and first x))))
			  *empty-context* '(function ((list bool) bool)
					    bool))))
