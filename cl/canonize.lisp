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

;; for convenience in constructing canonized expressions
(defun canonize-from-template (template values)
  (if (or (atom template) (consp (car template)))
      (progn (assert (not values) () "for template ~S got invalid values ~S"
		     template values)
	     template)
      (ccons (car template)
	     (mapcar #'canonize-from-template (cdr template) 
		     (pad (cdr values) (length template)))
	     (car values))))

;; cons with args in canonical form
(defun canonize-args (expr context type)
;  (print* 'ca expr context type)
  (if (atom expr) expr
      (ccons (fn expr)
	     (mapcar (bind #'canonize /1 context /2)
		     (args expr) (arg-types expr context type))
	     expr)))

;; copies a canonical-form expr, the copied expr will have nil as its parent
(defun copy-canon (expr)
  (if (atom expr) expr
      (ccons (fn expr) (mapcar #'copy-canon (args expr)) (canon-expr expr))))


(defdefbytype defcanonizer canonize)
(defun qcanonize (expr) ;;;useful for testing
  (canonize expr *empty-context* (expr-type expr *empty-context*)))

(defun map-subexprs-with-type-and-parent 
    (fn expr &optional (context *empty-context*)
     (type (expr-type expr context)) parent)
  (funcall fn expr type parent)
  (if (lambdap expr)
      (with-bound-types context (fn-args expr) (cadr type)
	(unless (atom (fn-body expr))
	  (map-subexprs-with-type-and-parent
	   fn (fn-body expr) context (caddr type) expr)))
      (mapc (lambda (arg type) 
	      (unless (atom arg)
		(map-subexprs-with-type-and-parent fn arg context type expr)))
	    (args expr) (arg-types expr *empty-context* type))))
;; useful for testing - note that to compile under sbcl these must be macros
;; rather than functions, else we get the dreaded "Objects of type FUNCTION
;; can't be dumped into fasl files." error...
(defmacro validate-canonize (target expr &optional (type `(expr-type expr))
			     (context *empty-context*))
  `(let* ((target ,target) (expr ,expr) (type ,type) (context ,context))
    (map-subexprs-with-type-and-parent 
     (lambda (cexpr type parent)
	(assert-true (consp cexpr))
	(assert-true (mark canon cexpr))
	(assert-equalp (p2sexpr (reduct (sexpr2p (p2sexpr cexpr))
					context type))
		       (p2sexpr (canon-expr cexpr))
		       cexpr)
	(assert-eq parent (canon-parent cexpr)))
     expr context type nil)
     (assert-equal (if (consp (acar target)) (p2sexpr target) target)
		   (p2sexpr expr)
		   target expr)))

(defcanonizer bool (expr context)
  (labels ((substructure (op expr dual)
	     (ccons op
		    (decompose-bool expr
		      (literal (list (if (atom expr) expr
					 (ccons 'not (list (arg0 expr))
						expr))))
		      (const nil)
		      (junctor (structure dual (args expr)))
		      (t (list (canonize-args expr context 'bool))))
		    expr))
	   (structure (op args &aux (dual (bool-dual op)))
	     (cons (ccons op nil (identity-elem dual))
		   (mapcar (bind #'substructure op /1 dual) args))))
    (let* ((op (if (matches (ifn expr) (true or)) 'or 'and))
	   (dual (bool-dual op)))
      (ccons dual 
	     (list (ccons op nil (identity-elem dual)) 
		   (substructure op expr dual))
	     expr))))
(define-test canonize-bool
  (validate-canonize %(or (and) (and)) (qcanonize false))
  (validate-canonize %(and (or) (or)) (qcanonize true))
  (validate-canonize %(or (and) (and x1))
		     (canonize 'x1 *empty-context* 'bool))
  (validate-canonize %(or (and) (and (not x))) (qcanonize %(not x)))
  (validate-canonize %(or (and) (and (or) (or x1) (or (not x4)))) 
		     (qcanonize %(and x1 (not x4))))
  (validate-canonize %(and (or) (or (and) (and x1) (and (not x4))))
		     (qcanonize %(or x1 (not x4))))
  (validate-canonize  
   %(and (or) (or (and) (and x3) (and (or) (or x1) (or x2))))
   (qcanonize %(or x3 (and x1 x2))))
  (validate-canonize
   %(and (or) (or (and) (and (not z)) (and (or) (or x) (or y))))
   (qcanonize %(or (not z) (and x y)))))

(define-constant +num-canonical-ops+ nil);'(exp log sin))
(define-constant +num-canonical-offsets+ '(0 1 0))
(define-constant +num-canonical-values+ 
  (mapcar #'funcall +num-canonical-ops+ +num-canonical-offsets+))

#| Numerical canonization is ... complicated. First let's consider the simpler
case of nested products-of-sums-of-products-of..., disallowing log, exp, and
sin. This is analagous to the canonization of Boolean
formulae (conjunctions-of-disjunctions-of-conjunctions-of....) - the main
difference is that every variable that appears has both and additive *and* and
multiplicative constant associated with it. When these are not present in the
source formula there zero and one used as appropriate.

So, for example, (+ c x y) -> (* 1 (+ 1) (+ c (* 0) (* 1 (+ 0 x))
                                                    (* 1 (+ 0 y))))

|#
(defcanonizer num (expr context)
 (labels ((nccons (op args at)
	    (unless (numberp (car args)) 
	      (push (identity-elem (if args op (num-dual op))) args))
	    (ccons op args at))
	  (substructure (op expr dual)
	    (nccons op 
		    (cond ((ring-op-p (afn expr)) (structure dual (args expr)))
			  ((numberp expr) (list expr))
			  (t (list (canonize-args expr context 'num))))
		    expr))
	  (structure (op args &aux (dual (num-dual op)))
	    (cons (nccons op nil (identity-elem dual))
		  (mapcar (bind #'substructure op /1 dual) args))))
    (let* ((op (if (eq (ifn expr) '+) '+ '*))
	   (dual (num-dual op)))
      (nccons dual
	      (list (nccons op nil (identity-elem dual))
		    (substructure op expr dual))
	      expr))))


;; (defcanonizer num (expr context)
;;   (labels
;;       ;; -> (* 0 (op (+ offset (* 0 (exp (+ 0)))
;;       ;;                       (* 0 (log (+ 1)))
;;       ;;                       (* 0 (sin (+ 0))))))
;;       ((sub-product (op offset value)
;; 	 ~((* 0 (,op (+ ,offset ,@(mapcar (lambda (op offset value)
;; 					    ~((* 0 (,op (+ ,offset)))
;; 					      (0 nil (,value (,offset)))))
;; 					  +num-canonical-ops+
;; 					  +num-canonical-offsets+
;; 					  +num-canonical-values+))))
;; 	   (0 nil (,value (,offset)))))
;;      (dual-assemble (at op ops-terms splitter builder o ws ts &optional top)
;; 	 (print* 'o-is o 'ws-are ws 'ts-are ts 'top-is top)
;; 	 (assert (eql (length ws) (length ts)))
;;        ~((,op ,o ,@ops-terms 
;; 	      ,@(when (or top (and ws (cdr ws)))
;; 		  (list (funcall builder nil (identity-elem op) nil nil)))
;; 	      ,@(mapcar (lambda (at weight term)
;; 			  (mvbind (o ws ts) (funcall splitter term)
;; 	    (assert (equalp o (identity-elem (dual-num-op op)))y)
;; 			    (funcall builder at weight ws ts)))
;; 			(if (eq (afn at) op)
;; 			    (if (numberp (arg0 at)) (cdr (args at)) (args at))
;; 			    (list at))
;; 			ws ts))
			  
			
;; ;; 	      ,@(cond 
;; ;; 		   ((or top (longerp ws 1)) 
		    
;; ;; 		    (cons (funcall builder nil (identity-elem op) nil nil)
;; ;; 			  (when ws
;; ;; 			    (mapcar (lambda (at weight term)
;; ;; 				      (mvbind (o ws ts) (funcall splitter term)
;; ;; 					(declare (ignore o))
;; ;; 					(funcall builder at weight ws ts)))
;; ;; 				    (if (eq (afn at) op)
;; ;; 					(if (numberp (arg0 at))
;; ;; 					    (cdr (args at))
;; ;; 					    (args at))
;; ;; 					(list at))
;; ;; 				    ws ts))))
;; ;; 		   (ws (let ((dual (dual-num-op op)))
;; ;; 			 (print* 'ws-arg ws ts)
;; ;; 			 (list ~((,dual ,(car ws)
;; ;; 					,(canonize-args (car ts) context 'num))
;; ;; 				 (,(if (eq (car ws) (identity-elem dual))
;; ;; 				       (car ts)
;; ;; 				       (pcons dual (append ws ts))))))))
;; ;; 		   (t (progn (print 'meep) nil))))
;; 	 (,(or at (funcall op o)))))
;;        (sum-of-products (expr o ws ts &optional top)
;; 	 (dual-assemble
;; 	  expr '+ (mapcar #'sub-product +num-canonical-ops+
;; 			  +num-canonical-offsets+ +num-canonical-values+)
;; 	  #'split-product-of-sums #'product-of-sums o ws ts top))
;;        (product-of-sums (expr o ws ts &optional top)
;; 	 (dual-assemble 
;; 	  expr '* (collecting
;; 		    (mapc (lambda (op offset value)
;; 			    (unless (find op ts :key #'afn)
;; 			      (collect ~((+ 1 ,(sub-product op offset value))
;; 					 (1 nil nil)))))
;; 			  +num-canonical-ops+ +num-canonical-offsets+ 
;; 			  +num-canonical-values+))
;; 	  #'split-sum-of-products #'sum-of-products o ws ts top)))
;;     (dbind (splitter builder)
;; 	(if (and (eq (afn expr) '+)
;; 		 (longerp (args expr) (if (numberp (arg0 expr)) 2 1)))
;; 	    `(,#'split-product-of-sums ,#'product-of-sums)
;; 	    `(,#'split-sum-of-products ,#'sum-of-products))
;;       (mvbind (o ws ts) (funcall splitter expr)
;; 	(funcall builder expr o ws ts t)))))

(define-test canonize-num
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
    (validate-canonize `(+ 0 ,@add-blocks ,mult-block)
		       (qcanonize 0))
    (validate-canonize `(+ 2 ,@add-blocks ,mult-block)
		       (qcanonize 2))
    (validate-canonize `(or (and)
			    (and (< (+ 2 ,@add-blocks ,mult-block) 
				    (+ 0 ,@add-blocks ,mult-block
				       (* 1 ,@(cddr mult-block) (+ 0 x))))))
		       (canonize %(< 2 x) (init-context '((x 0))) bool)
		       bool (init-context '((x 0))))
    (validate-canonize `(+ 0 ,@add-blocks ,mult-block
			   (* 1 ,@(cddr mult-block) (+ 0 x)))
		       (canonize 'x *empty-context* num))
    (validate-canonize 
     `(+ 0 ,@add-blocks ,mult-block
	 (* 1 (+ 1 ,exp-block) (+ 1 ,log-block)
	    (+ 0 (sin (+ 0 ,@add-blocks ,mult-block
			 (* 1 ,@(cddr mult-block) (+ 0 x)))))))
     (qcanonize %(sin x)))
    (validate-canonize `(+ 0 ,@add-blocks ,mult-block
			   (* 1 
			      ,@(cddr mult-block) 
			      (+ 1 ,@add-blocks)
			      (+ 0 ,@add-blocks (* 1 x))
			      (+ 0 ,@add-blocks (* 1 y))))
		       (qcanonize %(* x y)))
    (validate-canonize `(* 1 ,@(cddr mult-block) (+ 1 ,@add-blocks)
			   (+ 0 ,@add-blocks ,mult-block
			      (* 1 ,@(cddr mult-block) (+ 0 x))
			      (* 1 ,@(cddr mult-block) (+ 0 y))))
		       (qcanonize %(+ x y)))
    (validate-canonize `(+ 1 ,@add-blocks ,mult-block
			   (* 1 ,@(cddr mult-block) (+ 0 x)))
		       (qcanonize %(+ 1 x)))
    (validate-canonize `(* 1 ,@(cddr mult-block) (+ 1 ,@add-blocks)
			   (+ 1 ,@add-blocks ,mult-block
			      (* 1 ,@(cddr mult-block) (+ 0 x))
			      (* 1 ,@(cddr mult-block) (+ 0 y))))
		       (qcanonize %(+ 1 x y)))
    (let ((lhs `(+ 0 ,@add-blocks ,mult-block
		   (* 1 ,@(cddr mult-block) 
		      (+ 0 (split 
			    (lambda (? ?)
			      (split 
			       (lambda ()
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
						    nil)))))))))
			    (tuple ,(p2sexpr (canonize 'l *empty-context*
						       '(list bool)))
				   ,(p2sexpr (qcanonize true))))))))
	  (rhs (p2sexpr
		(with-bound-types *empty-context* '(l f) 
		    '((list bool) (function (bool (list bool)) num))
		  (canonize %(split f (tuple l true)) *empty-context* 'num)))))
      (assert-true (tree-matches lhs rhs) lhs rhs))))

(defcanonizer tuple (expr context type)
  (decompose-tuple expr
    (tuple (ccons 'tuple 
		  (mapcar (bind #'canonize /1 context /2) 
			  (args expr) (cdr type))
		  expr))
    (t (canonize-args expr context type))))
(define-test canonize-tuple
  (validate-canonize '(tuple) (qcanonize %(tuple)))
  (validate-canonize (pcons 'tuple (list %(and (or) (or)) (qcanonize 42)))
		     (qcanonize %(tuple true 42))))

;; list is a list of (list type)s
(defun structure-list (expr list elem context type)
  (flet ((sub-structure (x) 
	   (ccons 'if (list true (canonize-args x context type) nil) x)))
    (ccons 'append 
	   (if expr
	       (interleave elem (mapcar #'sub-structure list) #'copy-canon)
	       (list elem (copy-canon elem)))
	   expr)))
(defcanonizer list (expr context type &aux (subtype (cadr type))
		    (default (default-expr subtype)))
  (structure-list expr 
		  (decompose-list expr (append (args expr)) (t (list expr)))
		  ~((if false (list ,(canonize default context subtype)) nil)
		    (nil nil (,(pcons 'list (list default)))))
		  context type))

(define-test canonize-list
  (validate-canonize `(append (if false (list ,(p2sexpr (qcanonize 0))) nil)
			      (if true (list ,(p2sexpr (qcanonize 42))) nil)
			      (if false (list ,(p2sexpr (qcanonize 0))) nil))
		(qcanonize %(list 42))))

(defcanonizer function (expr context type &aux arg-names body)
  (dbind (arg-types return-type) (cdr type)
    (if (lambdap expr)
	(setf arg-names (fn-args expr)
	      body (fn-body expr))
	(setf arg-names (mapcar #'genname arg-types)
	      body (pcons expr arg-names)))
    (with-bound-types context arg-names arg-types
      (ccons-lambda 
       arg-names
       (if (find-if #'list-type-p arg-types)
	   (ccons 'split 
		  (if (eq (afn body) 'split)
		      (mapcar (bind #'canonize /1 context /2)
			      (args body) (arg-types body context return-type))
		      (list (ccons-lambda 
			     nil (canonize body context return-type)
			     (mklambda nil body))))
		  body)
 	   (canonize body context return-type))
       expr))))
(define-test canonize-function
  (validate-canonize '(lambda () (and (or) (or)))
		     (canonize %(lambda () true) *empty-context* 
			       '(function () bool)))
  (validate-canonize '(lambda (x) (or (and) (and (not x))))
		     (canonize %(lambda (x) (not x)) *empty-context*
			       '(function (bool) bool))
		     '(function (bool) bool))
  (validate-canonize '(lambda (l x) (split (lambda () (or (and) (and x)))))
		     (canonize %(lambda (l x) x) *empty-context*
			       '(function ((list bool) bool) bool))
		     '(function ((list bool) bool) bool))

  (validate-canonize `(lambda (first rest)
			(split (lambda ()
				 (or (and) (and (or) (or first)
						(or x))))))
		     (canonize %(lambda (first rest) (and first x))
			       *empty-context* 
			       '(function (bool (list bool)) bool))
		     '(function (bool (list bool)) bool))
  (validate-canonize 
   `(lambda (l x)
      (split (lambda (first rest)
	       (split (lambda () 
			(or (and) (and (or) (or first) (or x))))))
	     (tuple ,(p2sexpr (canonize 'l *empty-context* '(list bool)))
		    (and (or) (or)))))
   (canonize %(lambda (l x) (split (lambda (first rest) (and first x))
				   (tuple l true)))
	     *empty-context* '(function ((list bool) bool) bool))
   '(function ((list bool) bool) bool)))
