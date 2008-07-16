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

(defun eval-expr (expr &key bindings type)
  (labels ((eval-subexpr (expr)
	     (if (consp expr)
		 (call (car expr) (cdr expr))
		 (lookup expr)))
	   (call (op args)
	     (case op
	       (and (and-op args))
	       (or (or-op args))
	       (not (not (eval-subexpr (car args))))

	       (+ (reduce #'+ args :key #'eval-subexpr))
	       (* (reduce #'* args :key #'eval-subexpr))
	       (exp (exp (eval-subexpr (car args))))
	       (log (let ((arg (eval-subexpr (car args))))
		      (if (< arg 0.01) most-positive-single-float (log arg))))
	       (sin (sin (eval-subexpr (car args))))
	       (t (funeval op args))))
	   (funeval (op args)
	     (apply (symbol-function op) (mapcar #'eval-subexpr args)))
	   (lookup (arg)
	     (if (symbolp arg)
		 (case arg
		   (true t)
		   (false nil)
		   (nil nil)
		   (t (lookup-bindings arg bindings)))
		 arg))
	   (and-op (args) (and (eval-subexpr (car args))
			       (aif (cdr args) (and-op it) t)))
	   (or-op (args) (or (eval-subexpr (car args))
			     (aif (cdr args) (and-op it) nil))))
    (aif (eval-subexpr expr)
	 (if (eq it t) 'true it)
	 (if (eq (or type (expr-type expr bindings)) bool)
	     'false))))

;;   (let ((type (expr-type (or (and (not (consp expr))
;; 				  (aif (find expr bindings :key #'car)
;; 				       (cadr it)))
;; 			     expr))))
;;   (aif (handler-case 
;; 	   (eval `(locally (declare 
;; 			    (sb-ext:muffle-conditions style-warning)
;; 			    (sb-ext:muffle-conditions sb-ext:compiler-note))
;; 		    (let* ((false nil) (true t) ,@bindings) ,expr)))
;; 	 (division-by-zero () most-positive-single-float))
;;        (if (eq it t) 'true it)
;;        'false))

;; for some reason this version is even slower than the one final one (??)
;;   (eval `(let* ((false 'false) (true 'true) (unknown 'unknown) ,@bindings)
;; 		(macrolet ((and (&rest l)
;; 			     `(cond ,@(mapcar (lambda (x)
;; 						`((eq ,x false) false))
;; 					      l)
;; 				    ,@(mapcar (lambda (x)
;; 						`((eq ,x unknown) unknown))
;; 					      l)
;; 				    (t true)))
;; 			   (or (&rest l)
;; 			     `(cond ,@(mapcar (lambda (x)
;; 						`((eq ,x true) true))
;; 					      l)
;; 				    ,@(mapcar (lambda (x)
;; 						`((eq ,x unknown) unknown))
;; 					      l)
;; 				    (t false))))
;; 		  (flet ((not (x) (case x
;; 				    (false true)
;; 				    (true false)
;; 				    (t unknown))))
;; 		    ,expr)))))

;; uncomment to make unknowns work - but about 30x slower
;;   (eval `(let* ((false 'false) (true 'true) (unknown 'unknown) ,@bindings)
;; 	   (labels ((and (&rest l)
;; 		      (if l (case (car l)
;; 			      (true (apply #'and (cdr l)))
;; 			      (false false)
;; 			      (t (if (eq (and (cdr l) false)) false
;; 				     'unknown)))
;; 			  true))
;; 		    (or (&rest l)
;; 		      (if l (case (car l)
;; 			      (false (apply #'or (cdr l)))
;; 			      (true true)
;; 			      (t (if (eq (or (cdr l) true)) true
;; 				     'unknown)))
;; 			  false))
;; 		      (not (x) (case x 
;; 				 (true false)
;; 				 (false true)
;; 				 (t unknown))))
;; 	       ,expr))))

(define-all-equal-test eval-expr
    '((false ((and true false) (or false false) (and false true)))
      (4 ((+ 1 1 1 1) (* 2 2)))))
