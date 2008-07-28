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

(defun canonize-children (expr type)
  (if (atom expr) expr
      (cons (car expr) (mapcar #'canonize (cdr expr) (arg-types expr type)))))

(labels 
    ((structure-bool (op children)
       (cons
	(list op)
	(mapcar (lambda (expr)
		  (cons op (decompose-bool expr
			     (literal (ncons expr))
			     (junctor (structure-bool (dual-bool-op op) 
						      (cdr expr)))
			     (t (ncons (canonize-children expr 'bool))))))
		children))))
  (defun canonize-bool (expr &aux (op (if (junctorp expr) (car expr) 'and))
			(dual (dual-bool-op op)))
    `(,dual (,op) ,(cons op (decompose-bool expr
			      (literal (ncons expr))
			      (constant nil)
			      (junctor (structure-bool dual (cdr expr)))
			      (t (ncons (canonize-children expr 'bool))))))))

(defun canonize-num (expr &aux (ops '(exp log sin)) (op-offsets '(0 1 0)))
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
			     (canonize-children (car ts) 'num)))))))
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

(defun structure-list (list elem type)
  (cons 'append
	(interleave elem (mapcar (lambda (x)
				   `(if true ,(canonize-children x type) nil))
				 list))))

better name?
def-on-expr body -> (defun 
(defmacro defunexpr (name &body body)
  `(defun ,name (expr &optional context (type (expr-type expr)))
     ,@body))

    

(defun canonize (expr &optional context (type (expr-type expr)))
  (print* 'got expr type)
  (ecase (icar type)
    (bool (canonize-bool expr))
    (num  (canonize-num expr))
    (function
     (cons 
      'lambda
      (dbind (arg-types return-type) (cdr type)
	(decompose-function expr
	  (lambdsa
	   (dbind (arg-names body) (cdr expr)
	     (list arg-names
		   (if (find-if #'list-type-p arg-types)
		       (cons 'split 
			     (if (not (eq (acar body) 'split))
				 `(() () (lambda ()
					   ,(canonize body return-type)))
				 (dbind (lists defaults body) (cdr body)
				   (list (should canonical form for the 
						 list of lists go here?)
					 (mapcar #'canonize defaults
						 (mapcar 
						  fck, get types of defaults
						  somehow and canonize
						  
						  get based on types of lists
						  we need bindings(?)

						  wait this isn't list list any
						  it breaks type system
						  do we care?
						  ))
					 (canonize-function ;create me
					  body `(function ,default-types
							  return-type)

				 
	  (t (let ((arg-names (mapcar #'genname arg-types)))
	       (cons arg-names
		     (nconc (if (consp expr)
				`(funcall ,(canonize-children expr type))
				`(,expr))
			    (mapcar #'canonize arg-names arg-types)))))))))
    (list
     (let* ((subtype (cadr type))
	    (elem `(if false
		       (list ,(canonize (default-value subtype) subtype))
		       nil)))
       (structure-list (decompose-list expr (append (cdr expr)) (t `(,expr)))
		       elem type)))))

;; should inclusion of already present list elements be conditional?
;; should we build the boolean canonical form?
;; if not, maybe just let true/false slip into a literal?


(define-test canonize
  ;; boolean cases
  (assert-equal '(or (and) (and x))
		(canonize 'x 'bool))
  (assert-equal '(or (and) (and (or) (or x1) (or (not x4)))) 
		(canonize '(and x1 (not x4)) 'bool))
  (assert-equal '(and (or) (or (and) (and x1) (and (not x4))))
		(canonize '(or x1 (not x4)) 'bool))
  (assert-equal '(or (and) (and x1)) (canonize 'x1 'bool))
  (assert-equal  '(and (or) (or (and) (and (or) (or x1) (or x2)) (and x3)))
		 (canonize '(or (and x1 x2) x3) 'bool))
  ;; mixed boolean-numeric and numeric cases
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
		  (canonize '(< 2 3) 'bool))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block)
		  (canonize 0))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block
		      (* 1 ,@(cddr mult-block) (+ 0 x)))
		  (canonize 'x 'num))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block
		      (* 1
			 (+ 1 ,exp-block)
			 (+ 1 ,log-block)
			 (+ 0 (sin (+ 0 ,@add-blocks ,mult-block
				      (* 1 ,@(cddr mult-block) (+ 0 x)))))))
		  (canonize '(sin x)))
    (assert-equal `(+ 0 ,@add-blocks ,mult-block
		      (* 1 
			 ,@(cddr mult-block) 
			 (+ 1 ,@add-blocks)
			 (+ 0 ,@add-blocks (* 1 x))
			 (+ 0 ,@add-blocks (* 1 y))))
		  (canonize '(* x y)))
     (assert-equal `(* 1 ,@(cddr mult-block) (+ 1 ,@add-blocks)
		       (+ 0 ,@add-blocks ,mult-block
			  (* 1 ,@(cddr mult-block) (+ 0 x))
			  (* 1 ,@(cddr mult-block) (+ 0 y))))
		       (canonize '(+ x y)))
     (assert-equal `(+ 1 ,@add-blocks ,mult-block
		       (* 1 ,@(cddr mult-block) (+ 0 x)))
		   (canonize '(+ 1 x)))
     (assert-equal `(* 1 ,@(cddr mult-block) (+ 1 ,@add-blocks)
		       (+ 1 ,@add-blocks ,mult-block
			  (* 1 ,@(cddr mult-block) (+ 0 x))
			  (* 1 ,@(cddr mult-block) (+ 0 y))))
		   (canonize '(+ 1 x y))))
  ;; function type cases
  (assert-equal '(lambda (l x) (split () () (lambda () (or (and) (and x)))))
		(canonize '(lambda (l x) x)
			  '(function ((list bool) bool) bool)))
  (assert-equal '(lambda (l x) 
		  (split (l) (true)
		   (lambda (first rest)
		     (split () () 
		      (or (and) (and (or) (or first) (or x)))))))
		(canonize '(lambda (l x)
			    (split (l) (true)
			     (lambda (first rest) (and first x))))
			  '(function ((list bool) bool) bool)))
  ;; list type cases
  (assert-equal `(append (if false (list ,(canonize 0)) nil)
			 (if true (list ,(canonize 42)) nil)
			 (if false (list ,(canonize 0)) nil))
		(canonize '(list 42))))

(defun loci (fn expr &key (type (expr-type expr)) parents)
  (flet ((boolrec (&optional type)
	   (mapc (bindapp #'loci fn /1 :parents (cons expr parents)
			  (if type (list :type type)))
		 (cdr expr))))
    (ecase type
      (bool
       (decompose-bool expr
	 (literal)
	 (junctor (funcall fn expr parents)
		  (boolrec 'bool))
	 (t (boolrec)))))))
(define-test loci-bool
  (assert-equal '(((and (or x) (or y)))
		  ((or x) (and (or x) (or y)))
		  ((or y) (and (or x) (or y))))
		(collecting (loci (lambda (expr parents) 
					 (collect (cons expr parents)))
				       '(and (or x) (or y))))))
(defun make-replacer-knob (at &rest settings)
  (apply 
   #'vector 
   (cons (let ((original (car at))) (lambda () (rplaca at original)))
	 (mapcar (lambda (setting) (lambda () (rplaca at setting)))
		 settings))))
(defun make-inserter-knob (at &rest settings)
  (apply  
   #'vector
   (let ((set-to nil))
     (cons (lambda () (when set-to
			(aif (cdr set-to)
			     (rplacd (rplaca set-to (car it))
				     (cdr it))
			     (progn (assert (eq (cdr at) set-to))
				    (rplacd at nil)))
			(setf set-to nil)))
	   (mapcar (lambda (setting) 
		     (lambda () 
		       (if set-to
			   (rplaca set-to setting)
			   (rplacd at (setf set-to (cons setting (cdr at)))))))
		   settings)))))
(defun knob-arity (knob) (array-total-size knob))

(defmacro test-knob (list knob results)
  `(progn (dorepeat 100 (let ((n (random 4)))
			  (funcall (elt ,knob n))
			  (assert-equal (elt ,results n) ,list)))
	  (funcall (elt ,knob 0))))
(define-test make-replacer-knob
  (let* ((list (list 1 2 3 4))
	 (knob (make-replacer-knob (cdr list) 'x 'y 'z))
	 (knob2 (make-replacer-knob list 'a 'b 'c))
	 (knob3 (make-replacer-knob (last list) 'p 'd 'q))
	 (res (vector '(1 2 3 4) '(1 x 3 4) '(1 y 3 4) '(1 z 3 4)))
	 (res2 (vector '(1 2 3 4) '(a 2 3 4) '(b 2 3 4) '(c 2 3 4)))
	 (res3 (vector '(1 2 3 4) '(1 2 3 p) '(1 2 3 d) '(1 2 3 q))))
    (test-knob list knob res)
    (test-knob list knob2 res2)
    (test-knob list knob3 res3))
  (let* ((list (list 1))
	 (knob (make-replacer-knob list 'a 'b 'c))
	 (res (vector '(1) '(a) '(b) '(c))))
    (test-knob list knob res)))
(define-test make-inserter-knob
  (let* ((list (list 1 2 3 4))
	 (knob (make-inserter-knob (cdr list) 'x 'y 'z))
	 (knob2 (make-inserter-knob list 'a 'b 'c))
	 (knob3 (make-inserter-knob (last list) 'p 'd 'q))
	 (res (vector '(1 2 3 4) '(1 2 x 3 4) '(1 2 y 3 4) '(1 2 z 3 4)))
	 (res2 (vector '(1 2 3 4) '(1 a 2 3 4) '(1 b 2 3 4) '(1 c 2 3 4)))
	 (res3 (vector '(1 2 3 4) '(1 2 3 4 p) '(1 2 3 4 d) '(1 2 3 4 q))))
    (test-knob list knob res)
    (test-knob list knob2 res2)
    (test-knob list knob3 res3))
  (let* ((list (list 1))
	 (knob (make-inserter-knob list 'a 'b 'c))
	 (res (vector '(1) '(1 a) '(1 b) '(1 c))))
    (test-knob list knob res)))

(defun knobs-at (expr bindings &key (type (expr-type expr)))
  (let ((tovisit (copy-hash-table (gethash type bindings))))
    (collecting
      (ecase type
	(bool
	 (decompose-bool expr
	   (junctor
	    (aif (extract-literal expr)
		 (remhash (litvariable it) tovisit)
		 (mapl (lambda (l)
			 (let ((at (car l)))
			   (awhen (extract-literal at)
			     (assert (junctorp at))
			     (remhash (litvariable it) tovisit)
			     (collect (make-replacer-knob 
				       (cdr at) ; a single knob for:
				       (identity-elem (car at)) ; 1 rm
				       (litnegation it))))))    ; 2 negate
		       (cdr expr)))
	    (maphash-keys (lambda (x)
			    (collect (make-inserter-knob 
				      expr x (litnegation x))))
			  tovisit))))
	(num
	 (decompose-num expr
	   ((* +)
	    (let ((body
		   (let ((x (cadr expr)))
		     (if (numberp x)
			 (let ((e1 (little-epsilon x)) (e2 (big-epsilon x)))
			   (collect (make-replacer-knob (cdr expr)
							(+ x e1)
							(- x e1)
							(- x e2)
							(+ x e2)))
			   (cdr expr))
			 expr)))
		  (mult (eq (car expr) '*)))
	      (mvbind (o ws ts) (funcall (if mult
					     #'split-product-of-sums
					     #'split-sum-of-products)
					 expr)
		(declare (ignore o ws))
		(mapc (bind #'remhash /1 tovisit)
		      (mapcar #'haxx-num-2
			      (mapcar #'haxx-num-1 ts))))
	      (maphash-keys (lambda (x)
			      (let ((e1 (little-epsilon x))
				    (e2 (big-epsilon x)))
				(collect 
				 (apply #'make-inserter-knob body
					(let ((terms`((* ,e1 ,x)
						      (* ,(- e1) ,x)
						      (* ,(- e2) ,x)
						      (* ,(+ e2) ,x))))
					  (if mult 
					      (mapcar (bind #'list '+ 1 /1) 
						      terms)
					      terms))))))
			    tovisit)))))))))

