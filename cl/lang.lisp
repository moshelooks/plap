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

Author: madscience@google.com (Moshe Looks)

This defines the basic language used to represent evolved programs.
|#
(in-package :plop)

;;; for convenice - the plop language uses these instead of t and nil
(defvar true 'true)
(defvar false 'false)

;; a total ordering on all plop expressions
;; returns less, nil, or greater, with the important property that (not symbol)
;; is always ordered immediately after symbol
(defun total-cmp (l r) 
  (flet ((elem-cmp (l r)
	   (if (numberp l)
	       (if (numberp r)
		   (if (= l r) nil (if (< l r) 'less 'greater))
		   'less)
	       (if (numberp r)
		   'greater
		   (if (eql l r) nil (if (string< l r) 'less 'greater))))))
    (let ((lnot (and (consp l) (eq (car l) 'not) (not (consp (cadr l)))))
	  (rnot (and (consp r) (eq (car r) 'not) (not (consp (cadr r))))))
      (if (consp l)
	  (if lnot
	      (if rnot
		  (elem-cmp (cadr l) (cadr r))
		  (aif (total-cmp (cadr l) r) it 'greater))
	      (if (consp r)
		  (if rnot
		      (aif (total-cmp l (cadr r)) it 'less)
		      (aif (total-cmp (car l) (car r))
			   it 
			   (total-cmp (cdr l) (cdr r))))
		  'greater))
	  (if (consp r) 
	      (if rnot
		  (aif (elem-cmp l (cadr r)) it 'less)
		  'less)
	      (elem-cmp l r))))))
(defun total-order (l r)
  (eq (total-cmp l r) 'less))
(define-test total-order
  (assert-equal '(1 2 3) (sort-copy '(3 1 2) #'total-order))
  (assert-equal '((a 1) (a 1 1) (a 2)) 
		(sort-copy '((a 2) (a 1) (a 1 1)) #'total-order))
  (assert-equal 
   '(1 2 a b c nil (a 1) (a 2) (b b) (b b b))
   (sort-copy '(2 b 1 a c (a 2) (a 1) (b b) (b b b) nil) #'total-order))
  (assert-equal
   '((a (a (b c))) (a (a (b c)) b) (a (a (b c d))))
   (sort-copy '((a (a (b c))) (a (a (b c)) b) (a (a (b c d)))) #'total-order))
  (assert-equal
   '(a (not a) b (not b) c)
   (sort-copy '((not a) (not b) c b a) #'total-order))
  (let ((exprs (randremove 0.9 (enum-trees *enum-trees-test-symbols* 2))))
    (block enumerative-test
      (flet ((opp (x) (case x
			(less 'greater)
			(greater 'less)
		      (nil nil))))
	(dolist (expr1 exprs)
	  (dolist (expr2 exprs)
	    (unless (assert-equal (total-cmp expr1 expr2)
				  (opp (total-cmp expr2 expr1)))
	      (print* expr1 expr2)
	      (return-from enumerative-test nil))))))))

(defun commutativep (x)
  (matches x (and or * +)))
(defun associativep (x)
  (matches x (and or * +)))
(defun identityp (x)
  (matches x (and or * +)))
(defun identity-elem (x)
  (ecase x
    (and 'true)
    (or 'false)
    (* 1)
    (+ 0)))

(defun expr-size (expr) 
  (if (consp expr)
      (reduce #'+ (mapcar #'expr-size (cdr expr)) :initial-value 1) 
      1))

(defun expr-depths (expr)
  (if (consp expr)
      (mapcar #'1+ (mapcan #'expr-depths (cdr expr)))
      (list 0)))
(define-test expr-depths
  (assert-equal '(1 2 2 1) (expr-depths '(and x (or y z) q)))
  (assert-equal '(0) (expr-depths 'x)))

(defun equalp-to-eq (expr)
  (mapl (lambda (expr1) (if (consp (car expr1))
			 (mapl (lambda (expr2)
				 (if (equalp (car expr1) (car expr2))
				     (setf (car expr2) (car expr1))))
			       (cdr expr1))))
	expr))
(define-test equalp-to-eq
  (let* ((foo '(and (or x y) (or x y) (or x y)))
	 (goo (copy-tree foo)))
    (equalp-to-eq foo)
    (assert-eq (second foo) (third foo) (fourth foo))
    (assert-equal foo goo)))

;;; decompositions of expressions by contents
(macrolet
    ((mkdecomposer (name &body conditions)
       `(defmacro ,name (expr &body clauses)
	  `(cond ,@(mapcar (lambda (clause)
			     (dbind (pred &body body) clause
			       (let ((condition (case pred
						  ((t) 't)
						  ,@conditions)))
				 `(,condition ,@body))))
			   clauses)))))
  (mkdecomposer decompose-num
		(constant `(numberp ,expr))
		(t `(and (consp ,expr)
			 ,(if (consp pred) 
			      `(matches (car ,expr) ,pred)
			      `(eq (car ,expr) ',pred)))))
  (mkdecomposer decompose-bool
		(literal `(literalp ,expr))
		(constant `(matches ,expr (true false)))
		(junctor `(junctorp ,expr)))
  (mkdecomposer decompose-tuple
		(tuple `(eq (acar ,expr) 'tuple)))
  (mkdecomposer decompose-function
		(lambda `(and (consp ,expr) (eq (car ,expr) 'lambda))))
  (mkdecomposer decompose-list
		(append `(and (consp ,expr) (eq (car ,expr) 'append)))
		(list  `(and (consp ,expr) (eq (car ,expr) 'list)))))
(define-test decompose-num
  (assert-equal 
   '(cond ((numberp expr) foo) 
     ((and (consp expr) (eq (car expr) '/)) goo)
     ((and (consp expr) (matches (car expr) (* +))) loo)
     (t moo))
   (macroexpand-1 '(decompose-num 
		    expr (constant foo) (/ goo) ((* +) loo) (t moo)))))
(define-test decompose-bool
  (flet ((dectest (expr)
	   (decompose-bool expr
	     (junctor 'junctor)
	     (literal 'literal)
	     (t 'other))))
    (assert-equal 'literal (dectest 'x))
    (assert-equal 'literal (dectest '(not x)))
    (assert-equal 'junctor (dectest '(and x y)))
    (assert-equal 'other (dectest '(foo bar baz)))))

(defun split-by-coefficients (exprs &key (op '*) (identity 1))
  (with-collectors (coefficient term)
    (mapc (lambda (expr)
	    (dbind (coefficient term)
		(if (and (consp expr) (eq (car expr) op) (numberp (cadr expr)))
		    `(,(cadr expr) ,(if (cdddr expr)
					(cons op (cddr expr))
					(caddr expr)))
		    `(,identity ,expr))
	      (coefficient coefficient)
	      (term term)))
	  exprs)))
(defun dual-decompose (expr op op-identity dual dual-identity)
  (flet ((mksplit (offset exprs)
	   (mvbind (weights terms) 
	       (split-by-coefficients exprs :op dual :identity dual-identity)
	     (values offset weights terms))))
    (cond ((numberp expr) (values expr nil nil))
	  ((not (consp expr)) (values op-identity `(,dual-identity) `(,expr)))
	  ((eq (car expr) op) (if (numberp (cadr expr))
				  (mksplit (cadr expr) (cddr expr))
				  (mksplit op-identity (cdr expr))))
	  (t (mksplit op-identity `(,expr))))))

(defun split-sum-of-products (expr) (dual-decompose expr '+ 0 '* 1))
(defun split-product-of-sums (expr) (dual-decompose expr '* 1 '+ 0))

(define-test dual-decompose
  (flet ((ldass (expr o1 ws1 ts1 o2 ws2 ts2)
	   (mvbind (o ws ts) (split-sum-of-products expr)
	     (assert-equal o1 o)
	     (assert-equal ws1 ws)
	     (assert-equal ts1 ts))
	   (mvbind (o ws ts) (split-product-of-sums expr)
	     (assert-equal o2 o)
	     (assert-equal ws2 ws)
	     (assert-equal ts2 ts))))
    (ldass '(+ 1 (* 2 x) (* 3 y z))
	   1 '(2 3) '(x (* y z))
	   1 '(1) '((+ (* 2 x) (* 3 y z))))
    (ldass 42 
	   42 nil nil
	   42 nil nil)
    (ldass '(+ 1 x)
	   1 '(1) '(x)
	   1 '(1) '(x))
    (ldass '(+ x (* y z))
	   0 '(1 1) '(x (* y z))
	   1 '(0) '((+ x (* y z))))
    (ldass 'x 
	   0 '(1) '(x)
	   1 '(0) '(x))
    (ldass '(sin x)
	   0 '(1) '((sin x))
	   1 '(0) '((sin x)))
    (ldass '(* 2 (+ x y) (+ 3 x) (+ x y z))
	   0 '(2) '((* (+ x y) (+ 3 x) (+ x y z)))
	   2 '(0 3 0) '((+ x y) x (+ x y z)))
    (ldass 0
	   0 nil nil
	   0 nil nil)))

(defmacro defdefbytype (defname name)
  `(progn 
     (defvar ,name nil)
     (defmacro ,defname (typematch args &body body)
       `(push (cons ',typematch (lambda ,args ,@body)) ,',name))
     (defun ,name (expr context type)
       (apply (cdr (assoc (icar type) ,name))
	      (if (consp type) `(,expr ,context ,type) `(,expr ,context))))))
