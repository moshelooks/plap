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

Expressions in plop are either lisp atoms or function applications. Function
applications are ((fn . markup) . args) where args are the arguments to the
function fn, and markup is arbitrary metadata. Note that args and markup must
be proper lists. |#
(in-package :plop)

;;; for convenice - the plop language uses these instead of t and nil
(defvar true 'true)
(defvar false 'false)

;;; use these accessors and predicates instead of car/cdr & friends
(defun fn (expr) (car expr))
(defun ifn (expr) (if (consp expr) (car expr) expr))
(defun args (expr) (cdr expr))
(defun arg0 (expr) (second expr))
(defun arg1 (expr) (third expr))
(defun arg2 (expr) (fourth expr))
(defun arg3 (expr) (fifth expr))
(defun arg4 (expr) (sixth expr))
(defun arg5 (expr) (seventh expr))
(defun arg6 (expr) (eighth expr))
(defun arg7 (expr) (ninth expr))
(defun arg8 (expr) (tenth expr))

;;; use these constructors instead of cons/list/quote
(defun mkexpr (fn args) (cons (ncons fn) args))

(defun eqfn (expr fn) (and (consp expr) (eq (fn expr) fn)))

;; a total ordering on all plop expressions
;; returns less, nil, or greater, with the important property that (not symbol)
;; is always ordered immediately after symbol
;; markup is ignored
(defun total-cmp (l r)
  (flet ((elem-cmp (l r)
	   (if (numberp l)
	       (if (numberp r)
		   (if (= l r) nil (if (< l r) 'less 'greater))
		   'less)
	       (if (numberp r)
		   'greater
		   (if (eql l r) nil (if (string< l r) 'less 'greater))))))
    (if (consp l)
	(if (eq (fn l) 'not)
	    (if (eqfn r 'not)
		(total-cmp (arg0 l) (arg0 r))
		(or (total-cmp (arg0 l) r) 'greater))
	    (if (consp r)
		(if (eq (fn r) 'not)
		    (or (total-cmp l (arg0 r)) 'less)
		    (or (elem-cmp (fn l) (fn r))
			(prog () (mapl 
				  (lambda (l r)
				    (aif (total-cmp (car l) (car r))
					 (return it)
					 (let ((x (consp (cdr l))) 
					       (y (consp (cdr r))))
					   (unless (eq x y)
					     (return (if x 'greater 'less))))))
				  (args l) (args r)))))
		'greater))
	(if (consp r)
	    (if (eq (fn r) 'not) 
		(or (total-cmp l (arg0 r)) 'less)
		'less)
	    (elem-cmp l r)))))
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


;;   (assert-equal '(1 2 3) (sort-copy '(3 1 2) #'total-order))
;;   (assert-equal '(((a) 1) ((a) 1 1) ((a) 2)) 
;; 		(sort-copy '(((a) 2) ((a) 1) ((a) 1 1)) #'total-order))
;;   (assert-equal '(1 2 a b c nil ((a) 1) ((a) 2) ((b) b) ((b) b b))
;; 		(sort-copy '(2 b 1 a c ((a) 2) ((a) 1) ((b) b) ((b) b b) nil) 
;; 			   #'total-order))
;;   (assert-equal '(((a) ((a) ((b) c))) ((a) ((a) ((b) c)) b)
;; 		  ((a) ((a) ((b) c d))))
;;    (sort-copy '(((a) ((a) ((b) c))) ((a) ((a) ((b) c)) b)
;; 		((a) ((a) ((b) c d)))) #'total-order))
;;   (assert-equal'(a ((not) a) b ((not) b) c)
;;    (sort-copy '(((not) a) ((not) b) c b a) #'total-order))
  (let ((exprs (randremove 0.9 (enum-trees *enum-trees-test-symbols* 2))))
    (block enumerative-test
      (flet ((opp (x) (case x
			(less 'greater)
			(greater 'less)
		      (nil nil))))
	(dolist (expr1 exprs)
	  (dolist (expr2 exprs)
	    (unless (assert-equal (total-cmp expr1 expr2)
				  (opp (total-cmp expr2 expr1))
				  expr1 expr2)
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
      (reduce #'+ (mapcar #'expr-size (args expr)) :initial-value 1) 
      1))

(defun expr-depths (expr)
  (if (consp expr)
      (mapcar #'1+ (mapcan #'expr-depths (args expr)))
      (list 0)))
(define-test expr-depths
  (assert-equal '(1 2 2 1) (expr-depths '(and x (or y z) q)))
  (assert-equal '(0) (expr-depths 'x)))

(defun equalp-to-eq (expr) ;fixme - do we need/use this?
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
			      `(matches (fn ,expr) ,pred)
			      `(eq (fn ,expr) ',pred)))))
  (mkdecomposer decompose-bool
		(literal `(literalp ,expr))
		(constant `(matches ,expr (true false)))
		(junctor `(junctorp ,expr)))
  (mkdecomposer decompose-tuple
		(tuple `(and (consp ,expr) (eq (fn ,expr) 'tuple))))
  (mkdecomposer decompose-function
		(lambda `(and (consp ,expr) (eq (fn ,expr) 'lambda))))
  (mkdecomposer decompose-list
		(append `(and (consp ,expr) (eq (fn ,expr) 'append)))
		(list  `(and (consp ,expr) (eq (fn ,expr) 'list)))))
(define-test decompose-num
  (assert-equal 
   '(cond ((numberp expr) foo) 
     ((and (consp expr) (eq (fn expr) '/)) goo)
     ((and (consp expr) (matches (fn expr) (* +))) loo)
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
		(if (and (consp expr) (eq (fn expr) op) (numberp (arg0 expr)))
		    `(,(arg0 expr) ,(if (cddr (args expr))
					(cons op (cdr (args expr)))
					(arg1 expr)))
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
	  ((eq (fn expr) op) (if (numberp (arg0 expr))
				 (mksplit (arg0 expr) (cdr (args expr)))
				 (mksplit op-identity (args expr))))
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
