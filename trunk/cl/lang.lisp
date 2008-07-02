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

This defines the basic language used to represent evolved programs, along with
type inference and evaluation code. Valid types are: any, bool, num,
nil, (list t), (tuple t1 ... tN), (fun out in1 ... inN), (enum name),
and (act-result name), where name may be any symbol. |#
(in-package :plop)

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
  (assert-equal '(1 2 3) (sort '(3 1 2) #'total-order))
  (assert-equal '((a 1) (a 1 1) (a 2)) 
		(sort '((a 2) (a 1) (a 1 1)) #'total-order))
  (assert-equal 
   '(1 2 a b c nil (a 1) (a 2) (b b) (b b b))
   (sort '(2 b 1 a c (a 2) (a 1) (b b) (b b b) nil) #'total-order))
  (assert-equal
   '((a (a (b c))) (a (a (b c)) b) (a (a (b c d))))
   (sort '((a (a (b c))) (a (a (b c)) b) (a (a (b c d)))) #'total-order))
  (assert-equal
   '(a (not a) b (not b) c)
   (sort '((not a) (not b) c b a) #'total-order))
  (let ((exprs (randremove 0.9 (enum-trees *enum-trees-test-symbols* 2))))
    (block enumerative-test
      (flet ((opp (x) (case x
			(less 'greater)
			(greater 'less)
		      (nil nil))))
	(dolist (expr1 exprs)
	  (dolist (expr2 exprs)
	    (let ((v (eql (total-cmp expr1 expr2)
			  (opp (total-cmp expr2 expr1)))))
	      (unless (assert-equal (total-cmp expr1 expr2)
				    (opp (total-cmp expr2 expr1)))
		(print* expr1 expr2)
		(return-from enumerative-test nil)))))))))

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
    ((mkdecomposer (name type &body conditions)
       `(defmacro ,name (expr &body clauses)
	  `(cond ,@(mapcar (lambda (clause)
			     (destructuring-bind (pred &body body) clause
			       (let ((condition (case pred
						  ((t) 't)
						  ,@conditions)))
				 `(,condition ,@body))))
			   clauses)))))
  (mkdecomposer decompose-num 'num
		(constant `(numberp ,expr))
		(t `(and (consp ,expr)
			 ,(if (consp pred) 
			      `(matches (car ,expr) ,pred)
			      `(eq (car ,expr) ',pred)))))
  (mkdecomposer decompose-bool 'bool
		(literal `(literalp ,expr))
		(junctor `(junctorp ,expr))))
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
	    (destructuring-bind (coefficient term)
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
	   (multiple-value-bind (weights terms)
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
	   (multiple-value-bind (o ws ts) (split-sum-of-products expr)
	     (assert-equal o1 o)
	     (assert-equal ws1 ws)
	     (assert-equal ts1 ts))
	   (multiple-value-bind (o ws ts) (split-product-of-sums expr)
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

;(defun mapcar-expr (fn expr)
	     


;idea - make tuples arrays

;; (defun type-union (&rest types)
;;   (flet ((pairwise-union (x y)
;; 	   (cond ((equal x y) x)
;; 		 ((and (consp t) (consp y)
;; 		       (eq (car t) (car y)) (same-length-p x y))
;; 		  (case (car t)
;; 		    (list `(list ,(type-union (cadr t) (cadr y))))
;; 		    (tuple (cons 'tuple (mapcar #'type-union (cdr t) (cdr y))))
;; 		    (fun (aif (mapcar-until nil #'type-intersection 
;; 					    (cddr t) (cddr y))
;; 			      `(fun ,(type-union (cadr t) (cadr y)) ,@it)))
;; 		    (t 'any)))
;; 		 (t 'any))))
;;     (reduce-until 'any #'pairwise-union types)))
;; (define-all-equal-test type-union
;;     `((bool ((bool) (bool bool)))
;;       (num ((num) (num num)))
;;       (nil ((nil) (nil nil)))
;;       ((list num) ((list num) nil (list num))))
;;   (lambda (args) (apply #'type-union args)))

;; (let (fun2type 
;;       (init-hash-table
;;        `((and (fun bool (list bool)))
;; 	 (or (fun bool (list bool)))
;; 	 (not (fun bool bool))
;; 	 (true bool)
;; 	 (false bool)
	 
;; 	 (+ (fun num (list num)))
;; 	 (* (fun num (list num)))
;; 	 (/ (fun num num num))
	 
;; 	 (< (fun bool any any)) ;fixme < and = just on nums
;; 	 (= (fun bool any any))

;; 	 (cons (fun (list any) any (list any)))
;; 	 (append (fun (list any) (list (list any))))
;; 	 (car (fun any (list any)))
;; 	 (cdr (fun (list any) (list any)))
;; 	 (length num)
;; 	 (accumulate (fun any (fun any any any) (list any) any))
;; 	 (mapcar (fun (list any) (fun any any) (list any)))

;; 	 (progn (act-result any),type-of-first)
;; 	  (andseq ,type-of-first)
;; 	  (orseq ,type-of-first)))))

;; 	   (cons ,(lambda (x l) (type-union `(list ,(tree-type x))
;; 					    (tree-type l))))
;; 	   (append ,(lambda (&rest ls) (apply #'type-union 
;; 					      (mapcar '#tree-type ls))))
;; 	   (car ,(lambda (l) (if (eq (car l) 'list)
;; 				 (tree-type (car l))
;; 				 (cadr (tree-type l)))))
;; 	   (cdr ,(lambda (l) (if (eq (car l) 'list)
;; 				 (tree-type `(list ,(cddr l)))
;; 				 (tree-type l))))
;; (type-of-first (lambda (&rest l) (tree-type (car l))))

;;   (defun fun-type (f) 
;;     (multiple-value-bind (v exists) (gethash f fun2type)
;;       (assert exists)
;;       v)))
;; x
