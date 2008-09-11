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

(defun truth-table (expr &optional (vs (free-variables expr))
		    &aux (context (make-context)))
  (collecting
    (labels ((enum-bindings (vs)
	       (if vs
		   (dbind (v &rest vs) vs
		     (setf (get-value v context) 'true)
		     (enum-bindings vs)
		     (setf (get-value v context) 'false)
		     (enum-bindings vs))
		   (collect (peval-cl expr context)))))
      (mapc (bind #'bind-symbol context /1 nil 'bool) vs)
      (enum-bindings vs))))
(defun truth-table-hamming-distance (tt1 tt2)
  (let ((i 0))
    (map nil (lambda (x y) (unless (eq x y) (incf i)))
	 tt1 tt2)
    i))
(define-test truth-table-hamming-distance
  (mapc (lambda (tt1 tt2 d)
	  (assert-equal d (truth-table-hamming-distance tt1 tt2)))
	'((true true false) (true true) (false true))
	'((false true true) (true true) (true false))
	'(2 0 2)))

;; (defvar *n-random-trees-for-testing* 10000)
;; (labels ((randtree (depth)
;; 	   (if (eql (depth 0)

;; (nconc (collecting
;; 			     (dotimes (n *n-random-trees-for-testing*)
;; 			       (let ((depth (1+ (random 10))))
;; 				 (labels 
(defmacro test-by-truth-tables (rewrite)
  `(let ((vars (mapcar #'car (remove-if (lambda (x) (or (< 0 (cdr x))
							(eq (car x) 'true)
							(eq (car x) 'false)))
					*enum-trees-test-symbols*))))
     (dolist (expr (enum-trees *enum-trees-test-symbols* 2) t)
       (unless (assert-equal (truth-table expr vars)
			     (truth-table (funcall ,rewrite expr) vars)
			     expr
			     (funcall ,rewrite expr))
	 (return nil)))))

(defun bool-dual (f) (ecase f (and 'or) (or 'and) (true false) (false true)))
(defun junctorp (expr) (matches (afn expr) (and or)))
(defun literalp (expr)
  (if (consp expr)
      (and (eq (fn expr) 'not) (not (consp (arg0 expr))))
      (not (matches expr (true false)))))

;;; boolean reductions

(define-reduction push-nots (expr)
    :type bool
    :condition (and (eq (fn expr) 'not)
		    (matches (afn (arg0 expr)) (and or not)))
    :action 
    (if (eq (fn (arg0 expr)) 'not)
	(arg0 (arg0 expr))
	(pcons (bool-dual (fn (arg0 expr)))
	       (mapcar (lambda (subexpr)
			 (pcons 'not (list subexpr)))
		       (args (arg0 expr)))
	       (markup expr)))
    :order downwards)
(define-test push-nots
  (assert-equal  '(and (not x) (not y)) (p2sexpr (push-nots %(not (or x y)))))
  (test-by-truth-tables #'push-nots))

(defmacro define-bool-dual-reductions (and-name or-name
				       (operator identity complement expr)
				       &body body)
  (flet ((dosub (x y z)
	   (subst x operator (subst y identity (subst z complement body)))))
    `(progn (define-reduction ,and-name (,expr)
	      :type bool
	      ,@(dosub ''and ''true ''false))
	    (define-reduction ,or-name (,expr)
	      :type bool
	      ,@(dosub ''or ''false ''true)))))

;; (and true x y)  -> (and x y)  (or true x y)  -> true
;; (and false x y) -> false      (or false x y) -> x
;; (and x)         -> x          (or x)         -> x 
(define-bool-dual-reductions bool-and-identities bool-or-identities 
  (operator identity complement expr)
  :condition (and (eq operator (fn expr))
		  (find-if #'const-atom-p (args expr)))
  :action (let ((result (loop for x in (args expr)
			   if (eq x complement) return (list complement)
			   unless (eq x identity) collect x)))
	    (cond ((null result) identity)
		  ((null (cdr result)) (car result))
		  (t (pcons (fn expr) result (markup expr)))))
  :order upwards)
(define-test bool-and-identities
  (assert-equal '(and x y) (p2sexpr (bool-and-identities %(and x true y))))
   (assert-for-all (compose (bind #'eq 'false /1) #'bool-and-identities)
		   (mapcar #'sexpr2p 
			   '((and false x y) (and x false y) (and x y false))))
   (assert-equal 'x  (bool-and-identities %(and x)))
   (test-by-truth-tables #'bool-and-identities))
(define-test bool-or-identities
  (assert-equal true (bool-or-identities %(or x true y)))
  (mapc (lambda (expr) 
	  (assert-equal '(or x y) 
			(p2sexpr (bool-or-identities (sexpr2p expr)))))
	'((or false x y) (or x false y) (or x y false)))
  (assert-equal 'x  (bool-or-identities %(or x)))
  (test-by-truth-tables #'bool-or-identities))

(defun negate (expr)
  (if (eq (afn expr) 'not) (arg0 expr) (pcons 'not expr)))
(defun litvariable (x) (if (consp x) (arg0 x) x))
(defun negatesp (x y &key (pred #'eq))
  (flet ((check (neg other) 
	   (and (eq (fn neg) 'not) (funcall pred (arg0 neg) other))))
  (if (consp x) (check x y) (if (consp y) (check y x)))))
(define-test negatesp
  (assert-true (negatesp %(not x) 'x))
  (assert-true (negatesp 'x %(not x)))
  (assert-false (negatesp 'x 'x))
  (assert-false (negatesp %(not x) %(not x))))

; returns literals or literals only-children of junctors
(defun extract-literal (expr)
  (cond ((literalp expr) expr)
	((and (consp expr) (singlep (args expr)) (literalp (arg0 expr))) 
	 (arg0 expr))))

(defun var-and-negation-p (clause) ; clauses must be sorted
  (mapc (lambda (x y) 
	  (if (negatesp x y) (return-from var-and-negation-p t)))
	clause (cdr clause))
  nil)
(defun bool-identities (op)
  (ecase op
    (and 'bool-and-identities)
    (or 'bool-or-identities)))

(define-bool-dual-reductions identify-contradictions identify-tautologies 
  (operator identity complement expr)
  :assumes (sort-commutative)
  :condition (eq operator (fn expr))
  :action (if (var-and-negation-p (args expr)) complement expr)
  :order upwards)
(define-test identify-contradictions
  (flet ((mung (expr) (p2sexpr (identify-contradictions expr))))
    (assert-equal 'false (mung %(and x (not x))))
    (assert-equal '(and x (not y)) (mung %(and x (not y))))
    (assert-equal 'z (mung '(or z (and x (not x)))))
    (test-by-truth-tables #'identify-contradictions)))
(define-test identify-tautologies
  (flet ((mung (expr) (p2sexpr (identify-tautologies expr))))
    (assert-equal 'true (mung %(or x (not x))))
    (assert-equal '(or x (not y)) (mung %(or x (not y))))
    (assert-equal 'z (mung %(and z (or x (not x)))))
    (test-by-truth-tables #'identify-tautologies)))

(define-reduction remove-bool-duplicates (expr)
  :type bool
  :assumes (sort-commutative)
  :condition (and (junctorp expr) (some #'eq (args expr) (cdr (args expr))))
  :action (pcons (fn expr)
		 (remove-adjacent-duplicates (args expr))
		 (markup expr))
  :order upwards)
(define-test remove-bool-duplicates
  (assert-equal '(and x z) (p2sexpr (remove-bool-duplicates %(and z x x z z))))
  (let ((expr %(and x y z)))
    (assert-eq expr (remove-bool-duplicates expr))))

(defun mkclause (expr)
  (if (junctorp expr) (args expr) (list expr)))

(define-reduction remove-superset-clauses (expr)
  :type bool
  :assumes (flatten-associative remove-bool-duplicates
	    identify-contradictions identify-tautologies
	    bool-and-identities bool-or-identities 
	    sort-commutative)
  :condition (junctorp expr)
  :action
  (aif (collecting (map-nonidentical-pairs 
		    (lambda (c1 c2)
		      (if (includesp c1 c2 #'total-order)
			  (collect c1)))
		    (mapcar #'mkclause (args expr))))
       (remove-if (lambda (x) 
		    (and (consp x) (find (mkclause x) it :test #'equal)))
		  expr)
       expr)
  :order upwards)
;; (define-test remove-superset-clauses
;;   (flet ((assert-reduces-to (target exprs)
;; 	   (dolist (expr exprs)
;; 	     (assert-equal target (remove-superset-clauses
;; 				   (sort-commutative expr))))))
;;     (assert-reduces-to '(and x z) '((and (or x y) x z)
;; 				    (and (or x y) x z (or x y) (or x y z))))
;;     (assert-reduces-to '(or x z) '((or (and x y) x z)
;; 				   (or (and x y z) x z (and x y) (and x y z))))
;;     (test-by-truth-tables #'remove-superset-clauses)))

(defun implications (clause1 clause2)
  (let ((result nil))
    (dolist (x clause1 result)
      (dolist (y clause2)
	(if (negatesp x y)
	    (let ((implication 
		   (delete-adjacent-duplicates
		    (merge 'list (remove x clause1) (remove y clause2) 
			   #'total-order))))
	      (unless (var-and-negation-p implication) 
		(push implication result))))))))
(define-test implications
  (assert-equal '((x y)) (implications '(x y ((not) z)) '(x y z)))
  (assert-equal '((x y)) (implications '(x ((not) z)) '(y z)))
  (assert-equal nil (implications '(x y ((not) z)) '(x y ((not) z))))
  (assert-equal nil (implications '(x ((not) y) ((not) z)) '(y z))))

(defun unmkclause (x)
  (if (or (eq 'not (car x)) (cddr x)) x (cadr x)))

;; (define-reduction reduce-or-implications (expr)
;;   :type bool
;;   :condition (eq 'or (car expr))
;;   :action
;;   (let* ((clauses (mapcar #'mkclause (cdr expr)))
;; 	 (for-replacement (make-hash-table))
;; 	 (for-removal
;; 	  (collecting (map-upper-triangle-pairs
;; 		       (lambda (c1 c2) 
;; 			 (mapc (lambda (impl)
;; 				 (mapc (lambda (c3)
;; 					 (if (and (not (eq c3 c1)) 
;; 						  (not (eq c3 c2))
;; 						  (includesp c3 impl 
;; 							     #'total-order))
;; 					     (collect c3)))
;; 				       clauses)
;; 				 (flet
;; 				     ((strict-check (x)
;; 					(if (strict-includes-p x impl 
;; 							      #'total-order)
;; 					    (push impl (gethash 
;; 							x for-replacement)))))
;; 				   (strict-check c1)
;; 				   (strict-check c2)))
;; 			       (implications c1 c2)))
;; 		       clauses))))
;;     (let ((removed (if for-removal 
;; 		       (cons (car expr) 
;; 			     (remove-if (lambda (e) 
;; 					  (find (mkclause e) for-removal
;; 						:test #'equal))
;; 					(cdr expr)))
;; 		       expr)))
;;       (if (eql 0 (hash-table-count for-replacement)) removed
;; 	  (mapcar (lambda (e)
;; 		    (aif (gethash (mkclause e) for-replacement)
;; 			 (unmkclause (argmin #'expr-size it))
;; 			 e))
;; 		  removed))))
;;   :prerequisites '(remove-superset-clauses)
;;   :order upwards)
;; (define-test reduce-or-implications
;;   (test-by-truth-tables #'reduce-or-implications))



;; ;;; if the handle set centered at expr is inconsistent, remove the subtree
;; ;;; rooted at expr
;; (define-reduction remove-inconsistent-handles (expr :parents parents)
;;   :type bool
;;   :order downwards
;; )

;; ;;; holman calls this promote-common-constraints
;; (define-reduction inverse-distribution (expr :parent parent)
;;   :condition (distributive-over expr parent)

;; ;;; holman's cut-unnecessary-or and cut-unnecessary-and
;; (define-reduction eliminate-identities (expr)
;;   :condition (and (identityp (car expr)) (not (cddr expr)))
;; )

;; ;;; constraints in expr's handle are subtracted from expr
;; (define-reduction subtract-redundant-constraints (expr :parents parents)
;;   :type bool
;; )

;; ;;; and clauses containing unit-command literals have their subtrees removed
;; (define-reduction constraint-subsumption (expr :parents parents)
;;   :type bool
;;   :condition (eq 'and (car expr))
;; )

;; ;;; the negations of unit-command literals are subtracted from and clauses
;; (define-reduction contraint-complement-subtraction (expr :parents parents)
;;   :type bool
;;   :condition (eq 'and (car expr))
;; )

;;; need to handle dangling ors and ands from holman's transformations
;;; these can be propagated upwards ... this should be a cleanup function
;;; - make cleanup any function, btw, not a reduction

;; idea - what about considering for insertion minimal trees that only use n
;; arguments? e.g. all boolean exprs with the vars, etc. these could be cached
;; in minimal form...