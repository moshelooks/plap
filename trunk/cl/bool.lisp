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

(defun free-variables (expr)
  (if (consp expr)
      (case (car expr)
	((and or not) (reduce #'nunion (cdr expr) :key #'free-variables)))
      (list expr)))

(defun truth-table (expr &optional vars)
  (labels ((enum-bindings (vars)
	     (if vars 
		 (flet ((make-bindings (sub-binding v)
			  (cons (list (car vars) v) sub-binding)))
		   (mapcan (lambda (b) (list (make-bindings b 'true)
					     (make-bindings b 'false)))
			   (enum-bindings (cdr vars))))
		 '(nil))))
    (let ((bindings (enum-bindings (or vars (sort (free-variables expr) 
						  #'string<)))))
      (values (mapcar (lambda (b) (eval-expr expr :bindings b)) bindings)
	      bindings))))

(defun test-by-truth-tables (rewrite)
  (let ((vars (mapcar #'car (remove-if (lambda (x) (or (< 0 (cdr x))
						       (eq (car x) 'true)
						       (eq (car x) 'false)))
				       *enum-trees-test-symbols*))))
    (dolist (expr (enum-trees *enum-trees-test-symbols* 3) t)
      (unless (assert-equal (truth-table expr vars)
			    (truth-table (funcall rewrite expr) vars))
	(print expr)
	(return nil)))))

(define-reduction push-nots (expr)
    :type bool
    :condition (and (eq (car expr) 'not)
		    (consp (cadr expr))
		    (case (caadr expr) ((and or not) t)))
    :action 
    (flet ((dual (f) (ecase f (and 'or) (or 'and))))
      (if (eq (caadr expr) 'not) 
	  (cadadr expr)
 	  (cons (dual (caadr expr)) 
 		(mapcar (bind #'cons 'not (list /1)) (cdadr expr)))))
    :order downwards)
(define-test push-nots
  (assert-equal  '(and (not x) (not y)) (push-nots '(not (or x y))))
  (test-by-truth-tables #'push-nots))

;; (define-normal-form bool (expr)
;;   :condition
;; ;;   (labels ((applicable (expr)
;; ;; 	     (and (consp expr)
;; ;; 		  (or (case (car expr)
;; ;; 			(and (find 'and (cdr expr)))
;; ;; 			(or (find 'or (cdr expr)))
;; ;; 			(not (eq (cadr expr 'not))))
;; ;; 		      (find-if #'applicable (cdr expr))))))
;; ;;    (applicable expr))
;;   t
;;   :action
;;   (labels ((to-nf (expr neg) (if (consp expr) 
;; 				 (recurse-on expr neg)
;; 				 (negate-if neg expr)))
;; 	   (negate-if (neg expr) (if neg `(not ,expr) expr))
;; 	   (dual (f) (case f (and 'or) (or 'and)))
;; 	   (recurse-on (expr neg)
;; 	     (case (car expr)
;; 	       (not (to-nf (cadr expr) (not neg)))
;; 	       ((and or) (polyacidize (if neg (dual (car expr)) (car expr))
;; 				      (mapcar (lambda (c) (to-nf c neg))
;; 					      (cdr expr))))
;; 	       (t (negate-if neg (normalize expr)))))
;; 	   (polyacidize (x children) ;children are already in normal form
;; 	     (cons x (reduce (lambda (rest child)
;; 			       (if (and (consp child) (eq (car child) x))
;; 				   (append (cdr child) rest)
;; 				   (cons child rest)))
;; 			     children :initial-value nil))))
;;     (to-nf expr nil)))
;; (define-test normalize-bool
;;   (assert-equal  '(and x y (or q w)) (normalize '(and x (and y (or q w)))))
;;   (assert-equal  '(and (not x) (not y)) (normalize '(not (or x y))))
;;   (test-by-truth-tables #'normalize))

;; (and true x y)  -> (and x y)  (or true x y)  -> true
;; (and false x y) -> false      (or false x y) -> x
;; (and x)         -> x          (or x)         -> x 
(macrolet ((make-reduction (name symbol abort ignore)
	     `(define-reduction ,name (expr)
		:type bool
		:condition (eq ,symbol (car expr))
		:action (let ((result (loop for x in (cdr expr)
					 if (eq x ,abort) return (list ,abort)
					 unless (eq x ,ignore) collect x)))
			  (cond ((null result) ,ignore)
				((null (cdr result)) (car result))
				(t (cons (car expr) result))))
		:order upwards)))
  (make-reduction bool-and-identities 'and 'false 'true)
  (make-reduction bool-or-identities 'or 'true 'false))
(define-test bool-and-identities
  (assert-equal '(and x y) (bool-and-identities '(and x true y)))
  (assert-equal 'false 
		(bool-and-identities '(and false x y))
 		(bool-and-identities '(and x false y))
 		(bool-and-identities '(and x y false)))
  (assert-equal 'x  (bool-and-identities '(and x)))
  (test-by-truth-tables #'bool-and-identities))
(define-test bool-or-identities
  (assert-equal 'true (bool-or-identities '(or x true y)))
  (assert-equal '(or x y) 
		(bool-or-identities '(or false x y))
		(bool-or-identities '(or x false y))
		(bool-or-identities '(or x y false)))
  (assert-equal 'x  (bool-or-identities '(or x)))
  (test-by-truth-tables #'bool-or-identities))

(defun negatesp (x y &key (pred #'eq))
  (flet ((check (neg other) 
	   (and (eq (car neg) 'not) (funcall pred (cadr neg) other))))
  (if (consp x) (check x y) (if (consp y) (check y x)))))
(define-test negatesp
  (assert-true (negatesp '(not x) 'x) (negatesp 'x '(not x)))
  (assert-false (negatesp 'x 'x) (negatesp '(not x) '(not x))))

;;; for the following, clauses must be sorted
;;; a clause is a tautology if it contains a variable and its negation
(defun tautologyp (clause)
  (find-if (lambda (p) (funcall #'negatesp (car p) (cdr p)))
	   (adjacent-pairs clause)))
;; (define-reduction identify-tautologies (expr)
;;   :type bool
;;   :condition (eq 'or (car expr))
;;   :action (if (tautology

(defun implications (clause1 clause2)
  (let ((result nil))
    (dolist (x clause1 result)
      (dolist (y clause2)
	(if (negatesp x y)
	    (let ((implication 
		   (uniq (merge 'list (remove x clause1) (remove y clause2) 
				#'total-order))))
	      (unless (tautologyp implication) (push implication result))))))))
(define-test implications
  (assert-equal '((x y)) 
		(implications '(x y (not z)) '(x y z))
		(implications '(x (not z)) '(y z)))
  (assert-equal nil 
		(implications '(x y (not z)) '(x y (not z)))
		(implications '(x (not y) (not z)) '(y z))))


;; (define-reduction reduce-ors (expr)
;;   :type bool
;;   :condition (eq 'or (car expr))
;; ;  :prerequisites '(level bool-and-identities bool-or-identities equalp-to-eq
;;   ;remove-tautologies)
;;   :action
;;   (progn (equalp-to-eq (apply #'append (cdr expr)))
	 

