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

;;;; general-purpose reductions are defined here

(define-reduction sort-commutative (fn args markup)
  :condition (and (commutativep fn) (not (sortedp args #'total-order)))
  :action (pcons fn (sort (copy-list args) #'total-order) markup)
  :order upwards
  :preserves all)
(define-test sort-commutative
  (assert-equal '((and simp (sort-commutative)) 
		  x y z ((or simp (sort-commutative)) a b))
		(sort-commutative %(and y (or b a) z x)))
  (assert-equal '((foo simp (sort-commutative)) 
		  zaa baa ((or simp (sort-commutative)) a b))
		(sort-commutative %(foo zaa baa (or b a))))
  (let ((expr %(and x y z)))
    (assert-eq expr (sort-commutative expr))))
  
(define-reduction flatten-associative (fn args markup)
  :condition (and (associativep fn) (find fn args :key #'afn))
  :action (pcons fn
		 (mappend (lambda (arg) 
			    (if (eq (afn arg) fn) (args arg) `(,arg)))
			  args)
		 markup)
  :order upwards)
(define-test flatten-associative
  (assert-equal '((and simp (flatten-associative)) 
		  x y ((or simp (flatten-associative)) q w))
		(flatten-associative %(and x (and y (or q w))))))

(defun pfuncall-const (fn args)
  (peval (pcons fn args) *empty-context*)) ;fixme - much faster

(define-reduction eval-const (expr)
  :condition (and (not (matches (fn expr) (list tuple lambda)))
		  (purep (fn expr)))
  :action
  (dexpr expr (fn args markup)
    (cond ((and (identityp fn) (unary-expr-p expr)) (arg0 expr))
	  ((commutativep fn)
	   (bind-collectors (constants others)
	       (mapc (lambda (arg) 
		       (if (const-value-p arg) (constants arg) (others arg)))
		     args)
	     (if others 
		 (if constants
		     (pcons fn (cons (pfuncall-const fn constants) others)
			    markup)
		     expr)
		 (pfuncall-const fn args))))
	  ((every #'const-value-p args) (pfuncall-const fn args))
	  (t expr)))
  :order upwards)
(define-test eval-const
  (assert-equal 42 (eval-const %(+ 1 (* 1 41))))
  (assert-equal '((+ simp (eval-const)) 3 x ((* simp (eval-const)) 41 x))
		(eval-const %(+ x (* x 41 1) 1 2)))
  (assert-equal true (eval-const %(and (or) (or))))
  (assert-equal '((foo simp (eval-const)) 1 2 x 42)
		(eval-const %(foo 1 2 x (+ 2 40))))
  (assert-equal '((+ simp (eval-const)) 1 x) 
		(eval-const %(+ 1 -2 x 2)))
  (assert-equal '((list simp (eval-const)) 42)
		(eval-const %(if true (list 42) nil)))
  (assert-equal '((list simp (eval-const)) 42 42)
		(eval-const %(append (list 42) (list 42)))))

;; (if true x y) -> x, (if false x y) -> y
;;(define-reduction if-identities (expr) fixme - complete
