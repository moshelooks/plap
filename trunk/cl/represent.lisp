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

(defun canonize (expr &key (type (expr-type expr)) parent)
  (let ((op (if (consp expr) (car expr))))
    (labels
	((rec-canonize (expr) 
	   (let ((op (if (consp expr) (car expr))))
	     (if op
		 (cons op (mapcar (bind #'canonize /1 :parent op
					:type (or (expr-type /1) type)) ;fixme
				  (cdr expr)))
		 expr)))
	 (bool-structure (expr)
	   (if parent 
	       (list (dual-bool-op parent) expr)
	       (list 'or (list 'and) (list 'and expr))))
	 (linear-structure (expr)
	   (linear-decompose expr (offset weights terms)
	     (nconc (list '+ offset)
		    (mapcar (lambda (weight term)
			      (cons '* (cons weight 
					     (decompose-num term
					       (* (cdr term))
					       (t (list term))))))
			    weights terms))))
	 (div-structure (expr)
	   (decompose-num expr
	     (/ (rec-canonize expr))
	     (t (list '/ (linear-structure (rec-canonize expr)) (list '+ 1)))))
	 (num-structure (expr &optional (offset 0) (weight 1))
	   (list '+ offset (list '* weight (div-structure expr)))))
      (decompose (expr type)
	(bool
	 (literal (bool-structure expr))
	 (junctor 
	  (let 
	      ((body (apply #'list op (list (dual-bool-op op))
			    (mapcar (bind #'canonize /1 :type 'bool :parent op)
				    (cdr expr)))))
	    (if parent body (list (dual-bool-op op) (list op) body))))
	 (t (bool-structure (rec-canonize expr))))
	(num
	 (constant (num-structure expr))
	 (t (case  parent
	      (* (linear-structure (rec-canonize expr)))
	      (/ (linear-structure expr)) ; already div-structured
	      (t (linear-decompose expr (offset weights terms)
		   (nconc (num-structure 0 offset 0)
			  (mapcar (lambda (weight term)
				    (list '* weight (div-structure term)))
				  weights terms)))))))))))

(define-test canonize-bool
  (assert-equal '(or (and) (and (or) (or x1) (or (not x4)))) 
		(canonize '(and x1 (not x4)) :type 'bool))
  (assert-equal '(and (or) (or (and) (and x1) (and (not x4))))
		(canonize '(or x1 (not x4)) :type 'bool))
  (assert-equal '(or (and) (and x1)) (canonize 'x1 :type 'bool))
  (assert-equal  '(and (or) (or (and) (and (or) (or x1) (or x2)) (and x3)))
		 (canonize '(or (and x1 x2) x3) :type 'bool)))
(define-test canonize-mixed-bool-num
  (assert-equal '(or (and) (and (< (+ 0 (* 1 (/ (+ 2) (+ 1))))
				   (+ 0 (* 1 (/ (+ 3) (+ 1)))))))
		(canonize '(< 2 3) :type 'bool)))
(define-test canonize-num
  (assert-equal '(+ 0 
		  (* 0 (/ (+ 0) (+ 1)))
		  (* 1 (/ (+ 0 (* 1 x)) (+ 1))))
		(canonize 'x :type 'num))
  (assert-equal '(+ 0 
		  (* 0 (/ (+ 0) (+ 1)))
		  (* 1 (/ (+ 0 (* 1 (sin (+ 0 
					    (* 0 (/ (+ 0) (+ 1)))
					    (* 1 (/ (+ 0 (* 1 x)) (+ 1)))))))
			(+ 1))))
		(canonize '(sin x)))
  (assert-equal '(+ 0
		  (* 0 (/ (+ 0) (+ 1)))
		  (* 1 (/ (+ 0 (* 1 x)) (+ 0 (* 1 y)))))
		(canonize '(/ x y)))
  (assert-equal '(+ 0
		  (* 0 (/ (+ 0) (+ 1)))
		  (* 1 (/ (+ 0 (* 1 (+ 0 (* 1 x)) (+ 0 (* 1 y))))
			(+ 1))))
		(canonize '(* x y)))
  (assert-equal '(+ 0 
		  (* 0 (/ (+ 0) (+ 1)))
		  (* 1 (/ (+ 0 (* 1 x)) (+ 1)))
		  (* 1 (/ (+ 0 (* 1 y)) (+ 1))))
		(canonize '(+ x y))))

(defun loci (fn expr &key (type (expr-type expr)) parents)
  (flet ((boolrec (&optional type)
	   (mapc (bindapp #'loci fn /1 :parents (cons expr parents)
			  (if type (list :type type)))
		 (cdr expr))))
    (decompose (expr type)
      (bool (literal)
	    (junctor (funcall fn expr parents)
		     (boolrec 'bool))
	    (t (boolrec))))))
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

(defun test-knob (list knob results)
  (dorepeat 100 (let ((n (random 4)))
		  (funcall (elt knob n))
		  (assert-equal (elt results n) list)))
  (funcall (elt knob 0)))
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
  (decompose (expr type)
    (bool
     (junctor
      (collecting ; all the knobs
	(let ((tovisit (copy-hash-table (gethash 'bool bindings))))
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
				     (litnegation it)))))) ; 2 negate
		     (cdr expr)))
	  (maphash-keys (lambda (x)
			  (collect (make-inserter-knob expr
						       x
						       (litnegation x))))
			  tovisit))))
     (t nil))))
