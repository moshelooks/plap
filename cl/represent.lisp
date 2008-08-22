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

(defdefbytype defknobs knobs-at)

;;; call enum-knobs to get a list of all knobs for a particular expression - 
;;; just calling knobs-at recursively is not enough because we have to add and
;;; remove local variables from the context
(defun map-knobs (fn expr context type)
  (if (eqfn expr 'lambda)
      (dbind (arg-names body) (args expr)
	(dbind (arg-types return-type) (cdr type)
	  (mapc (bind #'bind-type context /1 /2) arg-names arg-types)
	  (map-knobs fn body context return-type)
	  (mapc (bind #'unbind-type context /1) arg-names)))
      (progn (mapc fn (knobs-at expr context type))
	     (when (consp expr)
	       (mapc (bind #'map-knobs fn /1 context /2)
		     (args expr) (arg-types expr context type))))))
(defun enum-knobs (expr context type)
  (collecting (map-knobs (collector) expr context type)))

(defun map-knob-settings (fn knob)
  (map nil (lambda (setting) (funcall setting) (funcall fn))
       (subseq knob 1))
  (funcall (elt knob 0)))

(defknobs bool (expr context)
  (when (junctorp expr)
    (collecting
      (let ((tovisit (copy-hash-table (get-symbols 'bool context))))
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
			(collect (make-inserter-knob expr x (litnegation x))))
		      tovisit)))))

(defknobs num (expr context)
  (when (ring-op-p expr)
    (collecting     
      (let* ((tovisit (copy-hash-table (get-symbols 'num context)))
	     (x (cadr expr))
	     (body (if (numberp x)
		       (let ((e1 (big-epsilon x)) (e2 (little-epsilon x)))
			 (collect (make-replacer-knob (cdr expr)
						      (+ x e1) (- x e1)
						      (- x e2) (+ x e2)))
			 (cdr expr))
		       expr))
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
			(let ((e1 (big-epsilon x))
			      (e2 (little-epsilon x)))
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
		      tovisit)))))

(defknobs tuple (expr context type)
  (declare (ignore expr context type))
  nil)

(defknobs list (expr context type)
  (when (eqfn expr 'if)
    (assert (or (eq (arg0 expr) 'true) (eq (arg0 expr) 'false)))
    (cons (apply #'make-replacer-knob (args expr) (bool-dual (arg0 expr))
		 (mapcan (lambda (var) (list var `(not ,var)))
			 (keys-to-list (get-symbols bool context))))
	  (when (or (atom (arg1 expr)) (atom (arg2 expr)))
	    (let ((xs (keys-to-list (get-symbols type context))))
	      (flet ((mkknob (arglist)
		       (apply #'make-replacer-knob arglist
			      (aif (car arglist) (remove it xs) xs))))
		(collecting (when (atom (arg1 expr))
			      (collect (mkknob (cdr (args expr)))))
			    (when (atom (arg2 expr))
			      (collect (mkknob (cddr (args expr))))))))))))

;; (defknobs functions (expr context type)
;;   (declare (ignore expr context type))
;;   nil)
