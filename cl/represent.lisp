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
  (let ((op (if (consp expr) (car expr)))
	(op-offsets '(0 1 0))
	(ops '(exp log sin)))
    (labels
	((canonize-children (expr)
	   (if (consp expr)
	       (cons (car expr) (mapcar (bind #'canonize /1 :parent op ;fixme
					      :type (or (expr-type /1) type))
					  (cdr expr)))
	       expr))
	 (bool-structure (expr)
	   (if parent 
	       (list (dual-bool-op parent) expr)
	       (list 'or (list 'and) (list 'and expr))))

	 (sum-of-products (o ws ts)
	   (nconc (list '+ o)
		  ;;;
		  (product-of-sums 0 nil nil)
		  (mapcar (lambda (weight term)
			    (multiple-value-bind (o ws ts)
				(split-product-of-sums term)
			      (assert (eql o 1))
			      (product-of-sums weight ws ts)))
			  ws ts)))
	 (product-of-sums (o ws ts)
	   (nconc (list '* o)
		  ;;;
		  (sum-of-products 0 nil nil)
		  (mapcar (lambda (weight term)
			    (nconc (list '+ weight)
				   (decompose-num term
				     (+ (cdr term))
				     (t term))))
					
			  ws ts)
		  
			   (mapcar (bind #'list '+ 1 (weighted-op /1 /2))
				   op-offsets ops))
		    (mapcar #'product-of-sums ws ts))))

	 (weighted-op (offset op)
	   (list '* 0 (list op (nconc (list '+ offset)
				      (mapcar
				       (bind #'list '* 0 (list /1 ('+ /2)))
				       ops op-offsets)))))
	 (full-sum-of-products (expr)
	   (multiple-value-bind (o ws ts) (split-sum-of-products expr)
	     (nconc (list '+ o) (mapcar #'weighted-op ops op-offsets)
		    (nconc (list '* 0) 
			   (mapcar (bind #'list '+ 1 (weighted-op /1 /2))
				   op-offsets ops))
		    (mapcar #'product-of-sums ws ts))))
	 (product-of-sums (weight term)
	   (list '* weight
			      (let (
#'linear-weighted-op ops offsets)
			  
(lambda (op offset)
			      


(bind make-term /1 

	 (sum-of-products (expr)
	   (multiple-value-bind (o ws ts) (split-sum-of-products expr)
	     (nconc (list '+ offset (list '* 0)
		    (mapcar (lambda (weight term)
			      (assert (eq (car term) '*))
			      (assert (eq (cadr term 1)))
			      (assert (equal (caddr term '(+ 0))))
			      (list '* weight (cdddr term)))
			    ws (mapcar #'product-of-sums ts))
	   (product-of-sums (expr)
	     (multiple-value-bind (o ws ts) (split-product-of-sums expr)
	       (nconc (list '* o (list '+ 1))
		      (mapcar (lambda (weight term)
				(li
	     (ncons
	       
      (case type
	(bool
	 (decompose-bool expr
	   (literal (bool-structure expr))
	   (junctor (let ((body (nconc (list op (list (dual-bool-op op)))
				       (canonize-children expr))))
		      (if parent 
			  body 
			  (list (dual-bool-op op) (list op) body))))
	   (t (structure (canonize-children expr)))))
	(num 
	 (sum-of-products expr))
	     ((sum-of-products (expr)
		(multiple-value-bind (offset weights terms)
		    (get-sum-of-products expr)
		  (make-sum-of-products offset 
					(cons 0 weights) 
					(cons nil (mapcar #'product-of-sums terms)
	     (nconc (list offset (list '*))
		    (mapcar (lambda weight term

structure (expr)))
	   (sum-of-products expr)

	   (apply
	    #'full-p-of-s
	    (decompose-num expr
	      (constant `(,expr nil nil))
	      (* `(0 (1) (,(apply #'full-s-of-p 
				  (multiple-value-bind (offset weights terms)
				      (get-s-of-p expr)
			     offset weights (mapcar #'rec-canonize))))))
	     (t (multiple-value-bind (offset weights terms) 
		    (linear-decompose expr)
		  `(,offset ,weights ,(mapcar #'rec-canonize terms))))))))



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
      (case type
	(bool (bool-case expr
			 

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

(product-of-sums-decompose expr (offset weights terms)
  (product-of-sums-structure 

(* 1 (+ 0)

(* 3 x y)

(+ (*) (* (+ 3) (+ x) (+ y))

(product-of-sums (* x y (sin z))) -> 
  (* (+ 1) (+ 1 x) (+ y) (+ 1 (sin (sum-of-prods z))

(sums-of-products (+ x y (sin z))) -> 
  (+ (* 0) (* 1 x) (* 1 y) (+ 1 (sin (sum-of-prods z))

********
examples

x^2*y^2
log(x)*log(y)
(x+y)*x
(x+y)*(x+y)
x*log(x*x)

(+ (* (+) (+)) (* (+) (+)))

(+ x y 
   (* (exp (+ x y 
	      (* (exp (+ x y))
		 (log (+ x y))
		 (log (+ x y))
		 (sin (+ x y)))
	      (* (exp (+ x y))
		 (log (+ x y))
		 (log (+ x y))
		 (sin (+ x y)))))
      (log (+ x y 
	      (* (log (+ x y))
		 (sin (+ x y)))
	      (* (log (+ x y))
		 (sin (+ x y)))))

(* x y))) 
      (log (+ x y (* x y))) 
      (sin (+ x y (* x y))))
   (exp (+ x y
	   (* x y
	      (exp (+ x y (* x y))) 
	      (log (+ x y (* x y))) 
	      (sin (+ x y (* x y))))))
   (log (+ x y
	   (* x y
	      (exp (+ x y (* x y))) 
	      (log (+ x y (* x y))) 
	      (sin (+ x y (* x y))))))
   (sin (+ x y
	   (* x y
	      (exp (+ x y (* x y))) 
	      (log (+ x y (* x y))) 
	      (sin (+ x y (* x y)))))))

*****
(+ 0
   (* 0 (exp (+ 0 
		(* 0 (exp (+ 0)))
		(* 0 (log (+ 1)))
		(* 0 (sin (+ 0))))))
   (* 0 (log (+ 1
		(* 0 (exp (+ 0)))
		(* 0 (log (+ 1)))
		(* 0 (sin (+ 0))))))
   (* 0 (sin (+ 0
		(* 0 (exp (+ 0)))
		(* 0 (log (+ 1)))
		(* 0 (sin (+ 0))))))
   (* 0
      (+ 1 (* 0 (exp (+ 0 
			(* 0 (exp (+ 0)))
			(* 0 (log (+ 1)))
			(* 0 (sin (+ 0)))))))
      (+ 1 (* 0 (log (+ 1
			(* 0 (exp (+ 0)))
			(* 0 (log (+ 1)))
			(* 0 (sin (+ 0)))))))
      (+ 1 (* 0 (sin (+ 0
			(* 0 (exp (+ 0)))
			(* 0 (log (+ 1)))
			(* 0 (sin (+ 0)))))))))

*****

(+ x
   y
   (log (+ x y (* x y)))
   (exp (+ x y (* x y)))
   (sin (+ x y (* x y)))



(+ 0
   (* 0 x)
   (* 0 (exp (+ 0 
		(* 0 x)
		(* 0 (exp (+ 0 (* 0 x))))
		(* 0 (log (+ 1 (* 0 x))))
		(* 0 (sin (+ 0 (* 0 x)))))))
   (* 0 (log (+ 1
		(* 0 x)
		(* 0 (exp (+ 0 (* 0 x))))
		(* 0 (log (+ 1 (* 0 x))))
		(* 0 (sin (+ 0 (* 0 x)))))))
   (* 0 (sin (+ 0
		(* 0 x)
		(* 0 (exp (+ 0 (* 0 x))))
		(* 0 (log (+ 1 (* 0 x))))
		(* 0 (sin (+ 0 (* 0 x)))))))
   (* 0
      (+ 1 (* 0 x))
      (+ 1 (* 0 (exp (+ 0 
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))
      (+ 1 (* 0 (log (+ 1
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))
      (+ 1 (* 0 (sin (+ 0
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x)))))))))
   (* 1
      (+ 0 (* 1 x))
      (+ 1 (* 0 (exp (+ 0 
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))
      (+ 1 (* 0 (log (+ 1
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))
      (+ 1 (* 0 (sin (+ 0
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x)))))))))
   (* 1
      (+ 1 (* 0 x))
      (+ 1 (* 0 (exp (+ 0 
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))
      (+ 1 (* 0 (log (+ 1
			(* 0 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))
      (+ 0 (* 1 (sin (+ 0
			(* 1 x)
			(* 0 (exp (+ 0 (* 0 x))))
			(* 0 (log (+ 1 (* 0 x))))
			(* 0 (sin (+ 0 (* 0 x))))))))))


      (+ 1 (* 0 (exp (+ 0))))
      (+ 1 (* 0 (log (+ 1))))
      (+ 1 (* 0 (sin (+ 0)))))
   (* 0
      (+ 1 (* 0 (exp (+ 0))))
      (+ 1 (* 0 (log (+ 1))))
      (+ 1 (* 0 (sin (+ 0))))))

n variables gives

(((n+2)*3+n+2)*3+n+1)*2+4 knobs =
32+18+26n+4 knobs =
54+26n knobs.





;; (0              (+ (*) (*))
;; (x              (+ (*) (* x))
;; ((sin x)        (+ (*) (* (sin (+ (*) (* x))))))
;; ((* x y)        (+ (*) (* (+) (+ x) (+ y)))
;; ((+ x y)        (* (+) (+ (*) (* x) (* y)))
;; ((sin (log x))  (+ (*) (* (sin (+ (*) (* (log (+ (*) (* x)))))))))
;; ((* (+ x y) z)  (+ (*) (* (+) (+ (* x) (* y)) (+ z))))


;; (define-test canonize-num
;;   (assert-equal '(+ 0 
;; 		  (* 0 (exp (+ 0)))
;; 		  (* 0 (log (+ 1)))
;; 		  (* 0 (sin (+ 0))))
;; 		(canonize 0))
;;   (assert-equal '(+ 0 
;; 		  (* 0 (exp (+ 0)))
;; 		  (* 0 (log (+ 1)))
;; 		  (* 0 (sin (+ 0)))
;; 		  (* 
;; 		   (+ 0 (* 1 x))
;; 		   (+ 1 (* 0 (exp (+ 0))))
;; 		   (+ 1 (* 0 (log (+ 1))))
;; 		   (+ 1 (* 0 (sin (+ 0))))))
;; 		(canonize 'x :type 'num))
;;   (assert-equal '(+ 0 
;; 		  (* 0 (exp (+ 0)))
;; 		  (* 0 (log (+ 1)))
;; 		  (* 
;; 		   (+ 1 (* 0 (exp (+ 0))))
;; 		   (+ 1 (* 0 (log (+ 1))))
;; 		   (+ 0 (* 1 (sin (+ 0))))))
;; 		(canonize '(sin x)))
;;   (assert-equal '(+ 0 
;; 		  (* 0 (exp (+ 0)))
;; 		  (* 0 (log (+ 1)))
;; 		  (* 0 (sin (+ 0)))
;; 		  (* 
;; 		   (+ 0 
;; 		    (* 1 x)
;; 		    (* 0 (exp (+ 0)))
;; 		    (* 0 (log (+ 1))))
;; 		   (+ 1 (* 0 (sin (+ 0))))
;; 		    )
;; 		   (+ 0 (* 1 y))
;; 		   (+ 1 (* 0 (exp (+ 0))))
;; 		   (+ 1 (* 0 (log (+ 1))))
;; 		   (+ 1 (* 0 (sin (+ 0))))))
;; 		(canonize '(* x y)))
;;   (assert-equal '(+ 0 
;; 		  (* 0 (exp (+ 0)))
;; 		  (* 0 (log (+ 1)))
;; 		  (* 0 (sin (+ 0)))
;; 		  (* 
;; 		   (+ 0 (* 1 x))
;; 		   (+ 1 (* 0 (exp (+ 0))))
;; 		   (+ 1 (* 0 (log (+ 1))))
;; 		   (+ 1 (* 0 (sin (+ 0))))))

;; (+ 0 
;; 		  (* 0 (/ (+ 0) (+ 1)))
;; 		  (* 1 (/ (+ 0 (* 1 x)) (+ 1)))
;; 		  (* 1 (/ (+ 0 (* 1 y)) (+ 1))))
;; 		(canonize '(+ x y))))

;; 		   (+ 1 (* 0 (exp (+ 0))))
;; 		   (+ 1 (* 0 (log (+ 1))))
;; 		   (+ 1 (* 0 (sin (+ 0))))))

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
  (collecting
    (decompose (expr type)
      (bool
       (junctor
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
				     (litnegation it))))))    ; 2 negate
		     (cdr expr)))
	  (maphash-keys (lambda (x)
			  (collect (make-inserter-knob 
				    expr x (litnegation x))))
			tovisit)))))))
;;       (num
;;        ((* +) (let ((x (cadr expr)))
;; 		(when (numberp x)
;; 		  (let ((e1 (little-epsilon x)) 
;; 			(e2 (big-epsilon x)))
;; 		    (collect (make-replacer-knob (cdr expr) (+ x e1) (- x e1)
;; 						 (- x e2) (+ x e2)))))
;; 		(collect (make-inserter-knob (cdr expr)
		  
