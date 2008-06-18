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
    (flet ((rec-canonize () 
	     (if op
		 (cons op (mapcar (bind #'canonize /1 :parent op) (cdr expr)))
		 expr))
	   (bool-structure (expr)
	     (if parent 
		 `(,(dual-bool-op parent) ,expr)
		 `(or (and) (and ,expr)))))
      (decompose (expr type)
	(bool (literal (bool-structure expr))
	      (junctor 
	       (let ((body `(,op (,(dual-bool-op op)) 
				 ,@(mapcar (bind #'canonize /1 :type 'bool 
						 :parent op)
					   (cdr expr)))))
		 (if parent body (list (dual-bool-op op) (list op) body))))
	      (t (bool-structure (rec-canonize))))
	(num expr)))))
(define-test canonize-bool
  (assert-equal '(or (and) (and (or) (or x1) (or (not x4)))) 
		(canonize '(and x1 (not x4)) :type 'bool))
  (assert-equal '(and (or) (or (and) (and x1) (and (not x4))))
		(canonize '(or x1 (not x4)) :type 'bool))
  (assert-equal '(or (and) (and x1)) (canonize 'x1 :type 'bool))
  (assert-equal  '(and (or) (or (and) (and (or) (or x1) (or x2)) (and x3)))
		 (canonize '(or (and x1 x2) x3) :type 'bool)))
(define-test canonize-mixed-bool-num
  (assert-equal '(or (and) (and (< 2 3))) (canonize '(< 2 3) :type 'bool)))

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

(defun neighbors-at (fn expr bindings &key (type (expr-type expr)))
  (decompose (expr type)
    (bool
     (junctor
      (let ((tovisit (copy-hash-table (gethash 'bool bindings))))
	(mapl (lambda (l)
		(let ((subexpr (car l)))
		  (awhen (extract-literal subexpr)
		    (remhash (litvariable it) tovisit)
		    (unless (extract-literal expr)
		      (rplaca l (identity-elem (car expr)))
		      (funcall fn expr)
		      (rplaca l (litnegation it))
		      (funcall fn expr)
		      (rplaca l subexpr)))))
	      (cdr expr))
	(rplacd expr (cons nil (cdr expr)))
	(maphash-keys (lambda (x)
			(rplaca (cdr expr) x)
			(funcall fn expr)
			(rplaca (cdr expr) (litnegation x))
			(funcall fn expr))
		      tovisit)
	(rplacd expr (cddr expr))))))
  expr)
(define-test neighbors-at-bool
  (flet ((test (against expr bindings)
	   (let* ((expr (canonize expr :type 'bool))
		  (tmp (copy-tree expr)))
	     (assert-equal
	      (setf against (sort against #'total-order))
	      (sort (collecting (upwards (bind #'neighbors-at
					       (lambda (expr2) 
						 (collect (copy-tree expr)))
					       /1
					       bindings)
					 expr))
		     #'total-order))
	     (assert-equal tmp expr))))
    (test '((or (and x) (and x))
	    (or (and (not x)) (and x))
	    (or (and) (not x))
	    (or (and) false))
	  'x
	  (init-hash-table `((bool ,(init-hash-table '((x t)))))))
    (test '((or (and x) (and x))
	    (or (and (not x)) (and x))
	    (or (and) (not x))
	    (or (and) false)
	    (or (and y) (and x))
	    (or (and (not y)) (and x))
	    (or (and) (and y x))
	    (or (and) (and (not y) x))
	    (or (not y) (and) (and x))
	    (or y (and) (and x)))
	  'x 
	  (init-hash-table `((bool ,(init-hash-table '((x t) (y t)))))))))

;how would we do two changes at once??

;;   (let ((bindings1 
;; 	((bindings2 (init-hash-table `((bool ,(init-hash-table 
;; 					       '((x t) (y t)))))))))

;;       (test '(x (not x) y (not y) (and x y) (and (not x) y) (and x (not y))
;; 	      (and (not x) (not y))  (or x y) (or (not x) y) (or x (not y))
;; 	      (or (not x) (not y)))
;; 	    '(and x y)))))
;; '(x (not x) y (not y) 
;; 			  (and x y) (and (not x) y) 
;; 			  (and x (not y)) (and (not x) (not y))
;; 			  (or x y) (or (not x) y) 
;; 			  (or x (not y)) (or (not x) (not y)))


;;   (unless (consp expr) nil)
;;   (if (associativep (car expr)
      
;; accepts-locus-p

;; (defun bool-hillclimber (expr vars acceptsp terminationp)
;;   (let ((expr (canonize 'bool expr))
;; 	(vars (mapcar (
;;     (while (not (terminationp expr))
;;       (let ((tmp (copy-tree expr)))
;; 	(blockn (loci (bind #'try-changes (lambda (expr) 
;; 						 (if (acceptsp tmp expr)
;; 						     (return)))
;; 				 /1)
;; 			   expr))))
;;     expr))

;; (
;; 	 (
;; (bind #'try-changes #'filter
;;   (let ((funset (
;;   (while (not terminationp)
;;     (
  


;; (defgeneric update (model expr score))
;; (defgeneric set-exemplar (model expr score))
;; (defgeneric acceptsp (model expr score))
;; (defgeneric sample (model))

;; (defclass greedy-hillclimber ()
;;   (best best-score))
;; (defmethod update ((model greedy-hillclimber) expr score)
;;   (declare (ignore model))
;;   (declare (ignore expr))
;;   (declare (ignore score)))
;; (defmethod set-exemplar ((model greedy-hillclimber) expr score)
;;   (setf (slot-value model 'best) expr)
;;   (setf (slot-value model 'best-score) score))
;; (defmethod acceptsp ((model greedy-hillclimber) expr score)
;;   (declare (ignore expr))
;;   (> score (slot-value model 'best-score)))
;; (defmethod sample ((model greedy-hillclimber))
;;   (let ((locus (uniform-sample (mapl-loci #'identity best))))
;;     (if (matches (car locus) (and or))
;; ;;   (labels ((sample-from (expr) 
;; ;; 	     (let ((len (length expr)))
;; ;; 	       (if (eq 
;; ;; ) we need access to free variables and types, etc. etc.

		   
;; (defun basic-search (model scorer terminationp acceptsp)
;;   (let ((effort 0)
;; 	(exemplar (exemplar model))
;;     (while (not (funcall terminationp model effort))
;;       (let ((change (peturb model)))
;; 	(if acceptsp 
;;     (try-improvement model scorer)))

		   

;; (defun basic-search (model scorer terminationp acceptsp)
;;   (let ((effort 0)
;; 	(exemplar (exemplar model))
;;     (while (not (funcall terminationp model effort))
;;       (let ((change (peturb model)))
;; 	(if acceptsp 
;;     (try-improvement model scorer)))

;; setf



;; (defun try-improvement ((model greedy-hillclimber) scorer)
;;   (


;;       (let* ((candidate (sample model))
;; 	     (candidate-score (funcall score candidate)))
;; 	(if (acceptsp model candidate candidate-score)
;; 	    (return (search model score terminationp candidate 
;; 			  :expr-score candidate-score)))
;; 	(update model candidate candidate-score)))))

;; (defun model-based-search (model score terminationp expr &key expr-score)
;;   (let ((expr-score (or expr-score (funcall score expr))))
;;     (update model expr expr-score)
;;     (if (funcall terminationp model) model)
;;     (set-exemplar model expr expr-score)
;;     (do* ((candidate (sample model) (sample model))
;; 	  (candidate-score (funcall score candidate) 
;; 			   (funcall score candidate)))
;; 	 ((funcall terminationp model) model)
;;       (if (acceptsp model candidate candidate-score)
;; 	  (return (search model score terminationp candidate 
;; 			  :expr-score candidate-score))
;; 	  (update model candidate candidate-score)))))


;; (defmacro define-enumerator (name arguments &body body)
;;   (let ((top (gensym))
;; 	(callback (gensym)))
;;     `(defun ,name (,callback ,@arguments)
;;        (block ,top 

;; (defun enum-exprs (expr type context ctrl &key root parents))

;; (define-enumerator bool-neighborhood (ctrl context expr &key parents)
;;   (if (not (consp expr))
;;       (enum-exprs 'bool context ctrl)
;;       (case (car expr)
;; 	((and or)
;; 	 (enum-exprs (lambda (subexpr) 

;; 'bool context 
;; 		     (mkctrl ctrl :distance (distance context))
;; 		     :root expr)
;; 	 (bool-neighborhood 
	 
      
      
;;   (case (car

	  
;; 	  (set-examplar candidate model)
;; 	(context (mkcontext :location expr :
;;     (aif (greedy-draw neighborhood
;;     (generate (lambda (neighbor)
;; 	     (
;;     (aif (funcall neighborhood sol)
;; 	 (if (< best it) (search
       


;; analagous to define-reduction have a define-knob-creator

;; is it important to allof functions that generate reductions?

;; (defun kick (program find-move)
;;   (multiple-value-bind (siblings locus move) (funcall find-move program nil)
;;     (

;; (let (())
;;   (define-knob-creator subtree-inserter (root siblings locus)
;;     :nondeterministic t
;;     :condition (progn
;; 		 (setf (cdr locus) (cons (rand-tree) (cdr locus)))
;; 		 (let ((result (eq (normalize-and-reduce root) root)))
;; 		   (setf (cdr locus) (cddr locus))
;; 		   result))
;;     :arity
				
;; 	    normalize-and-reduce 		 
		       
;; 		      (

