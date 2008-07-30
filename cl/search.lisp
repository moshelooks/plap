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

(defun neighbors-at (fn expr bindings &key (type (expr-type expr)))
  (mapc (lambda (knob)
	  (map nil (lambda (setting) 
		     (funcall setting)
		     (funcall fn expr))
	       (subseq knob 1))
	  (funcall (elt knob 0)))
	(knobs-at expr bindings :type type))
  expr)

;;;fixme for nested varying types
(defun enum-neighbors (fn expr bindings &key (type (expr-type expr)))
  (upwards (bind #'neighbors-at (lambda (x)
				  (declare (ignore x))
				  (funcall fn expr))
		 /1 bindings :type type)
	   expr))

(define-test neighbors-at
  (flet ((test (against expr type bindings &optional nocanon)
	   (let* ((expr (if nocanon expr (canonize expr nil type)))
		  (tmp (copy-tree expr))
		  (bindings (init-type-map `((,type ,bindings)))))
	     (assert-equal
	      (setf against (sort (copy-seq against) #'total-order))
	      (sort (collecting (enum-neighbors
				 (lambda (expr2) 
				   (declare (ignore expr2))
				   (collect (copy-tree expr)))
				 expr
				 bindings))
		     #'total-order))
	     (assert-equal tmp expr))))
    ;; bool
    (test '((or (and x) (and x))
	    (or (and (not x)) (and x))
	    (or (and) (and (not x)))
	    (or (and) (and true)))
	  'x 'bool '(x))
    (test '((or (and x) (and (not x)))
	    (or (and (not x)) (and (not x)))
	    (or (and) (and x))
	    (or (and) (and true)))
	  '(not x) 'bool '(x))
    (test '((or (and x) (and x))
	    (or (and (not x)) (and x))
	    (or (and) (and (not x)))
	    (or (and) (and true))
	    (or (and y) (and x))
	    (or (and (not y)) (and x))
	    (or (and) (and y x))
	    (or (and) (and (not y) x))
	    (or (not y) (and) (and x))
	    (or y (and) (and x)))
	  'x 'bool '(x y))
    ;; num
    (flet ((buildnum (fn c) (list (funcall fn (+ c (little-epsilon c)))
				  (funcall fn (- c (little-epsilon c)))
				  (funcall fn (+ c (big-epsilon c)))
				  (funcall fn (- c (big-epsilon c))))))
      (test (nconc (buildnum (lambda (c) `(* ,c (+ 0 (* 1 x)))) 3)
		   (buildnum (lambda (c) `(* 3 (+ ,c (* 1 x)))) 0)
		   (buildnum (lambda (c) `(* 3 (+ 0 (* ,c x)))) 1)
		   (buildnum (lambda (c) `(* 3 (+ 1 (* ,c y)) (+ 0 (* 1 x))))
			     0)
		   (buildnum (lambda (c) `(* 3 (+ 0 (* 1 (+ 1 (* ,c y)) x))))
			     0)
		   (buildnum (lambda (c) `(* 3 (+ 0 (* ,c y) (* 1 x))))
			     0))
	    '(* 3 (+ 0 (* 1 x))) 'num '(x y) t))))

;;; a weak kick selects n knobs in a representation and randomly twiddles them
;;; to new settings
(defun weak-kick (n knobs)
  (mapc (lambda (knob)
	  (funcall (elt knob (1+ (random (1- (knob-arity knob)))))))
	(random-sample n knobs)))
      
;accepts-locus-p
(defun validate (expr bindings)
  (ecase (expr-type expr)
    (bool
     (if (literalp expr)
	 (assert (or (matches expr (true false))
		     (gethash (litvariable expr) (gethash 'bool bindings)))
		 () "no binding for variable ~S" (litvariable expr))
	 (progn (assert (junctorp expr) () "~S is not a juctor-expr" expr)
		(mapc (bind #'validate /1 bindings) (cdr expr)))))
    (num
     t)))

(defun hillclimb (expr vars acceptsp terminationp &aux
		  (expr (copy-tree expr)) (type (expr-type expr))
		  (bindings (init-type-map `((,type ,vars)))))
  (while (not (funcall terminationp expr))
    (let ((tmp (copy-tree expr)))
      (blockn (enum-neighbors (lambda (expr) 
				(validate expr bindings)
				(when (or (funcall acceptsp tmp expr)
					  (funcall terminationp expr))
				  (print* 'improved-to expr)
				  (return)))
			      expr
			      bindings
			      :type type))
      (when (equalp tmp expr)
	(print* "local minimum at " expr)
	(let* ((knobs (knobs-at expr bindings :type type))
	       (nknobs (length knobs)))
	  (print* nknobs knobs)
	  (weak-kick (if (< 2 nknobs)
			 (+ 2 (random (- nknobs 2)))
			 nknobs)
		     knobs)
	  (validate expr bindings)
	  (print* 'kicked-to expr))))
					;(return-from hillclimb expr)))
    (setf expr (canonize expr type)));fixme
  expr)

(defun make-count-or-score-terminator (count score score-target)
  (lambda (expr) 
    (print* 'nevals count)
    (or (> 0 (decf count)) (>= (funcall score expr) score-target))))
(defun make-greedy-scoring-acceptor (score)
  (lambda (from to)
    (< (funcall score from) (funcall score to))))

;;;fixme dangling junctors

; vars should be sorted
(defun make-truth-table-scorer (target vars)
  (lambda (expr)
    (let ((expr (dangling-junctors expr)))
      (- (truth-table-hamming-distance target (truth-table expr vars))))))

(defun bool-hillclimb-with-target-expr (target-expr nsteps)
  (let* ((vars (free-variables target-expr))
	 (scorer (make-truth-table-scorer (truth-table target-expr vars)
					  vars)))
    (hillclimb '(or (and) (and)) vars
	       (make-greedy-scoring-acceptor scorer)
	       (make-count-or-score-terminator nsteps scorer 0))))

(defun make-pair-scorer (pairs var)
  (lambda (expr)
    (blockn
      (- (reduce #'+ (mapcar (lambda (p &aux 
				      (d (abs (- (eval-expr expr 
							    `((,var ,(car p))))
						 (cadr p)))))
			       (if (> d 1) (return (* -2 (length pairs))) d))
			     pairs))))))

(defun num-hillclimb-with-target-pairs (target-pairs nsteps)
  (let ((vars '(x))
	(scorer (make-pair-scorer target-pairs 'x)))
    (hillclimb (canonize 0) vars (make-greedy-scoring-acceptor scorer)
	       (make-count-or-score-terminator nsteps scorer -0.01))))
				 

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
;;   (mvbind (siblings locus move) (funcall find-move program nil)
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


