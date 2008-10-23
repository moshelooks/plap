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

(defun map-neighbors (fn expr context type)
  (map-knobs (lambda (knob) (map-knob-settings (bind fn expr) knob))
	     expr context type)
  expr)

(defun enum-neighbors (expr context type)
  (collecting (map-neighbors (lambda (expr) (collect (canon-clean expr)))
			     expr context type)))
(define-test enum-neighbors
  (flet ((test (neighbors expr type vars &optional nocanon &aux tmp)
	   (setf expr (sexpr2p expr)
		 expr (if nocanon expr (canonize expr *empty-context* type))
		 tmp (p2sexpr expr))
	   (with-bound-types *empty-context* vars (ntimes (length vars) type)
	     (assert-equality
	      #'set-equal neighbors
	      (mapcar #'p2sexpr (enum-neighbors expr *empty-context* type)))
	     (assert-equal tmp (p2sexpr expr)))))
    ;; bool
    (test '((or false (and false))
	    (or false (and (not x)))
	    (or (and x) x)
	    (or (and (not x)) x))
	  'x 'bool '(x))
    (test '((or false (and false))
	    (or false (and x))
	    (or (and x) (not x))
	    (or (and (not x)) (not x)))
	  '(not x) 'bool '(x))
    (test '((or false (and false))
	    (or false (and (not x)))
	    (or (and y) x)
	    (or (and (not y)) x)
	    (or (and x) x)
	    (or (and (not x)) x)
	    (or y false x)
	    (or (not y) false x)
	    (or false (and y x))
	    (or false (and (not y) x)))
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
    
(defun weak-kick-until (pred n knobs)
  (print* 'wku n)
  (weak-kick n knobs)
  (unless (funcall pred)
    (weak-kick-until pred n knobs)))

(defun find-neighbor-if (fn simplifier expr context type)
  (map-neighbors (lambda (expr &aux
			  (simplified (funcall simplifier (canon-clean expr))))
		   (when (funcall fn simplified)
		     (print* 'improved-to simplified)
		     (return-from find-neighbor-if simplified)))
		 expr context type)
  nil)

(defun hillclimb (expr context type simplifier acceptsp terminationp)
 (do ((canonical (canonize expr context type) (canonize expr context type)))
      ((funcall terminationp expr) expr)
    (aif (find-neighbor-if (lambda (nexpr) 
			     (or (funcall acceptsp expr nexpr)
				 (funcall terminationp nexpr)))
			   simplifier canonical context type)
	 (setf expr it)
	 (progn (print* 'canonical canonical)
	 (let* ((knobs (enum-knobs canonical context type))
		(nknobs (length knobs)))
	   (print* 'local-minimum expr nknobs)
	   (print* 'fck context (length (keys-to-list (symbols-with-type bool context))))
	   (weak-kick-until 
	    (lambda () 
	      (not (eq (setf expr (funcall simplifier 
					   (canon-clean canonical))) 'nan)))
	    (if (< 2 nknobs) (+ 2 (random (- nknobs 2))) nknobs) knobs)
	   (print* 'kicked-to canonical expr))))))
(defun make-count-or-score-terminator (count score score-target)
  (lambda (expr) 
;    (print* 'nevals count)
    (or (> 0 (decf count)) (>= (funcall score expr) score-target))))
(defun make-greedy-scoring-acceptor (score)
  (lambda (from to)
    (< (funcall score from) (funcall score to))))

;; ; vars should be sorted
(defun make-truth-table-scorer (target-tt vars)
  (lambda (expr)
    (- (truth-table-hamming-distance target-tt (truth-table expr vars)))))

(defun bool-hillclimb-with-target-fn 
    (target-fn nsteps &aux (vars (fn-args target-fn))
     (scorer (make-truth-table-scorer (truth-table (fn-body target-fn) vars)
				      vars)))
  (with-bound-type *empty-context* vars bool
    (hillclimb 'true *empty-context* 'bool 
	       (bind #'reduct /1 *empty-context* bool)
	       (make-greedy-scoring-acceptor scorer)
	       (make-count-or-score-terminator nsteps scorer 0))))

;; (defun make-num-abs-scorer 
;;     (target-fn context test-values &aux (args (fn-args target-fn))
;;      (targets (mapcar (bind #'papply target-fn context /1) test-values)))
;; (let ((best -99999))
;;   (lambda (expr)
;;     (blockn
;;      (let ((res
;; 	    (- (reduce
;; 		#'+ (mapcar 
;; 		     (lambda (test target)
;; 		       (with-bound-symbols context args test
;; 			 (abs (- (let ((res (peval expr context)))
;; 				   (if (eq 'nan res)
;; 				       (return most-negative-single-float)
;; 				       res))
;; 				 target))))
;; 			 test-values targets)))))
;; 	       (when (> res best) (setf best res) (print* 'new-best res))
;; 	       res)))))

;; (defun num-hillclimb-with-target-fn 
;;     (target-fn test-values nsteps &aux (context (make-context))
;;      (scorer (make-num-abs-scorer target-fn context test-values)))
;;   (mapc (bind #'bind-type context /1 'num) (fn-args target-fn))
;;   (hillclimb 0 context 'num #'maxima-reduce
;; 		 (make-greedy-scoring-acceptor scorer)
;; 		 (make-count-or-score-terminator nsteps scorer -0.01)))


;; ;;;;;;;;adkan

;; ;; (defun make-pair-scorer (pairs var)
;; ;;   (lambda (expr)
;; ;;     (blockn
;; ;;       (- (reduce #'+ (mapcar (lambda (p &aux 
;; ;; 				      (d (abs (- (peval expr 
;; ;; 							    `((,var ,(car p))))
;; ;; 						 (cadr p)))))
;; ;; 			       (if (> d 1) (return (* -2 (length pairs))) d))
;; ;; 			     pairs))))))

;; ;; (defun num-hillclimb-with-target-pairs (target-pairs nsteps)
;; ;;   (let ((vars '(x))
;; ;; 	(scorer (make-pair-scorer target-pairs 'x)))
;; ;;     (hillclimb (canonize 0) vars (make-greedy-scoring-acceptor scorer)
;; ;; 	       (make-count-or-score-terminator nsteps scorer -0.01))))
				 

;; ;; (
;; ;; 	 (
;; ;; (bind #'try-changes #'filter
;; ;;   (let ((funset (
;; ;;   (while (not terminationp)
;; ;;     (
  


;; ;; (defgeneric update (model expr score))
;; ;; (defgeneric set-exemplar (model expr score))
;; ;; (defgeneric acceptsp (model expr score))
;; ;; (defgeneric sample (model))

;; ;; (defclass greedy-hillclimber ()
;; ;;   (best best-score))
;; ;; (defmethod update ((model greedy-hillclimber) expr score)
;; ;;   (declare (ignore model))
;; ;;   (declare (ignore expr))
;; ;;   (declare (ignore score)))
;; ;; (defmethod set-exemplar ((model greedy-hillclimber) expr score)
;; ;;   (setf (slot-value model 'best) expr)
;; ;;   (setf (slot-value model 'best-score) score))
;; ;; (defmethod acceptsp ((model greedy-hillclimber) expr score)
;; ;;   (declare (ignore expr))
;; ;;   (> score (slot-value model 'best-score)))
;; ;; (defmethod sample ((model greedy-hillclimber))
;; ;;   (let ((locus (uniform-sample (mapl-loci #'identity best))))
;; ;;     (if (matches (car locus) (and or))
;; ;; ;;   (labels ((sample-from (expr) 
;; ;; ;; 	     (let ((len (length expr)))
;; ;; ;; 	       (if (eq 
;; ;; ;; ) we need access to free variables and types, etc. etc.

		   
;; ;; (defun basic-search (model scorer terminationp acceptsp)
;; ;;   (let ((effort 0)
;; ;; 	(exemplar (exemplar model))
;; ;;     (while (not (funcall terminationp model effort))
;; ;;       (let ((change (peturb model)))
;; ;; 	(if acceptsp 
;; ;;     (try-improvement model scorer)))

		   

;; ;; (defun basic-search (model scorer terminationp acceptsp)
;; ;;   (let ((effort 0)
;; ;; 	(exemplar (exemplar model))
;; ;;     (while (not (funcall terminationp model effort))
;; ;;       (let ((change (peturb model)))
;; ;; 	(if acceptsp 
;; ;;     (try-improvement model scorer)))

;; ;; setf



;; ;; (defun try-improvement ((model greedy-hillclimber) scorer)
;; ;;   (


;; ;;       (let* ((candidate (sample model))
;; ;; 	     (candidate-score (funcall score candidate)))
;; ;; 	(if (acceptsp model candidate candidate-score)
;; ;; 	    (return (search model score terminationp candidate 
;; ;; 			  :expr-score candidate-score)))
;; ;; 	(update model candidate candidate-score)))))

;; ;; (defun model-based-search (model score terminationp expr &key expr-score)
;; ;;   (let ((expr-score (or expr-score (funcall score expr))))
;; ;;     (update model expr expr-score)
;; ;;     (if (funcall terminationp model) model)
;; ;;     (set-exemplar model expr expr-score)
;; ;;     (do* ((candidate (sample model) (sample model))
;; ;; 	  (candidate-score (funcall score candidate) 
;; ;; 			   (funcall score candidate)))
;; ;; 	 ((funcall terminationp model) model)
;; ;;       (if (acceptsp model candidate candidate-score)
;; ;; 	  (return (search model score terminationp candidate 
;; ;; 			  :expr-score candidate-score))
;; ;; 	  (update model candidate candidate-score)))))


;; ;; (defmacro define-enumerator (name arguments &body body)
;; ;;   (let ((top (gensym))
;; ;; 	(callback (gensym)))
;; ;;     `(defun ,name (,callback ,@arguments)
;; ;;        (block ,top 

;; ;; (defun enum-exprs (expr type context ctrl &key root parents))

;; ;; (define-enumerator bool-neighborhood (ctrl context expr &key parents)
;; ;;   (if (not (consp expr))
;; ;;       (enum-exprs 'bool context ctrl)
;; ;;       (case (car expr)
;; ;; 	((and or)
;; ;; 	 (enum-exprs (lambda (subexpr) 

;; ;; 'bool context 
;; ;; 		     (mkctrl ctrl :distance (distance context))
;; ;; 		     :root expr)
;; ;; 	 (bool-neighborhood 
	 
      
      
;; ;;   (case (car

	  
;; ;; 	  (set-examplar candidate model)
;; ;; 	(context (mkcontext :location expr :
;; ;;     (aif (greedy-draw neighborhood
;; ;;     (generate (lambda (neighbor)
;; ;; 	     (
;; ;;     (aif (funcall neighborhood sol)
;; ;; 	 (if (< best it) (search
       


;; ;; analagous to define-reduction have a define-knob-creator

;; ;; is it important to allof functions that generate reductions?

;; ;; (defun kick (program find-move)
;; ;;   (mvbind (siblings locus move) (funcall find-move program nil)
;; ;;     (

;; ;; (let (())
;; ;;   (define-knob-creator subtree-inserter (root siblings locus)
;; ;;     :nondeterministic t
;; ;;     :condition (progn
;; ;; 		 (setf (cdr locus) (cons (rand-tree) (cdr locus)))
;; ;; 		 (let ((result (eq (normalize-and-reduce root) root)))
;; ;; 		   (setf (cdr locus) (cddr locus))
;; ;; 		   result))
;; ;;     :arity
				
;; ;; 	    normalize-and-reduce 		 
		       
;; ;; 		      (


;; ;; (defun loci (fn expr &key (type (expr-type expr)) parents)
;; ;;   (flet ((boolrec (&optional type)
;; ;; 	   (mapc (bindapp #'loci fn /1 :parents (cons expr parents)
;; ;; 			  (if type (list :type type)))
;; ;; 		 (cdr expr))))
;; ;;     (ecase type
;; ;;       (bool
;; ;;        (decompose-bool expr
;; ;; 	 (literal)
;; ;; 	 (junctor (funcall fn expr parents)
;; ;; 		  (boolrec 'bool))
;; ;; 	 (t (boolrec)))))))
;; ;; (define-test loci-bool
;; ;;   (assert-equal '(((and (or x) (or y)))
;; ;; 		  ((or x) (and (or x) (or y)))
;; ;; 		  ((or y) (and (or x) (or y))))
;; ;; 		(collecting (loci (lambda (expr parents) 
;; ;; 					 (collect (cons expr parents)))
;; ;; 				       '(and (or x) (or y))))))


;; ;; ;accepts-locus-p
;; ;; (defun validate (expr bindings)
;; ;;   (ecase (expr-type expr)
;; ;;     (bool
;; ;;      (if (literalp expr)
;; ;; 	 (assert (or (matches expr (true false))
;; ;; 		     (gethash (litvariable expr) (gethash 'bool bindings)))
;; ;; 		 () "no binding for variable ~S" (litvariable expr))
;; ;; 	 (progn (assert (junctorp expr) () "~S is not a juctor-expr" expr)
;; ;; 		(mapc (bind #'validate /1 bindings) (cdr expr)))))
;; ;;     (num
;; ;;      t)))

;; ;; (defun map-neighbors-at (fn expr context type)
;; ;;   (mapc (lambda (knob)
;; ;; 	  (map nil (lambda (setting) 
;; ;; 		     (funcall setting)
;; ;; 		     (funcall fn expr))
;; ;; 	       (subseq knob 1))
;; ;; 	  (funcall (elt knob 0)))
;; ;; 	(knobs-at expr context type))
;; ;;   expr)

;; ;; (defun map-neighbors (fn expr context type &aux (fn (bind fn expr)))
;; ;;   (labels ((rec (subexpr type) 
;; ;; 	     (when (consp subexpr)
;; ;; 	       (map-neighbors-at fn subexpr context type)
;; ;; 	       (mapc #'rec (args subexpr) (arg-types subexpr context type)))))
;; ;;      (rec expr type))
;; ;;   nil)
