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

;;; to avoid trying to factor expressions that produce pathological behavior
;;; when one tries to factor them (e.g. -0.2+x^0.70086)
;;; in the future it would be nice to do a more sophisticated check
(in-package :maxima)
(defun factor-if-small (expr) expr)
(setf $ratprint nil) ; to prevent maxima from spewing out warnings
(setf errorsw t)     ; and error messages
(setf errrjfflag t)  ; and more error messages
(setf $numer t) ; to force evaluation of e.g. exp(sin(sin(2)))
(in-package :plop)

;;; blocks non-numeric expressions from being munged by maxima
(define-reduction maxima-prepare (expr)
    :type num
    :order upwards
    :action (progn (mapc (lambda (x)
			   (unless (or (atom x) (simpp x 'maxima-prepare))
			     (setf (mark 'maxima::simp x) nil)))
			 (args expr))
		   expr)
    :preserves all)

(defun to-maxima (expr)
  (if (atom expr) expr
      (pcons
       (acase (caar expr)
	 (/ (assert nil () "to-maxima ~S" it))
	 (+ 'maxima::mplus)
	 (* 'maxima::mtimes)
	 (sin 'maxima::%sin)
	 (exp (return-from to-maxima
		(pcons 'maxima::mexpt
		       (list 'maxima::$%e (to-maxima (cadr expr)))
		       (markup expr))))
	 (log (return-from to-maxima 
		(pcons 'maxima::%log 
		       (ncons (pcons 'maxima::mabs 
				     (ncons (to-maxima (arg0 expr)))))
		       (markup expr))))
	 (t it))
       (mapcar #'to-maxima (args expr))
       (markup expr))))
(defun from-maxima (expr)
  (if (atom expr) expr
      (pcons 
       (acase (caar expr) 
	 ((maxima::rat maxima::$exp maxima::mexp)
	  (assert nil () "from-maxima ~S" it))
	 (maxima::mabs (return-from from-maxima (from-maxima (cadr expr))))
	 (maxima::mexpt (assert (or (eql (cadr expr) 2.718281828459045)
				    (eq (cadr expr) 'maxima::$%e)))
			(return-from from-maxima
			  (pcons 'exp (ncons (from-maxima (caddr expr))))))
	 (maxima::mplus '+)
	 (maxima::mtimes '*)
	 (maxima::%sin 'sin)
	 (maxima::%log 'log)
	 (t it))
       (mapcar #'from-maxima (cdr expr))
       (remove 'maxima::simp (cdar expr)))))
(define-test to-from-maxima
  (flet ((test (expr &aux (orig (copy-tree expr)))
	   (assert-equal orig (from-maxima (to-maxima expr)))))
    (let ((exprs
	   '(((exp) x3)
	     ((sin) 0)
	     ((sin) 0.8206447)
	     ((+) x3 x1)
	     ((*) x1 ((exp) ((*) -1.0 ((log) x2))))
	     ((exp) x1)
	     ((+) x3 0.5202172)
	     ((exp) 1)
	     ((+) 0.518894 x2)
	     ((exp) 1)
	     ((*) x1 ((exp) ((*) -1.0 ((log) x3))))
	     ((exp) x3)
	     0.5054184
	     ((+) x3 x1)
	     ((*) x1 ((exp) ((*) -1.0 ((log) 1))))
	     ((*) 1 ((exp) ((*) -1.0 ((log) x3))))
	     ((log) x3) 
	     ((*) x1 0.3784288 ((exp) ((*) -1.0 ((log) ((log) ((exp) x1))))))
	     ((sin)
	      ((sin)
	       ((*) 0.518894 
		((exp) ((*) -1.0 ((log) 1)))
		((exp) ((*) -1.0 ((log) ((+) 0.9713698 x2))))
		((exp) ((*) -1.0
			((log) ((log) ((*) x2 
				       ((exp) ((*) -1.0 ((log) x2)))))))))))
	     ((*) x2 ((log) ((exp) ((exp) 0.1124193))))
	     ((exp) ((+) ((log) ((exp) ((sin) 0)))
		     ((*) ((*) ((log) 0) ((exp) 
					  ((*) -1.0 ((log) ((+) 0 x3))))) 
		      ((log) ((sin) 0.2528254)))))
	     ((*) 1 ((exp) ((*) -1.0 ((log) ((+) 0.6924095 0.9507193)))))
	     ((*) x3 ((exp) ((*) -1.0 ((log) x3)))))))
      (mapc #'test exprs))))

(define-reduction maxima-reduce (expr)
  :type num
  :assumes (maxima-prepare)
  :obviates (eval-const)
  :action 
  (labels ((mreduce (mexpr) (maxima::simplify (maxima::$float mexpr)))
	   (mung-expts (mexpr) (mung-helper mexpr) mexpr)
	   (mung-helper (mexpr &aux (munged nil))
	     (when (consp mexpr)
	       (mapc (lambda (subexpr) (if (mung-helper subexpr)
					   (setf munged t)))
		     (cdr mexpr))
	       (when (and (eq (caar mexpr) 'maxima::mexpt)
			  (not (or (eq (cadr mexpr) 'maxima::$%e)
				   (eql (cadr mexpr) 2.718281828459045))))
		 (setf munged t)
		 (setf (caddr mexpr) (list (ncons 'maxima::mtimes)
					   (caddr mexpr)
					   (list (ncons 'maxima::%log)
						 (list (ncons 'maxima::mabs)
						       (cadr mexpr)))))
		 (setf (cadr mexpr) 'maxima::$%e))
	       (when munged
		 (rplaca mexpr (ncons (caar mexpr))))) ;nix simp flag
	     munged)
	   (full-mreduce (mexpr)
	     (handler-case (catch* (maxima::raterr 
				    maxima::errorsw
				    maxima::macsyma-quit)
			     (return-from full-mreduce
			       (fixed-point (lambda (mexpr)
					      (mung-expts (mreduce mexpr)))
					    (maxima::simplify mexpr)
					    :test #'equalp)))
	       (system::simple-floating-point-overflow ())
	       (system::simple-arithmetic-error ()))
	     'nan)
	   (all-simped-p (mexpr)
	     (or (atom mexpr) (and (eq (cadar mexpr) 'maxima::simp)
				   (every #'all-simped-p (cdr mexpr))))))
    (let* ((mexpr (to-maxima expr))
	   (reduced (full-mreduce mexpr)))
      (if (pequal mexpr reduced)
	  expr
	  (from-maxima reduced)))))
    


;;     (to-maxima expr)
;;     (let ((reduced (full-mreduce (copy-tree expr)))) ;fixme copy
;;       (if (pequal expr reduced)
;; 	  (from-maxima expr) 
;; 	  (prog1 (from-maxima (copy-tree reduced))
;; 	    (from-maxima expr))))))

;;     (let ((mexpr (copy-tree expr)))
;;       (to-maxima mexpr)
;;       (let* ((reduced-mexpr (full-mreduce mexpr)))
;; 	(if (equalp mexpr reduced-mexpr)
;; 	    expr 
;; 	    (from-maxima (copy-tree reduced-mexpr)))))))
