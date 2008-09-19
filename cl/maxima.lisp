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

(defun to-maxima (expr) ;; inplace
  (when (consp expr)
    (mapc #'to-maxima (cdr expr))
    (aif (case (caar expr)
	   (+ 'maxima::mplus)
	   (* 'maxima::mtimes)
	   (/ (setf (cddr expr) `(((maxima::mexpt) ,(caddr expr) -1)))
	      'maxima::mtimes)
	   (sin 'maxima::%sin)
	   (exp 'maxima::$exp)
	   (log (setf (cdr expr) (ncons (cons '(maxima::mabs) (cdr expr))))
		'maxima::%log))
	 (setf (caar expr) it)))
  expr)
(defun from-maxima (expr) ;; destructive
  (unless (atom expr)
    (case (caar expr) 
      (maxima::rat (setf expr (float (/ (cadr expr) (caddr expr)))))
      (maxima::mabs (return-from from-maxima (from-maxima (cadr expr))))
      (maxima::mexpt (assert (or (eq (cadr expr) 'maxima::$%e)
				 (eql (cadr expr) 2.718281828459045)))
		     (setf (caar expr) 'exp)
		     (setf (cdr expr) (cddr expr)))
      (maxima::mplus (setf (caar expr) '+))
      (maxima::mtimes (setf (caar expr) '*))
      (maxima::%sin (setf (caar expr) 'sin))
      (maxima::mexp (setf (caar expr) 'exp))
      (maxima::%log (setf (caar expr) 'log))
      (maxima::$exp (setf (caar expr) 'exp)))
    (setf (cdar expr) nil)
    (nmapcar #'from-maxima (cdr expr)))
  expr)
(define-test to-from-maxima
  (flet ((test (expr &aux (orig (copy-tree expr)))
	   (assert-equal orig (from-maxima (to-maxima expr)))))
    (let ((exprs
	   '(((EXP) X3)
	     ((SIN) 0)
	     ((SIN) 0.8206447)
	     ((+) X3 X1)
	     ((*) X1 ((EXP) ((*) -1.0 ((LOG) X2))))
	     ((EXP) X1)
	     ((+) X3 0.5202172)
	     ((EXP) 1)
	     ((+) 0.518894 X2)
	     ((EXP) 1)
	     ((*) X1 ((EXP) ((*) -1.0 ((LOG) X3))))
	     ((EXP) X3)
	     0.5054184
	     ((+) X3 X1)
	     ((*) X1 ((EXP) ((*) -1.0 ((LOG) 1))))
	     ((*) 1 ((EXP) ((*) -1.0 ((LOG) X3))))
	     ((LOG) X3) 
	     ((*) X1 0.3784288 ((EXP) ((*) -1.0 ((LOG) ((LOG) ((EXP) X1))))))
	     ((SIN)
	      ((SIN)
	       ((*) 0.518894 
		((EXP) ((*) -1.0 ((LOG) 1)))
		((EXP) ((*) -1.0 ((LOG) ((+) 0.9713698 X2))))
		((EXP) ((*) -1.0
			((LOG) ((LOG) ((*) X2 
				       ((EXP) ((*) -1.0 ((LOG) X2)))))))))))
	     ((*) X2 ((LOG) ((EXP) ((EXP) 0.1124193))))
	     ((EXP) ((+) ((LOG) ((EXP) ((SIN) 0)))
		     ((*) ((*) ((LOG) 0) ((EXP) 
					  ((*) -1.0 ((LOG) ((+) 0 X3))))) 
		      ((LOG) ((SIN) 0.2528254)))))
	     ((*) 1 ((EXP) ((*) -1.0 ((LOG) ((+) 0.6924095 0.9507193)))))
	     ((*) X3 ((EXP) ((*) -1.0 ((LOG) X3)))))))
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
;    (print* 'wtf expr)
    (let* ((mexpr (to-maxima (copy-tree expr)))
	   (reduced-mexpr (full-mreduce mexpr))
	   (is-equal (equalp mexpr reduced-mexpr)))
;      (print* mexpr reduced-mexpr)
      (from-maxima mexpr)
      (if is-equal mexpr (from-maxima (copy-tree reduced-mexpr))))))
