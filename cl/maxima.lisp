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
			   (print* 'ppp x)
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
      (maxima::mabs (setf (car expr) (caadr expr))
		    (setf (cdr expr) (cdadr expr))
		    (return-from from-maxima (from-maxima expr)))
      (maxima::mexpt (assert (or (eq (cadr expr) 'maxima::$%e)
				 (eql (cadr expr) 2.718281828459045)))
		     (setf (caar expr) 'exp)
		     (setf (cdr expr) (cddr expr)))
      (maxima::mplus (setf (caar expr) '+))
      (maxima::mtimes (setf (caar expr) '*))
      (maxima::%sin (setf (caar expr) 'sin))
      (maxima::mexp (setf (caar expr) 'exp))
      (maxima::%log (setf (caar expr) 'log)))
    (setf (cdar expr) nil)
    (mapcar #'from-maxima (cdr expr)))
  expr)

(define-reduction maxima-reduce (expr)
  :type num
  :assumes (maxima-prepare)
  :action 
  (labels ((mreduce (mexpr) (maxima::$float (maxima::simplify mexpr)))
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
						 (cadr mexpr))))
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
					      (print* 'xxx mexpr)
					      (mung-expts (mreduce mexpr)))
					    mexpr :test #'equalp)))
	       (system::simple-floating-point-overflow ())
	       (system::simple-arithmetic-error ()))
	     'nan)
	   (all-simped-p (mexpr)
	     (or (atom mexpr) (and (eq (cadar mexpr) 'maxima::simp)
				   (every #'all-simped-p (cdr mexpr))))))
    (let* ((mexpr (to-maxima expr))
	   (reduced-mexpr (full-mreduce mexpr)))
      (assert (all-simped-p reduced-mexpr) () 
	      "can't find simp in cadar, expr is ~S" reduced-mexpr)
      (from-maxima (if (equalp mexpr reduced-mexpr)
		       mexpr 
		       (copy-tree reduced-mexpr))))))
