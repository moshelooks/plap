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

(defun cons-cars (expr)
  (if (consp expr)
      (cons (ncons (car expr)) (mapcar #'cons-cars (cdr expr)))
      expr))
(defun to-maxima (expr) ;;; inplace - expr should be already cons-car'ed
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
(defun reduce-maxima-expr (mexpr)
  (labels ((has-expt (mexpr)
	     (and (consp mexpr)
		  (or (eq (caar mexpr) 'maxima::mexpt)
		      (find-if #'has-expt (cdr mexpr)))))
	   (reduce () 
	     (setf mexpr (maxima::$float (maxima::simplify (copy-tree mexpr)))))
	   (mung-expts (mexpr &aux (munged nil))
	     (when (consp mexpr)
	       (mapc (lambda (subexpr) (if (mung-expts subexpr)
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
		 (setf (cdar mexpr) nil))) ;nix simp flag
	     munged))
    (handler-case (catch* (maxima::raterr maxima::errorsw maxima::macsyma-quit)
		    (return-from reduce-maxima-expr
		      (do ((x nil (copy-tree mexpr)))
			  ((progn (reduce) (or (not (mung-expts mexpr))
					       (equalp mexpr x))) mexpr))))
    (system::simple-floating-point-overflow ())
    (system::simple-arithmetic-error ()))
    'nan))
(defun from-maxima (expr)
  (if (atom expr) expr
      (let ((args (cdr expr)))
	(mkexpr (acase (caar expr)
		  (maxima::mplus '+)
		  (maxima::mtimes '*)
		  (maxima::%sin 'sin)
		  (maxima::mexp 'exp)
		  (maxima::%log 'log)
		  (maxima::mabs
		   (return-from from-maxima (from-maxima (car args))))
		  (maxima::rat 
		   (return-from from-maxima
		     (float (/ (car args) (cadr args)))))
		  (maxima::mexpt 
		   (assert (or (eq (car args) 'maxima::$%e)
			       (eql (car args) 2.718281828459045)))
		   (return-from from-maxima (list 'exp 
						  (from-maxima (cadr args)))))
		  (t it))
		(mapcar #'from-maxima args)))))

(define-reduction maxima-reduce (expr)
  :type num
  :action (from-maxima (reduce-maxima-expr (to-maxima (cons-cars expr)))))
