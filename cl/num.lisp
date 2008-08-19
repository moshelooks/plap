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

(defun little-epsilon (x) 
  (let* ((x (if (numberp x) x 0))
	 (y (abs x))
	 (v 0.01))
    (if (and (not (equal y 0)) (< y (/ v 2))) (/ y 2) v)))
(defun big-epsilon (x)
  (let* ((x (if (numberp x) x 0)))
    (if (eql x 0) 1 (/ (+ 1 (abs x)) 2))))

(defun dual-num-op (f) (ecase f (* '+) (+ '*)))

;;; replace with identity-element removal
(define-reduction haxx-num-1 (expr)
  :type num
  :condition (and (eq (car expr) '*) (eql (cadr expr) 1))
  :action (if (longerp expr 3) (cons '* (cddr expr)) (caddr expr)))
(define-reduction haxx-num-2 (expr)
  :type num
  :condition (and (eq (car expr) '+) (eql (cadr expr) 0))
  :action (if (longerp expr 3) (cons '+ (cddr expr)) (caddr expr)))

(defun ring-op-p (expr) ;true if rooted in + or * or and or or
  (matches (acar expr) (+ * and or)))

(defun cons-cars (expr)
  (if (consp expr)
      (cons (ncons (car expr)) (mapcar #'cons-cars (cdr expr)))
      expr))

;;; expr should be already cons-car'ed
(defun to-maxima (expr)
  (if (atom expr) expr
      (let ((subexprs (mapcar #'to-maxima (cdr expr))))
	(cons (ncons (acase (caar expr)
		       (+ 'maxima::mplus)
		       (* 'maxima::mtimes)
		       (sin 'maxima::%sin)
		       (exp 'maxima::$exp)
		       (log (return-from to-maxima
			      `((maxima::%log) ((maxima::mabs) ,@subexprs))))
		       (/ 'maxima::mquotient)
		       (t it)))
	      subexprs))))

;; (define-reduction reduce-abs (expr)
;;   :type num
;;   :condition (matches (car expr) (*
;;   :action
	    
(defun from-maxima (expr)
  (if (atom expr) expr
      (let ((args (cdr expr)))
	(mkexpr (acase (caar expr)
		  (maxima::mplus '+)
		  (maxima::mtimes '*)
		  (maxima::%sin 'sin)
		  (maxima::mexp 'exp)
		  (maxima::%log 'log)
		  (maxima::mabs 'abs)
		  (maxima::mexpt 'expt)
;; 		   (ecase (caaadr expr)
;; 		     (maxima::mabs (setf args (cddr expr)))
;; 		     (maxima::mtimes
;; 		      (mapc (lambda (subexpr) 
;; 			      (print subexpr)
;; 			      (assert (or (numberp subexpr)
;; 					  (eq (caar subexpr) 'maxima::mabs)
;; 					  (eq (caar subexpr) 'maxima::mexpt))))
;; 			    (cdadr expr))
;; 		      (return-from from-maxima
;; 			`(logabs (* ,@(mapcar #'cadr (cdadr expr))))))))
		  (t it))
		(mapcar #'from-maxima args)))))

;(defun mknums (d n) (generate n (lambda () (1- (random 2.0)))))

(defun num-table (expr vars table &aux (context (make-context)))
    (mapcar (lambda (values)
	      (with-bound-symbols context vars values
		(eval-expr expr context)))
	    table))

;; (defun mung (expr)
;;   (if (atom expr) expr
;;       (if (eq (car expr) 'maxima::mabs)
;; 	  (mung (cadr expr))
;; 	  (if (and (eq (car expr) 'maxima::mexpt) 
;; 		   (or (eq (cadr expr) 'maxima::$%e)
;; 		       (eql (cadr expr) 2.718281828459045)))
;; 	      (list 'maxima::exp (mung (caddr expr)))
;; 	      (cons (car expr) (mapcar #'mung (cdr expr)))))))


(setf maxima::$ratprint nil) ; to prevent maxima from spewing out warnings
(setf maxima::errorsw t)     ; and error messages
(setf maxima::errrjfflag t)

(setf maxima::$numer t) ; to force evaluation of e.g. exp(sin(sin(2)))

;;; to avoid trying to factor expressions that produce pathological behavior
;;; when one tries to factor them (e.g. -0.2+x^0.70086)
;;; in the future it would be nice to do a more sophisticated check
(in-package :maxima)
(defun factor-if-small (expr) expr)
(in-package :plop)

(define-reduction maxima-reduce (expr)
  :type num
  :action
  (blockn 
   (handler-case
       (catch* (maxima::raterr maxima::errorsw maxima::macsyma-quit)
	 (return
	   (from-maxima
	    (maxima::$float 
	     (maxima::simplify (to-maxima (cons-cars expr)))))))
     (system::simple-floating-point-overflow ())
     (system::simple-arithmetic-error ()))
   'nan))

(define-reduction eliminate-division (expr)
  :type num
  :action
  (if (eq (fn expr) '/)
      (if (eq (acar (arg0 expr)) '*)
	  (if (eq (acar (arg1 expr)) '*)
	      `(* ,@(args (arg0 expr)) ,@(mapcar (lambda (subexpr)
						   `(expt ,subexpr -1))
						 (args (arg1 expr))))
	      `(* ,@(args (arg0 expr)) (expt ,(arg1 expr) -1)))
	  (if (eq (acar (arg1 expr)) '*)
	      `(* ,(arg0 expr) ,@(mapcar (lambda (subexpr)
					   `(expt ,subexpr -1))
				       (args (arg1 expr))))
	      `(* ,(arg0 expr) (expt ,(arg1 expr) -1))))
      expr))

(defparameter raw-sexprs nil)
(with-open-file
    (stream "/Users/madscience/work/plap/trunk/cl/sample_real_trees_10k")
  (do ((expr (read stream nil) (read stream nil))) ((null expr))
    (push expr raw-sexprs)))

(defparameter combo-sexprs nil)
(with-open-file
    (stream "/Users/madscience/work/moses/moses2/combo_sexprs")
  (do ((expr (read stream nil) (read stream nil))) ((null expr))
	  (push expr combo-sexprs)))

(defun mmtry (fn expr)
  (handler-case
      (catch 'maxima::raterr
	(catch 'maxima::errorsw
	  (catch 'maxima::macsyma-quit
	    (funcall fn (to-maxima (cons-cars expr))))))
    (system::simple-floating-point-overflow () 'infinity)
    (system::simple-arithmetic-error () 'infinity)))

(defun mtry (fn expr)
  (handler-case
      (catch 'maxima::raterr
	(catch 'maxima::errorsw
	  (catch 'maxima::macsyma-quit
	    (from-maxima (funcall fn (to-maxima (cons-cars expr)))))))
    (system::simple-floating-point-overflow () 'infinity)
    (system::simple-arithmetic-error () 'infinity)))
(defun mung (expr)
  (if (atom expr) expr
      (if (eq (car expr) 'maxima::mabs)
	  (mung (cadr expr))
	  (if (and (eq (car expr) 'maxima::mexpt) 
		   (or (eq (cadr expr) 'maxima::$%e)
		       (eql (cadr expr) 2.718281828459045)))
	      (list 'exp (mung (caddr expr)))
	      (cons (car expr) (mapcar #'mung (cdr expr)))))))
(defun sizeme (fn)
  (time (reduce #'+ raw-sexprs 
		:key (lambda (expr) (tree-size (mung (mtry fn expr)))))))

(defparameter combo-nodiv-sexprs 
  (mapcar (bind #'upwards #'eliminate-division /1) combo-sexprs))

;(defun sexprs-size (sexprs) (reduce #'+ sexprs :key #'tree-size))
