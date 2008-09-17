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

(defun ring-op-p (expr) ;true if rooted in + or * or and or or
  (matches (fn expr) (+ * and or)))

;; (define-reduction reduce-abs (expr)
;;   :type num
;;   :condition (matches (car expr) (*
;;   :action
	 
(defun num-table (expr vars table)
  (mapcar (lambda (values)
	    (with-bound-symbols *empty-context* vars values
	      (peval expr context)))
	  table))

(labels
    ((mkexp (expr) `(exp (* -1 (log ,expr))))
     (eliminate-division (expr)
       (if (eq (afn expr) '/)
	   (pcons '* (if (eq (afn (arg0 expr)) '*)
			 (if (eq (afn (arg1 expr)) '*)
			     (append (args (arg0 expr))
				     (mapcar #'mkexp (args (arg1 expr))))
			     (append (args (arg0 expr)) (mkexp (arg1 expr))))
			 (if (eq (afn (arg1 expr)) '*)
			     (append (arg0 expr) 
				     (mapcar #'mkexp (args (arg1 expr))))
			     (append (arg0 expr) (mkexp (arg1 expr))))))
	   expr))
     (read-stream (fname &aux res)
       (with-open-file (stream (concatenate 'string *plop-root-dir* fname))
	 (do ((expr (read stream nil) (read stream nil))) ((null expr))
	   (push expr res)))
       res))
  (let ((raw-sexprs (read-stream "sample_real_trees_10k"))
	(combo-sexprs (read-stream "combo_sexprs")))

;; (defun mmtry (fn expr)
;;   (handler-case
;;       (catch 'maxima::raterr
;; 	(catch 'maxima::errorsw
;; 	  (catch 'maxima::macsyma-quit
;; 	    (funcall fn (to-maxima (cons-cars expr))))))
;;     (system::simple-floating-point-overflow () 'infinity)
;;     (system::simple-arithmetic-error () 'infinity)))

;; (defun mtry (fn expr)
;;   (handler-case
;;       (catch 'maxima::raterr
;; 	(catch 'maxima::errorsw
;; 	  (catch 'maxima::macsyma-quit
;; 	    (from-maxima (funcall fn (to-maxima (cons-cars expr)))))))
;;     (system::simple-floating-point-overflow () 'infinity)
;;     (system::simple-arithmetic-error () 'infinity)))
;; (defun mung (expr)
;;   (if (atom expr) expr
;;       (if (eq (car expr) 'maxima::mabs)
;; 	  (mung (cadr expr))
;; 	  (if (and (eq (car expr) 'maxima::mexpt) 
;; 		   (or (eq (cadr expr) 'maxima::$%e)
;; 		       (eql (cadr expr) 2.718281828459045)))
;; 	      (list 'exp (mung (caddr expr)))
;; 	      (cons (car expr) (mapcar #'mung (cdr expr)))))))
;; (defun sizeme (fn)
;;   (time (reduce #'+ raw-sexprs 
;; 		:key (lambda (expr) (expr-size (mung (mtry fn expr)))))))

;; (defparameter combo-nodiv-sexprs 
;;   (mapcar (bind #'upwards #'eliminate-division /1) combo-sexprs))

;(defun sexprs-size (sexprs) (reduce #'+ sexprs :key #'tree-size))
