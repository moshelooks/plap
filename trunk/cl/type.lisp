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

(let ((fun2type (init-hash-table
		 `((and (fun bool (list bool)))
		   (or (fun bool (list bool)))
		   (not (fun bool bool))
		   (true bool)
		   (false bool)
		   
		   (+ (fun num (list num)))
		   (* (fun num (list num)))
		   (/ (fun num num num))
		   
		   (< (fun bool any any))
		   (= (fun bool any any))))))

;; 		  (cons (fun (list any) any (list any)))
;; 		  (append (fun (list any) (list (list any))))
;; 		  (car (fun any (list any)))
;; 		  (cdr (fun (list any) (list any)))
;; 		  (length num)
;; 		  (accumulate (fun any (fun any any any) (list any) any))
;; 		  (mapcar (fun (list any) (fun any any) (list any)))

;; 	 (progn (act-result any),type-of-first)
;; 	  (andseq ,type-of-first)
;; 	  (orseq ,type-of-first)))))

;; 	   (cons ,(lambda (x l) (type-union `(list ,(tree-type x))
;; 					    (tree-type l))))
;; 	   (append ,(lambda (&rest ls) (apply #'type-union 
;; 					      (mapcar '#tree-type ls))))
;; 	   (car ,(lambda (l) (if (eq (car l) 'list)
;; 				 (tree-type (car l))
;; 				 (cadr (tree-type l)))))
;; 	   (cdr ,(lambda (l) (if (eq (car l) 'list)
;; 				 (tree-type `(list ,(cddr l)))
;; 				 (tree-type l))))
;; (type-of-first (lambda (&rest l) (tree-type (car l))))		   
  (defun fun-type (f) 
    (multiple-value-bind (v exists) (gethash f fun2type)
      (assert exists () (concatenate 'string "couldn't find a type for " 
				     (string f)))
      v)))
(defun atom-type (x)
  (cond ((or (eq x 'true) (eq x 'false)) 'bool)
	((numberp x) 'num)
	((null x) nil)
	(t (fun-type x))))
(defun expr-type (tree) ;fixme - doesn't yet handle user-defined funs
  (if (consp tree)
      (case (car tree)
	(list (list 'list (type-of (cadr tree))))
	(tuple (cons 'tuple (mapcar #'expr-type (cdr tree))))
	(t (let ((type (fun-type (car tree))))
	     (assert (eq (car type) 'fun))
	     (cadr type))))
      (atom-type tree)))
(define-all-equal-test expr-type
    `((bool (true false (and true false) (not (or true false))))
      (num  (1 4.3 ,(/ 1 3) ,(sqrt -1) (+ 1 2 3) (* (+ 1 0) 3)))
      ((fun bool (list bool)) (and or))
      ((fun bool bool) (not))
      ((fun num (list num)) (+ *))
      ((fun bool any any) (< =))
      (bool ((< 2 3) (< true false) (= (= 2 2) 7)))))

(defun type<= (t1 t2)
  (if (eq t2 'any) t (eq t1 t2))) ;fixme

(defun tuple-type-p (type) (and (consp type) (eq (car type) 'tuple)))

;; (defun is-type-p (type) 
;;   (if (consp type)
;;       (matches (car type) (list tuple fun))
;;       (matches type (bool num))