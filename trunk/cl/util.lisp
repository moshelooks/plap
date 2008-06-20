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

(declaim (optimize (speed 0) (safety 3) (debug 3)))
;(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; control structures
(defmacro blockn (&body body) `(block nil (progn ,@body)))
(defmacro aif (test then &optional else) ; described in onlisp, by pg
  `(let ((it ,test))
     (if it ,then ,else)))
(defmacro while (test &body body) ; described in onlisp, by pg
  `(do ()
       ((not ,test))
     ,@body))
(defmacro awhen (test &body body) ; described in onlisp, by pg
  `(aif ,test
        (progn ,@body)))
(defmacro dorepeat (n &body body)
  (let ((var (gensym)))
    `(dotimes (,var ,n)
       ,@body)))

;;; list iteration, comparison, and construction
(defun same-length-p (l1 l2)
  (if (null l1) (null l2) 
      (and (not (null l2)) (same-length-p (cdr l1) (cdr l2)))))
;;; a stalk is a list with a single child (the cadr)
(defun stalkp (l) (and (consp l) (consp (cdr l)) (not (cddr l))))
(defun reduce-until (v f l)
  (blockn (reduce (lambda (x y) (let ((z (funcall f x y)))
				  (if (equal v z) (return v) z)))
			 l)))
(defun mapcar-until (v f &rest ls)
  (blockn (apply #'mapcar (lambda (&rest x) (let ((y (apply f x)))
					      (if (equal v y) (return v) y)))
			 ls)))
(defun mappend (f l) (apply #'append (mapcar f l)))
(defun iota (n) (loop for i from 0 to (- n 1) collect i))

;;; generalized argument-binding construct
(defmacro bindapp (fn &rest args) ; takes arguments /1 ... /9
  (let ((zero (char-int #\0)))
    (labels ((char-to-int (c) (- (char-int c) zero))
	     (max-arg-idx (l)
	       (reduce (lambda (n arg)
			 (cond ((symbolp arg)
				(if (eql (elt (symbol-name arg) 0) #\/)
				    (max n (char-to-int 
					    (elt (symbol-name arg) 1)))
				    n))
			       ((consp arg)
				(if (eq (car arg) 'bind) n
				    (max n (max-arg-idx (cdr arg)))))
			       (t n)))
		       l :initial-value 0)))
      (let ((bind-max-arg-idx (max-arg-idx (butlast args))))
	`(lambda ,(append
		   (mapcar (lambda (i) 
			     (intern (concatenate 'string 
						  "/" (write-to-string i))))
			   (loop for i from 1 to bind-max-arg-idx collect i))
		   '(&rest) (list (gensym)))
	   (funcall #'apply ,fn ,@args))))))
(defmacro bind (fn &rest args)
  `(bindapp ,fn ,@args nil))

;;; memoization
(defun tabulate (fn array &rest indices)
  (or (apply #'aref array indices) 
      (setf (apply #'aref array indices) (apply fn indices))))

;;; testing
(defun assert-all-equal (to f &rest l)
  (dolist (x l) (assert-equal to (funcall f x))))
(defmacro define-all-equal-test (name pairs &optional (f nil))
  `(define-test ,name 
     (dolist (p ,pairs) 
       (apply #'assert-all-equal (car p) (or ,f #',name) (cadr p)))))
(defun assert-for-all (f &rest l)
  (dolist (x l) (assert-true (funcall f x))))
(defun assert-for-none (f &rest l)
  (apply #'assert-for-all (lambda (x) (not (funcall f x))) l))

;;; O(1) helpers
(defun emptyp (seq)
  (or (null seq) (not (some (lambda (x) (declare (ignore x)) t) seq))))
(define-test emptyp
  (assert-for-all #'emptyp nil (vector) (make-array 0) )
  (assert-for-none #'emptyp '(a) (vector 1) (make-array 1)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))
(defmacro matches (x l) `(case ,x (,l t)))

;;; combinatorial algorithms
(defun cross-prod (fn xlist ylist)
  (mapcan (lambda (x) (mapcar (lambda (y) (funcall fn x y)) ylist)) xlist))
(defun cartesian-prod (&rest lists)
  (reduce (bind #'cross-prod #'cons /1 /2)
	  lists :initial-value '(nil) :from-end t))

(defun adjacent-pairs (f l)
  (if l (mapcar f l (cdr l))))
(define-test adjacent-pairs
  (assert-equal '((1 . 2) (2 . 3) (3 . 4)) (adjacent-pairs #'cons '(1 2 3 4)))
  (assert-equal nil (adjacent-pairs #'cons nil))
  (assert-equal nil (adjacent-pairs #'cons '(32))))

(defun nonidentical-pairs (fn l)
  (mapl (lambda (l1)
	  (mapl (lambda (l2) 
		  (unless (eq l1 l2) (funcall fn (car l1) (car l2))))
		l))
	l))
(defun upper-triangle-pairs (fn l)
  (mapl (lambda (sublist) (mapc (bind fn (car sublist) /1) (cdr sublist))) l))
(define-test upper-triangle-pairs
  (assert-equal '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
		(collecting (upper-triangle-pairs (lambda (x y) (collect
								 (list x y)))
						  '(1 2 3 4)))))

;;;removes adjacent eql pairs - fixme from O(n^2) to O(n)
(defun remove-adjacent-duplicates (l &key (test #'eql))
  (remove-duplicates l :test test))
(defun delete-adjacent-duplicates (l &key (test #'eql))
  (delete-duplicates l :test test))

(defun includesp (l1 l2 cmp)
  (if (and l1 l2)
      (unless (funcall cmp (car l2) (car l1))
	(includesp (cdr l1) 
		   (if (funcall cmp (car l1) (car l2)) l2 (cdr l2))
		   cmp))
      (not l2)))
(define-test includesp
  (flet ((itest (a b)
	   (assert-true (includesp a b #'<))
	   (assert-true (logically-equal (equal a b) (includesp b a #'<)))))
    (itest (iota 10) (iota 10))
    (itest (iota 10) (iota 9))
    (itest '(1 2 5 9 10) '(2 9 10))
    (itest nil nil)
    (itest '(3) '(3))
    (itest '(52) nil)))

(defun strict-includes-p (l1 l2 cmp)
  (and (eql (length l1) (length l2))
       (includesp l1 l2 cmp)))

(defun sortedp (l pred) 
  (labels ((rec-sorted (x xs)
	     (or (null xs) (and (funcall pred x (car xs))
				(rec-sorted (car xs) (cdr xs))))))
    (or (null l) (rec-sorted (car l) (cdr l)))))
(defun nondestructive-sort (l pred) ; returns l if it is already sorted
  (if (sortedp l pred) l (sort (copy-list l) pred)))
(define-test nondestructive-sort
  (assert-equal '(1 2 3 4) (nondestructive-sort '(4 3 2 1) #'<))
  (let* ((l '(1 2 3 9 4))
	 (l2  (nondestructive-sort l #'<)))
    (assert-equal '(1 2 3 4 9) l2)
    (assert-eq l2 (nondestructive-sort l2 #'<))))


(defun copy-range (l r) ; this is modern C++ - maybe there's a more lispy way?
  (cond 
    ((eq l r) nil)
    ((null r) (copy-list l))
    (t (let* ((result (cons (car l) nil))
              (dst result))
         (blockn 
	  (mapl (lambda (i) (if (eq i r)
				(return result)
				(setf dst (setf (cdr dst)
						(cons (car i) nil)))))
		(cdr l))
	  (assert nil () 
		  "bad arguments to copy-range - don't form a range"))))))
(define-test copy-range
  (let ((l '(1 2 3 4 5)))
    (assert-equal nil (copy-range l l))
    (assert-equal '(1) (copy-range l (cdr l)))
    (assert-equal '(1 2) (copy-range l (cddr l)))
    (assert-equal '(1 2 3) (copy-range l (cdddr l)))
    (assert-equal '(1 2 3 4 5) (copy-range l nil))))

;;; hashtable utilities
(defmacro amodhash (key table to)
  `(setf (gethash ,key ,table) (let ((it (gethash ,key ,table))) ,to)))
(defun sethash (pair table) (setf (gethash (car pair) table) (cadr pair)))
(defun init-hash-table (pairs &key (insert #'sethash))
  (let ((table (make-hash-table)))
    (dolist (p pairs table) (funcall insert p table))))
(defun hashmapcan (f h)
  (let ((res nil))
    (maphash (lambda (x y) (setf res (nconc (funcall f x y) res))) h) 
    res))
(defun touch-hash (key table)
  (setf (gethash key table) (gethash key table)))
(defun copy-hash-table (table)
  (let ((copy (make-hash-table :size (hash-table-size table))))
    (maphash (lambda (key value) (setf (gethash key copy) value))
	     table)
    copy))
(defun maphash-keys (fn table)
  (maphash (bind fn /1) table))
(defun hash-table-empty-p (table) ;;could be faster
  (eql (hash-table-size table) 0))

;;; mathematical functions
(macrolet ((declare-argcmp (name cmp)
	     `(defun ,name (f l ) 
		(let* ((maxelem (car l))
		       (maxval (funcall f maxelem)))
		  (mapc (lambda (x) (let ((v (funcall f x)))
				      (when (,cmp v maxval)
					(setf maxelem x)
					(setf maxval v))))
			l)
		  maxelem))))
  (declare-argcmp argmax >)
  (declare-argcmp argmin <))

(defun randbool () (eql (random 2) 0))
(defun randremove (p l) (remove-if (bind #'> p (random 1.0)) l))

;;; io
(defun print* (&rest args)
  (print (car args))
  (mapc (lambda (x) (prin1 x) (write-char #\space)) (cdr args))
  nil)


