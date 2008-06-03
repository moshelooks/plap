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

; hash-table utilities
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

(defun iota (n) (loop for i from 0 to (- n 1) collect i))

(defun tabulate (fn array &rest indices)
  (or (apply #'aref array indices) 
      (setf (apply #'aref array indices) (apply fn indices))))


(defun assert-all-equal (to l f)
  (dolist (x l) (assert-equal to (funcall f x))))
(defmacro define-all-equal-test (name pairs &optional (f nil))
  `(define-test ,name 
     (dolist (p ,pairs) (assert-all-equal (car p) (cadr p) 
					  (or ,f #',name)))))
(defun assert-for-all (f &rest l)
  (dolist (x l) (assert-true (funcall f x))))
(defun assert-for-none (f &rest l)
  (apply #'assert-for-all (lambda (x) (not (funcall f x))) l))

(defun same-length (l1 l2)
  (if (null l1) (null l2) 
      (and (not (null l2)) (same-length (cdr l1) (cdr l2)))))

(defmacro blockn (&rest body) `(block nil (progn ,@body)))

(defun reduce-until (v f l)
  (blockn (reduce (lambda (x y) (let ((z (funcall f x y)))
				  (if (equal v z) (return v) z)))
			 l)))
(defun mapcar-until (v f &rest ls)
  (blockn (apply #'mapcar (lambda (&rest x) (let ((y (apply f x)))
					      (if (equal v y) (return v) y)))
			 ls)))

(defmacro aif (test then &optional else) 
  `(let ((it ,test))
     (if it ,then ,else)))

(defun mappend (f l) (apply #'append (mapcar f l)))

(defun expr-size (expr) 
  (if (consp expr)
      (reduce #'+ (mapcar #'expr-size (cdr expr)) :initial-value 1) 
      1))

(defun cross-prod (xlist ylist &key (pair #'cons))
  (mapcan (lambda (x) (mapcar (lambda (y) (funcall pair x y)) ylist)) xlist))
(defun cartesian-prod (&rest lists)
  (reduce #'cross-prod lists :initial-value '(nil) :from-end t))

(defun empty (seq)
  (or (null seq) (not (some (lambda (x) (declare (ignore x)) t) seq))))
(define-test empty
  (assert-for-all #'empty nil (vector) (make-array 0) )
  (assert-for-none #'empty '(a) (vector 1) (make-array 1)))

(defun equalp-to-eq (l)
  (mapl (lambda (l1) (if (consp (car l1))
			 (mapl (lambda (l2)
				 (if (equalp (car l1) (car l2))
				     (setf (car l2) (car l1))))
			       (cdr l1))))
	l))
(define-test equalp-to-eq
  (let* ((foo '(and (or x y) (or x y) (or x y)))
	 (goo (copy-tree foo)))
    (equalp-to-eq foo)
    (assert-eq (second foo) (third foo) (fourth foo))
    (assert-equal foo goo)))

(defun adjacent-pairs (l)
  (if l (mapcar #'cons l (cdr l))))
(define-test adjacent-pairs
  (assert-equal '((1 . 2) (2 . 3) (3 . 4)) (adjacent-pairs '(1 2 3 4)))
  (assert-equal nil (adjacent-pairs nil))
  (assert-equal nil (adjacent-pairs '(32))))

;;;destructively removes adjacent eql pairs - fixme from O(n^2) to O(n)
(defun uniq (l &key (test #'eql))
  (delete-duplicates l :test test))

(defun sorted (l pred) 
  (labels ((rec-sorted (x xs)
	     (or (null xs) (and (funcall pred x (car xs))
				(rec-sorted (car xs) (cdr xs))))))
    (or (null l) (rec-sorted (car l) (cdr l)))))
(defun nondestructive-sort (l pred) ; returns l if it is already sorted
  (if (sorted l pred) l (sort (copy-list l) pred)))
(define-test nondestructive-sort
  (assert-equal '(1 2 3 4) (nondestructive-sort '(4 3 2 1) #'<))
  (let* ((l '(1 2 3 9 4))
	 (l2  (nondestructive-sort l #'<)))
    (assert-equal '(1 2 3 4 9) l2)
    (assert-eq l2 (nondestructive-sort l2 #'<))))

(defun maphash-keys (fn table)
  (maphash (bind fn /1) table))

(let ((zero (char-int #\0)))
  (defun char-to-int (c) (- (char-int c) zero)))

(defmacro bind (fn &rest args) ; takes arguments /1 ... /9
  (labels ((max-arg-idx (l)
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
    (let ((bind-max-arg-idx (max-arg-idx args)))
      `(lambda ,(append
		 (mapcar (lambda (i) (intern (concatenate 
					      'string "/" (write-to-string i))))
			 (cdr (iota (1+ bind-max-arg-idx))))
		 '(&rest) (list (gensym)))
	 (funcall ,fn ,@args)))))

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

