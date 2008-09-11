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

;(proclaim '(optimize debug safety debug)
(declaim (optimize (speed 0) (safety 3) (debug 3)))
;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun shuffle (sequence)
  (let ((temp (coerce sequence 'vector)))
    (loop for i downfrom (1- (length temp)) to 1 do
      (rotatef (aref temp i) (aref temp (random (1+ i)))))
    (unless (eq temp sequence)
      (replace sequence temp))
    sequence))

;;; control structures
(defmacro blockn (&body body) `(block nil (progn ,@body)))
(defmacro while (test &body body)	; onlisp
  `(do ()
       ((not ,test))
     ,@body))
;; (defmacro aif (test then &optional else) ; onlisp
;;   `(let ((it ,test))
;;      (if it ,then ,else)))
;; (defmacro awhen (test &body body)	; onlisp
;;   `(aif ,test
;;         (progn ,@body)))
(defmacro acase (keyform &body clauses)
  `(let ((it ,keyform))
     (case it ,@clauses)))
(defmacro dorepeat (n &body body)
  (let ((var (gensym)))
    `(dotimes (,var ,n)
       ,@body)))
(defun group (source n)			; onlisp
  (if (zerop n) (error "zero length")) 
  (labels ((rec (source acc) 
	     (let ((rest (nthcdr n source))) 
	       (if (consp rest) 
		   (rec rest (cons (subseq source 0 n) acc)) 
		   (nreverse (cons source acc)))))) 
    (if source (rec source nil) nil)))
(defun singlep (lst)
  "Test list for one element."   ; LMH
  (and (consp lst) (not (cdr lst))))

(defun acar (x) (and (consp x) (car x)))
(defun icar (x) (if (consp x) (car x) x))
(defun ncons (x) (cons x nil))

(defmacro collector () '(lambda (x) (collect x)))

;;; abbreviations
(defmacro mvbind (vars values &body body)
  `(multiple-value-bind ,vars ,values ,@body))
(defmacro dbind (llist expr &body body)
  `(destructuring-bind ,llist ,expr ,@body))
(defmacro secondary (values)
  `(cadr (multiple-value-list ,values)))
(define-test secondary (assert-equal (secondary (floor 3.5)) 0.5))

;;; list iteration, comparison, manipulation, and construction
(defun same-length-p (l1 l2)
  (if (null l1) (null l2) 
      (and (not (null l2)) (same-length-p (cdr l1) (cdr l2)))))
(defun longerp (list n)
  (and list (or (eql n 0) (longerp (cdr list) (1- n)))))
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
(defun sort-copy (l cmp) (sort (copy-seq l) cmp))

;;; interleave copies of elem between elements of l, with elem iteself as
;;; the last element
(defun interleave (elem l)
  (reduce (lambda (x sofar) (cons (copy-tree elem) (cons x sofar)))
	  l :from-end t :initial-value (ncons elem)))

(defmacro bind-collectors (collectors collectors-body &body body)
  `(mvbind ,collectors (with-collectors ,collectors ,collectors-body)
     ,@body))

(defun ntimes (n elt)
  (loop for i from 1 to n collect elt))
(defun generate (n fn)
  (loop for i from 1 to n collect (funcall fn)))
(defun odds (l)
  (if l (cons (car l) (odds (cddr l)))))
(defun evens (l)
  (odds (cdr l)))

;;; (split (vector l1 d1 l2 d2 ... lN dN) fn)
;;; li are lists, fn is a function of 2N arguments
;;; Sequentially tests lists l1 ... lN for emptiness - if some list li is found
;;; to be empty, returns the value di. If no lists are empty, then fn is called
;;; with argument list (first1 rest1 first2 rest2 ... firstN restN), where 
;;; firsti is (car li) and resti is (cdr li)
(defun split (lists defaults fn &aux (listmap (make-hash-table)))
  (bind-collectors (cars cdrs)
      (mapc (lambda (list default)
	      (aif (and list (mvbind (v exists) (gethash list listmap)
			       (if exists v list)))
		   (progn (cars (car it))
			  (cdrs (setf (gethash list listmap) (cdr it))))
		   (return-from split default)))
	    lists defaults)
    (apply fn (nconc cars cdrs))))
(define-test split
  (let ((x '(1 2 3))
	(y '(a b c))
	(z '(q)))
    (assert-equal '(1 a 2 3 b (2 3) (b c) (3) nil (c))
		  (split (list x y x x y) '(nil nil nil nil nil)
			 (lambda (&rest args) args)))
    (assert-equal 42 (split (list x y z z x) '(nil nil nil 42 nil)
			    (lambda (&rest args) args)))))

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
      (let ((bind-max-arg-idx (max-arg-idx (butlast args)))
	    (rest-arg (gensym)))
	`(lambda ,(append
		   (mapcar (lambda (i) 
			     (read-from-string
			      (concatenate 'string "/" (write-to-string i))))
			   (loop for i from 1 to bind-max-arg-idx collect i))
		   `(&rest ,rest-arg)) 
	   (declare (ignore ,rest-arg))
	   (funcall #'apply ,fn ,@args))))))
(defmacro bind (fn &rest args)
  `(bindapp ,fn ,@args nil))

;;; memoization
(defun tabulate (fn array &rest indices)
  (or (apply #'aref array indices) 
      (setf (apply #'aref array indices) (apply fn indices))))

;;; testing
(defmacro define-all-equal-test (name pairs)
  `(define-test ,name 
     (dolist (p ,pairs)
       (dolist (x (cadr p))
	 (assert-equal (car p) (funcall #',name x) (cadr p))))))
(defmacro assert-for-all (f l)
  `(mapcar (lambda (x)
	     (assert-true (funcall ,f x)))
	   ,l))

;;; O(1) helpers
(defun emptyp (seq)
  (or (null seq) (not (some (lambda (x) (declare (ignore x)) t) seq))))
(define-test emptyp
  (dolist (x (mapcar #'emptyp (list nil (vector) (make-array 0))))
    (assert-true x))
  (dolist (x (mapcar #'emptyp (list '(a) (vector 1) (make-array 1))))
    (assert-false x)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))
(defmacro matches (x l) `(case ,x (,l t)))

;;; combinatorial algorithms
(defun cross-prod (fn xlist ylist)
  (mapcan (lambda (x) (mapcar (lambda (y) (funcall fn x y)) ylist)) xlist))
(defun cartesian-prod (&rest lists)
  (reduce (bind #'cross-prod #'cons /1 /2)
	  lists :initial-value '(nil) :from-end t))

(defun map-nonidentical-pairs (fn l1 l2)
  (mapl (lambda (l1)
	  (mapl (lambda (l2) 
		  (unless (eq l1 l2) (funcall fn (car l1) (car l2))))
		l2))
	l1))
(defun map-upper-triangle-pairs (fn l)
  (mapl (lambda (sublist) (mapc (bind fn (car sublist) /1) (cdr sublist))) l))
(define-test map-upper-triangle-pairs
  (assert-equal '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
		(collecting (map-upper-triangle-pairs (lambda (x y) 
							(collect (list x y)))
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
(defun init-hash-set (items  &key (insert #'sethash))
  (let ((table (make-hash-table)))
    (dolist (x items table) (funcall insert (list x nil) table))))
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
(defun keys-to-list (table)
  (collecting (maphash-keys (collector) table)))

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

;;; trees
(defun map-internal-nodes (fn tree)
  (funcall fn (car tree))
  (mapc (bind #'map-internal-nodes fn /1) (cdr tree)))
(defun map-subtrees (fn tree)
  (labels ((rec (tree)
	     (funcall fn tree)
	     (when (consp tree)
	       (mapc #'rec (cdr tree)))))
    (if tree (rec tree))))
(define-test map-subtrees
  (assert-equal '((1 (2 3) 4) (2 3) 3 4) 
		(collecting (map-subtrees (collector) '(1 (2 3) 4)))))

;;; io
(defun print* (&rest args)
  (print (car args))
  (mapc (lambda (x) (prin1 x) (write-char #\space)) (cdr args))
  nil)

;;; uses '? for any atom, '* for any subtree
(defun tree-matches (pattern tree)
  (flet ((atom-matches (pattern atom) (or (eq atom pattern) (eq atom '?))))
    (or (eq pattern '*) (if (atom tree)
			    (atom-matches tree pattern)
			    (and (consp pattern)
				 (same-length-p tree pattern)
				 (every #'tree-matches pattern tree))))))

(defun tree-diff (x y)
  (flet ((pdiff () (print* x) (print* '> y)))
    (unless (equalp x y)
      (if (or (atom x) (atom y))
	  (pdiff)
	  (let* ((n (max (length x) (length y)))
		 (x (append x (ntimes (- n (length x)) nil)))
		 (y (append y (ntimes (- n (length y)) nil))))
	      (mapc #'tree-diff x y)))
      t)))

(defmacro catch* (tags &body body)
  (labels ((rec (tags)
	     (if tags
		 `(catch ',(car tags) ,(rec (cdr tags)))
		 `(progn ,@body))))
    (rec tags)))

(defun equalp-to-eq (expr) ;fixme - do we need/use this?
  (mapl (lambda (expr1) (if (consp (car expr1))
			 (mapl (lambda (expr2)
				 (if (equalp (car expr1) (car expr2))
				     (setf (car expr2) (car expr1))))
			       (cdr expr1))))
	expr))
(define-test equalp-to-eq
  (let* ((foo '(and (or x y) (or x y) (or x y)))
	 (goo (copy-tree foo)))
    (equalp-to-eq foo)
    (assert-eq (second foo) (third foo) (fourth foo))
    (assert-equal foo goo)))

(defun fixed-point (fn x &key (test #'eq) &aux (y (funcall fn x)))
  (if (funcall test x y) x (fixed-point fn y)))

(defun insert-if (pred item list)
  (mapl (lambda (subl)
	  (when (funcall pred (car subl))
	    (rplacd subl (cons (car subl) (cdr subl)))
	    (rplaca subl item)
	    (return-from insert-if list)))
	list)
  (if list 
      (progn (push item (cdr (last list))) list)
      (list item)))
