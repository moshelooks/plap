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

(defun make-dag () (make-hash-table))
(defun dag-insert-node (node dag) 
  (setf (gethash node dag) (cons (make-hash-table) (make-hash-table))))
(defmacro dag-ancestors (node dag) `(car (gethash ,node ,dag)))
(defmacro dag-descendants (node dag) `(cdr (gethash ,node ,dag)))
(defun dag-insert-edge (from to dag)
  (flet ((two-way-set (from to)
	   (setf (gethash from (dag-ancestors to dag)) t)
	   (setf (gethash to (dag-descendants from dag)) t)))
    (two-way-set from to)
    (maphash-keys (lambda (descendant) (two-way-set from descendant))
		  (dag-descendants to dag))
    (maphash-keys (lambda (ancestor) (two-way-set ancestor to))
		  (dag-ancestors from dag))))

(defun ancestorp (x y dag) ; is x an ancestor of y?
  (gethash x (dag-ancestors y dag)))
(defun descendantp (x y dag) ; is x a descendant or y?
  (gethash x (dag-descendants y dag)))
(define-test dag-insertion ; a -> b -> d, a -> c -> d, d -> e
  (let ((dag (make-dag)))
    (mapc (bind #'dag-insert-node /1 dag) '(a b c d e))
    (mapc (bind #'dag-insert-edge (car /1) (cadr /1) dag)
	  '((a b) (b d) (a c) (c d) (d e)))
    (mapc (lambda (p) (let ((x (car p)) (y (cadr p)))
			(assert-true (ancestorp x y dag))
			(assert-false (descendantp x y dag))
			(assert-false (ancestorp y x dag))
			(assert-true (descendantp y x dag))))
	  '((a b) (a d) (a c) (a d) (a e)
	    (b d) (b e) (c d) (c e) (d e)))))

(defun dag-order-insert (x l dag)
  (unless l (list x))
  (blockn (mapl (lambda (subl) (when (descendantp (car subl) x dag)
				 (setf (cdr subl) (cons (car subl) (cdr subl)))
				 (setf (car subl) x)
				 (return l)))
		l)
	  (push x l)))
