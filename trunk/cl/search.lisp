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

analagous to define-reduction have a define-knob-creator

is it important to allof functions that generate reductions?

(defun kick (program find-move)
  (multiple-value-bind (siblings locus move) (funcall find-move program nil)
    (

(let (())
  (define-knob-creator subtree-inserter (root siblings locus)
    :nondeterministic t
    :condition (progn
		 (setf (cdr locus) (cons (rand-tree) (cdr locus)))
		 (let ((result (eq (normalize-and-reduce root) root)))
		   (setf (cdr locus) (cddr locus))
		   result))
    :arity
				
	    normalize-and-reduce 		 
		       
		      (

(defun search (neighborhood sol terminationp score)
  (let ((best (funcall score sol)))
    (aif (funcall neighborhood sol)
	 (if (< best it) (search
       