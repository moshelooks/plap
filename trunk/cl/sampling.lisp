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

(defvar *semantic-sampling-max-cache-size* 65536)


(defparameter *n-random-trees-for-testing* 10000)
(labels ((randtree (depth)
	   (if (eql (depth 0)

(nconc (collecting
			     (dotimes (n *n-random-trees-for-testing*)
			       (let ((depth (1+ (random 10))))
				 (labels 

(defun make-semantic-sampler (sampler)
  (let ((cache (make-hash-table))) ;cache : type -> (nat -> (seq of trees))
    (labels 
	((cache-lookup (type size)
	   (aref (aif (gethash type cache)
		      (if (<= (length it) size)
			  (setf it (adjust-array it size :initial-element nil))
			  it)
		      (setf (gethash type cache) 
			    (make-array size :initial-element nil)))
		 size))
	 (full-max-size (return-type)
	   
	   (let ((size 
		  
(loop for (f t) in (all-functions-returning return-type)
     
     do (


(aref it size
		      
(aref 
      (sample 

       (get-expr (type size)
	 (let ((exprs (cache-lookup type size)))
	   (if (< size (full-max-size type))
	       (aref exprs (random (length exprs)))
	       (if (emptyp exprs)
		   (sample type size)
		   (pop exprs))))))
      
    get-expr))

))))))))))

(

get rid of make-normal form, just use make-reduction and have a normal form tag
/ prereqs

constantly


;;   (let ((exprs (randremove 0.9 (enum-trees *enum-trees-test-symbols* 2))))
;;     (block enumerative-test
;;       (flet ((opp (x) (case x
;; 			(less 'greater)
;; 			(greater 'less)
;; 		      ((nil) nil))))
;; 	(dolist (expr1 exprs)
;; 	  (dolist (expr2 exprs)
;; 	    (unless (assert-equal (total-cmp expr1 expr2)
;; 				  (opp (total-cmp expr2 expr1))
;; 				  expr1 expr2)
;; 	      (return-from enumerative-test nil)))))))) fixme
