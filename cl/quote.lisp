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

(defun sexpr2p (expr) 
  (cond ((atom expr) expr)
	((eq (car expr) 'lambda)
	 (cons (cons (car expr) nil) (list (cons (list 'list) (cadr expr)) 
					   (sexpr2p (caddr expr)))))
	(t (cons (cons (car expr) nil) (mapcar #'sexpr2p (cdr expr))))))
(set-macro-character
 #\% (lambda (stream char)
       (declare (ignore char))
       (list 'quote (sexpr2p (read stream t nil t)))) t)
(set-macro-character
 #\~ (lambda (stream char)
       (declare (ignore char))
       `(apply #'canonize-from-template
	       ,(read (make-concatenated-stream (make-string-input-stream "`")
						stream)
		      t nil t))) t)

