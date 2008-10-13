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

;;; symbol-bindings maps from symbol names to (value . type) pairs
;;; type-map maps from type to hashes 
(defstruct context
  (symbol-bindings (make-hash-table) :type hash-table)
  (type-map (make-hash-table) :type hash-table))
(defun init-context (bindings &aux (context (make-context)))
  (mapc (bind #'bind-symbol context (car /1) (cadr /1)) bindings)
  context)

;;; when setfing via get-value, must be already bound and of same type
(defmacro get-value (name context)
  `(caar (gethash ,name (context-symbol-bindings ,context))))
(defun get-type (name context)
  (cdar (gethash name (context-symbol-bindings context))))
(defun get-symbols (type context) ; returns a hashmap where keys are symbols,
  (or (gethash type (context-type-map context)) ; values nil
      (setf (gethash type (context-type-map context)) (make-hash-table))))
(defun bound-in-p (name context) 
  (secondary (gethash name (context-symbol-bindings context))))

(defun bind-type (context name type) ;fixme
  (bind-symbol context name nil type))
(defun unbind-type (context name) ;fixme
  (unbind-symbol context name))

;;; when binding a symbol, value must be already evaled
(defun bind-symbol (context name value
		    &optional (type (value-type value)))
  (or (awhen (get-type name context)
	(or (eq type it) 
	    (not (remhash name (gethash it (context-type-map context))))))
      (setf (gethash name (get-symbols type context)) nil))
  (push (cons value type) (gethash name (context-symbol-bindings context))))

(defun unbind-symbol (context name)
  (let ((oldtype (cdr (pop (gethash name (context-symbol-bindings context)))))
	(newtype (get-type name context)))
    (unless (eq newtype oldtype)
      (remhash name (get-symbols oldtype context))
      (if newtype
	  (setf (gethash name (get-symbols newtype context)) nil)
	  (remhash name (context-symbol-bindings context))))))

;;; note that this is not a constant - for efficiency you are alow to add
;;; things to the empty context prodived they are are removed afterwards
;;; (i.e. via unwind-protect) - note that this is somewhat dangerous however as
;;; you might inadvertantly call a function that expects the empty context to
;;; actually be empty
(defparameter *empty-context* (make-context))

(defun context-empty-p (context) 
  (hash-table-empty-p (context-symbol-bindings context)))

(defmacro with-bound-symbols (context symbols values &body body)
  `(unwind-protect
	(progn (mapc (bind #'bind-symbol ,context /1 /2) ,symbols ,values)
	       ,@body)
     (mapc (bind #'unbind-symbol ,context /1) ,symbols)))

(defmacro with-nil-bound-symbols (context symbols &body body)
  `(unwind-protect
	(progn (mapc (bind #'bind-symbol ,context /1 nil) ,symbols)
	       ,@body)
     (mapc (bind #'unbind-symbol ,context /1) ,symbols)))

(defmacro with-bound-symbol-types (context symbols types &body body)
  `(unwind-protect
	(progn (mapc (bind #'bind-type ,context /1 /2) ,symbols ,types)
	       ,@body)
     (mapc (bind #'unbind-type ,context /1) ,symbols)))
