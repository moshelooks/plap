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

(in-package :cl-user)
(defpackage :plop-asd
  (:use :cl :asdf :cl-utilities :anaphora))
(setf *print-circle* t) ; markup may contain circular references to parents

(in-package :plop-asd)
(defsystem "plop"
  :serial t
  :components ((:file "packages")
               (:file "util")
	       (:file "dag")
	       (:file "context")
	       (:file "syntax")
 	       (:file "markup")
	       (:file "semantics")
	       (:file "type")
	       (:file "eval")
	       (:file "enum")
	       (:file "reduct-core")
	       (:file "reductions")
;	       (:file "sampling")
	       (:file "bool")
	       (:file "num")
	       (:file "maxima")
	       (:file "list")
	       (:file "canonize")
;	       (:file "represent")
;	       (:file "search")
	       (:file "tests")))
