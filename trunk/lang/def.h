// Copyright 2008 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License")
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an AS IS BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: madscience@google.com (Moshe Looks)

// def(inition) is a pure virtual class than encapsulates a lambda abstraction
// and may be either a program tree or a piece of C++
//
// For C++, macro wizardry allows you to say make_eager_def(f,t1,...,tn) and
// get a heap-allocated def that, corresponds to a call of f with arguments of
// types t1,...,t2. Futhermore, lists are converted into ranges of the
// corresponding type, so e.g. 
//
// make_eager_def(bind(&accumulate<slist<disc_t>::const_iterator,disc_t>,
//                     _1,_2,disc_t(0)),list_of<disc_t>());
// creates a def that corresponds to summing over a list of ints
//
// make_lazy_def<arity>(f) does the same thing, but all arguments to f are of
// type const_vsubtree (make_lazy_def not yet implemented - fixme)

#ifndef PLAP_LANG_DEF_H__
#define PLAP_LANG_DEF_H__

#include "vertex.h"

namespace lang {

struct environment;

struct def {
  virtual ~def() {}
  virtual void operator()(const_vsubtree loc,vsubtree dst,
                          environment& env) const=0;
};

} //~namespace lang

#endif  // PLAP_LANG_DEF_H__
