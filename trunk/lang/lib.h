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

// All library functions should appear in initialize_lib, which is called to
// add library functions to an environment.

#ifndef PLAP_LANG_LIB_H__
#define PLAP_LANG_LIB_H__

#include <numeric>
#include <iostream>//fixme
#include "type.h"

namespace plap { namespace lang {

struct environment;
void initialize_lib(environment& env);

template<typename T>
T lang_plus(list_of<T> l) { return std::accumulate(l.begin(),l.end(),T(0)); }

inline disc_t lang_foreach(list_of<const_vsubtree> l,
                         func_of<const_vsubtree(const_vsubtree)> f) {
  std::for_each(l.begin(),l.end(),f);
  return 0;
}

disc_t lang_print(const_vsubtree v) { 
  std::cout << "yuk ";
  //foreach (const_vsubtree s,l) std::cout << "yuk ";
  return 0;
}

}} //namespace plap::lang

#endif //PLAP_LANG_LIB_H__
