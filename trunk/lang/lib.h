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

#ifndef PLAP_LANG_LIB_H__
#define PLAP_LANG_LIB_H__

#include <algorithm>
#include "type.h"

namespace lang {

struct environment;

template<typename T>
struct lang_plus {
  T operator()(list_of<T> l) const { 
    return std::accumulate(l.begin(),l.end(),T(0)); 
  }
};

struct lang_foreach {
  void operator()(list_of<const_vsubtree> l,
                  func_of<const_vsubtree(const_vsubtree)> f) const {
    //std::for_each(l.begin(),l.end(),f);
  }
};

struct lang_print {
  disc_t operator()(list_of<const_vsubtree> l) const { 
    //foreach (const_vsubtree s,l) std::cout << "yuk ";
    return 0;
  }
};


void initialize_lib(environment& env);

} //~namespace lang

#endif  // PLAP_LANG_LIB_H__
