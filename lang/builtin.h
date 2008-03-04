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
// add library functions to an context.

#ifndef PLAP_LANG_BUILTIN_H__
#define PLAP_LANG_BUILTIN_H__

#include <numeric>
#include "context.h"
#include "type.h"

namespace plap { namespace lang {

void initialize_lib(context& c);

//conditionals
inline void lang_if(context& c,const_subvtree cond,
                    const_subvtree if_br,const_subvtree else_br,subvtree dst) {
  c.eval(c.eval_to<bool>(cond) ? if_br : else_br,dst);
}  

//arithmetic operators
inline disc_t lang_plus(list_of<disc_t> l) { 
  return std::accumulate(l.begin(),l.end(),disc_t(0));
}

#if 0
inline disc_t lang_foreach(list_of<const_subvtree> l,
                         func_of<const_subvtree(const_subvtree)> f) {
  //fixmestd::for_each(l.begin(),l.end(),f);
  return 0;
}

disc_t lang_print(const_subvtree v) { 
  std::cout << "yuk ";
  //foreach (const_subvtree s,l) std::cout << "yuk ";
  return 0;
}
#endif
}} //namespace plap::lang

#endif //PLAP_LANG_BUILTIN_H__
