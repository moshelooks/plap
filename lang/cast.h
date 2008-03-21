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

#ifndef PLAP_LANG_CAST_H__
#define PLAP_LANG_CAST_H__

#include "type.h"

namespace plap { namespace lang {

//needed to do template partial specialization
template<typename T>
struct literal_caster { 
  T operator()(const_subvtree s) { 
    assert(s.childless());
    return arg_cast<T>(s.root());
  }
};

template<>
struct literal_caster<const_subvtree> {
  const_subvtree operator()(const_subvtree s) { return s; }
};
template<typename T>
struct literal_caster<list_of<T> > {
  list_of<T> operator()(const_subvtree s) { return list_of<T>(s); }
};
template<typename T>
struct literal_caster<func_of<T> > {
  func_of<T> operator()(const_subvtree s) { 
    assert(s.childless());
    return func_of<T>(arg_cast<func_t>(s.root()));
  }
};

template<typename T> //for convenience
T literal_cast(const_subvtree s) { return literal_caster<T>()(s); }

template<typename T>
struct arg_visitor { typedef T result_type; };

template<typename Visitor>
typename Visitor::result_type arg_visit(const Visitor& visit,vertex v) {
  if (is_func(v))
    return visit(arg_cast<func_t>(v));
  if (is_symbol(v))
    return visit(arg_cast<id_t>(v));
  return visit(arg_cast<number_t>(v));
}

}} //namespace plap::lang
#endif //PLAP_LANG_CAST_H__
