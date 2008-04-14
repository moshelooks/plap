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

// Core language classes: list, tuple, and identifier (used for defs and
// functions)

#ifndef PLAP_LANG_CORE_H__
#define PLAP_LANG_CORE_H__

#include "func.h"
#include "vtree.h"

namespace plap { namespace lang {

struct context;

struct lang_list : public stateless_func<lang_list,variadic_arity> {};
struct lang_tuple : public stateless_func<lang_tuple,variadic_arity> {};

struct lang_ident : public func {
  arity_t arity() const { return _arity; }
  void operator()(context& c,const_subvtree s,subvtree d) const;
  const vtree* body() const { return &_body; }
  arity_t offset() const { return _offset; }

  friend struct context;
  friend bool closure(const_subvtree);
 protected:
  vtree _body;
  arity_t _arity,_offset;
  bool _closure;

  void eval_leaf(context& c,subvtree d,bool expand_bindings=true) const;
  void set_body(context& c,subvtree b);
  bool has_var_outside_range(const_subvtree s) const;
  void instantiate_closure(context& c,subvtree d) const; 
  void rec_instantiate(context& c,subvtree d,bool& ready,
                       bool nested=false) const;

  lang_ident(arity_t a,arity_t o) : _arity(a),_offset(o) {}
};

struct lang_dummy : public stateless_func<lang_dummy,1> {
  void operator()(context& c,const_subvtree s,subvtree dst) const { dst=s[0]; }
};

inline const lang_ident* test_ident_arg_cast(vertex v) {
  if (func_t f=test_func_arg_cast(v))
    return dynamic_cast<const lang_ident*>(f);
  return NULL;
}
inline bool closure(const_subvtree s) {
  if (s.childless()) {
    if (const lang_ident* ident=test_ident_arg_cast(s.root()))
      return ident->_closure;
  } else if (const lang_ident* ident=
             dynamic_cast<const lang_ident*>(call_cast(s.root()))) {
    return ident->_closure;
  }
  return false;
}  

}} //namespace plap::lang

#endif //PLAP_LANG_CORE_H__
