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

#ifndef PLAP_LANG_CORE_H__
#define PLAP_LANG_CORE_H__

#include "func.h"
#include "vtree.h"

namespace plap { namespace lang {

struct context;

struct lang_list : public stateless_func<lang_list,variadic_arity> {
  void operator()(context& c,const_subvtree s,subvtree d) const;
};

struct lang_ident : public func {
  arity_t arity() const { return _arity; }
  void operator()(context& c,const_subvtree s,subvtree d) const;
  const vtree* body() const { return &_body; }
  const lang_ident* closure() const { return _closure ? this : NULL; }
  friend struct context;
 protected:
  vtree _body;
  arity_t _arity,_offset;
  bool _closure;
  bool set_body(context& c,subvtree b,bool contains_closure); //true if closure
  void expand_closure(context& c,subvtree d,arity_t m) const;
  lang_ident(arity_t a,arity_t o) : _arity(a),_offset(o) {}
};

struct lang_closure : public stateless_func<lang_closure,1> {
  void operator()(context& c,const_subvtree s,subvtree d) const { 
    assert(false); 
  }
};

}} //namespace plap::lang

#endif //PLAP_LANG_CORE_H__
