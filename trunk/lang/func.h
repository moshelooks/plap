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

#ifndef PLAP_LANG_FUNC_H__
#define PLAP_LANG_FUNC_H__

#include <cassert>
#include <vector>
#include <boost/noncopyable.hpp>
#include <boost/integer_traits.hpp>
#include "vtree_fwd.h"

#ifndef LANG_LIMIT_ARITY //determines the maximal arity of defs in the language
#  define LANG_LIMIT_ARITY 4
#endif
#ifndef LANG_ARG_MAX //determines max # of args, including nesting
#  define LANG_ARG_MAX 16
#endif

namespace plap { namespace lang {

struct context;
struct lang_ident;

typedef unsigned int  id_t;
typedef unsigned char arity_t;
//this is needed because numeric_limits::max() isn't a const-expression
static const arity_t variadic_arity=boost::integer_traits<arity_t>::const_max;

struct func;
namespace lang_private {
std::vector<func*>& ids(); //global repo mapping ids to functions
} //namespace lang_private

struct func : boost::noncopyable {
 //fixme - look for gaps in ids() and use them
  func() : _id(lang_private::ids().size()) { 
    lang_private::ids().push_back(this); 
  }
  virtual ~func() { lang_private::ids()[_id]=NULL; }

  id_t id() const { return _id; }
  bool variadic() const { return arity()==variadic_arity; }
  virtual const vtree* body() const { return NULL; }
  virtual arity_t offset() const { return 0; }
  virtual arity_t arity() const=0;
  //default func application copies self and evaluates children
  virtual void operator()(context&,const_subvtree,subvtree) const;
 protected:
  id_t _id;

};

inline func* id2func(id_t s) {
  assert(s<lang_private::ids().size());
  assert(lang_private::ids()[s]);
  return lang_private::ids()[s];
}

template<arity_t Arity>
struct narg_func : public func {
  arity_t arity() const { return Arity; }
};

template<typename Type,arity_t Arity>
struct stateless_func : public narg_func<Arity> { 
  static Type* instance() {
    static Type t;
    return &t;
  }
 protected:
  stateless_func() {}
};

}} //namespace plap::lang
#endif //PLAP_LANG_FUNC_H__
