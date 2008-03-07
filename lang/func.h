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

#include <ostream>
#include <boost/noncopyable.hpp>
#include "vtree.h"

//this determines the maximal arity of defs in the language
#ifndef LANG_LIMIT_ARITY
#  define LANG_LIMIT_ARITY 4
#endif

namespace plap { namespace lang {

struct context;

extern const char no_name[];
static const arity_t variadic_arity=-1;

struct func_base : boost::noncopyable {
  virtual ~func_base() {}

  virtual arity_t arity() const=0;
  virtual func_t arg_type(arity_t a) const=0;
  virtual void operator()(context&,const_subvtree loc,subvtree dst) const=0;
  virtual std::ostream& operator<<(std::ostream&) const=0;

  bool variadic() const { return arity()==variadic_arity; }
};
inline std::ostream& operator<<(std::ostream& out,const func_base& f) {
  return f.operator<<(out);
}

template<arity_t Arity>
struct narg_func : public func_base {
  arity_t arity() const { return Arity; }
};

template<typename Type,arity_t Arity,const char* Name=no_name>
struct stateless_func : public narg_func<Arity> { 
  static Type* instance() {
    static Type t;
    return &t;
  }
  std::ostream& operator<<(std::ostream& o) const { return o << Name; }
 protected:
  stateless_func() {}
};

struct func : public func_base {
  func(arity_t a) : _arity(a) {}

  arity_t arity() const { return _arity; }
  void operator()(context& c,const_subvtree loc,subvtree dst) const {}//fixme

  func_t arg_type(arity_t a) const { return NULL; } //fixme

  std::ostream& operator<<(std::ostream& out) const { return out << "func"; }
  
  const_subvtree body() const { return _body; }
  friend struct context;
 protected:
  arity_t _arity;
  vtree _body;

  void set_body(subvtree b) { _body.splice(_body.end(),b); }
};

}} //namespace plap::lang
#endif //PLAP_LANG_FUNCTION_H__
