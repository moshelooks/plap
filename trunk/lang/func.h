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

#include "vtree.h"
#include <ostream>

namespace plap { namespace lang {

struct func_base {
  virtual ~func_base() {}

  virtual arity_t arity() const=0;
  virtual void operator()(const_subvtree loc,subvtree dst) const=0;

  virtual std::ostream& operator<<(std::ostream&) const=0;

  /**
  virtual bool is_def()    const { return false; }
  virtual bool is_lambda() const { return false; }
  virtual bool is_let()    const { return false; }
  virtual bool is_decl()   const { return false; }
  **/
};
inline std::ostream& operator<<(std::ostream& out,const func_base& f) {
  return f.operator<<(out);
}

template<arity_t Arity>
struct narg_func : public func_base {
  arity_t arity() const { return Arity; }
};

struct func : public func_base {
  func(arity_t a) : _arity(a) {}

  arity_t arity() const { return _arity; }
  void operator()(const_subvtree loc,subvtree dst) const {}

  std::ostream& operator<<(std::ostream& out) const { return out << "func"; }
  
  friend struct context;
 protected:
  arity_t _arity;
  vtree _body;

  void set_body(subvtree body) { _body.splice(_body.end(),body); }
};

}} //namespace plap::lang
#endif //PLAP_LANG_FUNCTION_H__
