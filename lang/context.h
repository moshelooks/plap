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

#ifndef PLAP_LANG_CONTEXT_H__
#define PLAP_LANG_CONTEXT_H__

#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/noncopyable.hpp>
#include "type.h"

namespace plap { namespace lang {

struct context : public boost::noncopyable {

  //create a parent context
  context() : _parent(this),_root(this) { initialize_lib(*this); }

  //create a child context
  context(context& p) : _parent(&p),_root(_parent->_root) {}

  //declarations are only needed for defining mutually recurize functions
  func_t declare_func(arity_t a) {
    _funcs.push_back(new func(a));
    return &_funcs.back();
  }

  //these splice out body - so if you want to keep it, make a copy
  func_t define_func(subvtree body,func_t decl) { 
    assert(dynamic_cast<const func*>(decl));
    const_cast<func*>(static_cast<const func*>(decl))->set_body(body);
    return decl;
  }
  func_t define_func(subvtree body,arity_t a) {
    return define_func(body,declare_func(a));
  }

  //evaluation
  void eval(const_subvtree src,subvtree dst) {
    (*vertex_cast<func_t>(src.root()))(*this,src,dst);
  }

  template<typename T>
  T eval_to(const_subvtree src) { 
    vtree tmp;
    eval(src,tmp);
    assert(tmp.childless());
    return vertex_cast<T>(tmp.root());
  }  

  //type creation
  template<typename Type>
  func_t get_type() { return Type::instance(); }//fixme get rid of this

  //type lookups for arguments fixme
  func_t arg_type(func_t f,arity_t a) const { return f->arg_type(a); }
  bool func_arg_type(func_t f,arity_t a) const { 
    return arg_type(f,a)==func_type::instance(); 
  }

  friend void initialize_lib(context&);
  friend struct arg_func;//fixme get rig
 protected:
  typedef boost::ptr_vector<func_base> func_vector;
  context* _parent;
  context* _root;

  func_vector _funcs;

  template<typename Func>
  void insert_builtin(Func* f) { _funcs.push_back(f); }

  const_subvtree lookup_arg(arity_t i) const;
};

}} //namespace plap::lang
#endif //PLAP_LANG_context_H__
