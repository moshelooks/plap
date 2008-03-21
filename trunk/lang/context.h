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
#include "slist.h"
#include "vtree.h"
#include "func.h"

namespace plap { namespace lang {

struct lang_variable;

struct context : public boost::noncopyable {

  //create a parent context
  context() : _parent(this),_root(this) {}

  //create a child context
  context(context& p) : _parent(&p),_root(_parent->_root) {}

  lang_def* declare(arity_t a,arity_t o);
  void define(subvtree body,lang_def* d); //this splices out body
  void erase_last_var() { _defs.pop_back(); }  //needed for error recovery

  //evaluation methods
  void eval(const_subvtree src,subvtree dst);
  template<typename T>
  T eval_to(const_subvtree src) { 
    vtree tmp=vtree(vertex());
    eval(src,tmp);
    assert(tmp.childless());
    return arg_cast<T>(tmp.root());
  }

  /**  struct bind_seq : public std::vector<vtree> {
    bind_seq() {}
    bind_seq(arity_t a,arity_t o)
        : std::vector<vtree>(a,vtree(vertex())),offset(o) {}
    vtree& operator[](arity_t a) {
      assert(a<this->size()+offset && a>=offset);
      return std::vector<vtree>::operator[](a-offset);
    }
    const vtree& operator[](arity_t a) const {
      assert(a<this->size()+offset && a>=offset);
      return std::vector<vtree>::operator[](a-offset);
    }
    arity_t offset;
  };
  **/
  typedef std::vector<vtree> bind_seq;
  void pop_arg_bindings() { 
    assert(!_args.empty());
    _args.pop_front();
  }
  bind_seq& push_arg_bindings(bind_seq& b) {
    _args.push_front(bind_seq());
    b.swap(_args.front());
    return _args.front();
  }
  const vtree& arg_binding(arity_t idx) const { return _args.front()[idx]; }
  const vtree* def_binding(func_t f) const {
    let_map::const_iterator i=_lets.find(f);
    if (i==_lets.end())
      return f->body();
    return i->second;
  }
  void bind_def(func_t f);
 protected:
  context* _parent;
  context* _root;

  typedef boost::ptr_vector<lang_def> def_vector;
  def_vector _defs;

  const_subvtree lookup_arg(arity_t i) const;

  util::slist<bind_seq> _args;
  typedef tr1::unordered_map<func_t,util::slist<vtree> > let_map;
  let_map _lets;
};

}} //namespace plap::lang
#endif //PLAP_LANG_context_H__
