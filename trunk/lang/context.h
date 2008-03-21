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

#include <tr1/unordered_map>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/noncopyable.hpp>
#include <boost/bind.hpp>
#include "algorithm.h"
#include "slist.h"
#include "vtree.h"
#include "func.h"
#include "core.h"

namespace plap { namespace lang {

struct context : public boost::noncopyable {

  //create a parent context
  context() : _parent(this),_root(this) {}

  //create a child context
  context(context& p) : _parent(&p),_root(_parent->_root) {}

  lang_ident* declare(arity_t a,arity_t o);
  void define(lang_ident*,subvtree body); //this splices out body
  void erase_last_decl();  //needed for error recovery

  //evaluation methods
  void eval(const_subvtree src,subvtree dst);
  template<typename T>
  T eval_to(const_subvtree src) { 
    vtree tmp=vtree(vertex());
    eval(src,tmp);
    assert(tmp.childless());
    return arg_cast<T>(tmp.root());
  }

  //scalar bindings
  template<typename Iterator>
  void scalar_bind(arity_t offset,Iterator f,Iterator l) {
    std::size_t n=std::distance(f,l);
    if (offset==0) {
      _scalars.push_front(vtree_seq(n,vtree(vertex())));
    } else {
      assert(offset==_scalars.front().size());
      _scalars.front().resize(_scalars.front().size()+n,vtree(vertex()));
    }
    util::for_each(f,l,_scalars.front().end()-n,
                   boost::bind(&context::eval,this,_1,_2));
  }
  void scalar_unbind(arity_t n) {
    assert(_scalars.front().size()>=n);
    if (n==_scalars.front().size())
      _scalars.pop_front();
    else
      _scalars.front().resize(_scalars.front().size()-n);
  }
  
  //identifier bindings
  void ident_bind(func_t f,const_subvtree binding);
  void ident_unbind(func_t f);
 protected: 
  typedef std::vector<vtree> vtree_seq;
  typedef util::slist<vtree> vtree_list;
  typedef std::tr1::unordered_map<func_t,vtree_list> ident_map;

  context* _parent;
  context* _root;
  boost::ptr_vector<lang_ident> _decls;
  util::slist<vtree_seq> _scalars;
  ident_map _idents;
};

}} //namespace plap::lang
#endif //PLAP_LANG_context_H__
