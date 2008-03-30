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
#include "foreach.h"
#include "algorithm.h"
#include "slist.h"
#include "vtree.h"
#include "func.h"
#include "core.h"

#include "pretty_print.h"//fixme

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
    std::cout << "scalar bind " << int(offset)
              << "|" << std::distance(f,l)
              << "|" << *f << std::endl;
    //first eval, then add (we must not mung _scalars until after evaling)
    vtree_seq args(std::distance(f,l),vtree(vertex()));
    util::for_each(f,l,args.begin(),boost::bind(&context::eval,this,_1,_2));

    _scalars.push_front(make_pair(vtree_seq(),offset));
    std::swap(args,_scalars.front().first);
  }
  void scalar_unbind(arity_t n) {
    std::cout << "unbind!" << std::endl;
    assert(_scalars.front().first.size()==n);
    _scalars.pop_front();
  }
  const_subvtree scalar(arity_t idx) const;
  
  //identifier bindings
  void ident_bind(func_t f,const_subvtree binding);
  void ident_unbind(func_t f);
  const_subvtree ident_binding(func_t f) const {
    ident_map::const_iterator i=_idents.find(f);
    if (i==_idents.end()) {
      assert(f->body());
      return *f->body();
    }
    return i->second.front();
  }
 protected: 
  typedef std::vector<vtree> vtree_seq;
  typedef util::slist<vtree> vtree_list;
  typedef std::tr1::unordered_map<func_t,vtree_list> ident_map;
  typedef util::slist<std::pair<vtree_seq,arity_t> > scalar_list;

  context* _parent;
  context* _root;
  boost::ptr_vector<lang_ident> _decls;
  scalar_list _scalars;
  ident_map _idents;
};

}} //namespace plap::lang
#endif //PLAP_LANG_context_H__
