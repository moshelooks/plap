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
#include <tr1/unordered_map>

#include <string>
#include "bimap.h"
#include "type.h"

namespace plap { namespace lang {

struct context : public boost::noncopyable {
  typedef std::vector<std::string> argname_seq;

  //create a parent context
  context() : _parent(this),_root(this) { initialize_lib(*this); }

  //create a child context
  context(context& p) : _parent(&p),_root(_parent->_root) {}

  //declarations are only needed for defining mutually recurize functions
  func_t declare_func(arity_t a,const std::string& name) {
    return _names.insert(make_pair(name,declare_func(a))).first->second;
  }
  func_t declare_func(arity_t a) {
    _funcs.push_back(new func(a));
    return &_funcs.back();
  }

  //these all splice out body - so if you want to keep it, make a copy
  template<typename Iterator>
  func_t define_func(Iterator f,Iterator l,subvtree body,
                     const std::string& name) { 
    return define_func(f,l,body,declare_func(std::distance(f,l),name));
  }
  template<typename Iterator>
  func_t define_func(Iterator f,Iterator l,subvtree body) { 
    return define_func(f,l,body,new func(std::distance(f,l)));
  }
  template<typename Iterator>
  func_t define_func(Iterator f,Iterator l,subvtree body,func_t decl) { 
    assert(_argnames.find(decl)==_argnames.end());
    assert(dynamic_cast<const func*>(decl));
    _argnames.insert(make_pair(decl,argname_seq(f,l)));
    const_cast<func*>(dynamic_cast<const func*>(decl))->set_body(body);
    return decl;
  }

  //type creation
  template<typename Type>
  func_t get_type() { return Type::instance(); }

  //lookup functions
  func_t name2func(const std::string& name) const {
    func_index::index<left>::type::const_iterator i=
        boost::multi_index::get<left>(_names).find(name);
    return i==boost::multi_index::get<left>(_names).end() ? NULL : i->second;
  }
  const std::string* func2name(func_t f) const { 
    func_index::index<right>::type::const_iterator i=
        boost::multi_index::get<right>(_names).find(f);
    return i==boost::multi_index::get<right>(_names).end() ? NULL : &i->first;
  }
  const argname_seq& argnames(func_t f) const {
    static const argname_seq empty;
    //argname_index::const_iterator i=_argnames.find(f);
    //if (i!=_argnames.end())
    //return i->second;
      return empty;
  }
  disc_t symbol2idx(const std::string& name) { return 0; }

  //evaluation
  template<typename T>
  void eval(const_subvtree src,subvtree dst) {}

  /* fixme
  what about different funcs pointing to different things in diferent contexts?
  need to store the various definition *in the context*
  maybe we can use function pointers now??
  */

  //fixme,const type& t);
  //func& create_func(const type& t);

  //takes ownership of the definition
  //fixme
  //  void bind(func& f,def* d);

  //  const func_name_map& func2name() const { return _func2name; }
  //  const func_name_map& name2func() const { return _name2func; }


  void to_list(subvtree s) {}
  void to_apply(subvtree s) {}
  void to_tuple(subvtree s) {}

  friend void initialize_lib(context&);
  friend struct arg_func;
 protected:
  typedef boost::ptr_vector<func_base> func_vector;
  struct left {};
  struct right {};
  typedef util::bimap<std::string,func_t,left,right>::type func_index;
  typedef std::tr1::unordered_map<func_t,argname_seq> argname_index;
  //fixmetypedef util::bimap<std::string,disc_t,left,right>::type symbol_index;

  context* _parent;
  context* _root;

  func_vector _funcs;
  func_index _names;
  argname_index _argnames;

  template<typename Func>
  void insert_builtin(Func* f,bool manage) {
    if (manage)
      _funcs.push_back(f);
    _names.insert(make_pair(boost::lexical_cast<std::string>(*f),f));
  }

  const_subvtree lookup_arg(arity_t i) const;
};

}} //namespace plap::lang
#endif //PLAP_LANG_context_H__
