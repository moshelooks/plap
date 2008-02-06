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

#ifndef PLAP_LANG_ENVIRONMENT_H__
#define PLAP_LANG_ENVIRONMENT_H__

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include "func.h"
/***fixme
#include "vtree.h"
#include "cast.h"
***/
//#include "type.h"
//#include "def.h"

namespace plap { namespace lang {

//struct type;
typedef func* func_t;

struct environment : public boost::noncopyable {
  func_t create_func(arity_t a) { 
    _funcs.push_back(new func(a)); 
    return &_funcs.back(); 
  }
  func_t create_func(arity_t a,const std::string& name) {
    return _names.insert(make_pair(name,create_func(a))).first->second;
  }

  func_t name2func(const std::string& name) const {
    name_index::index<left>::type::const_iterator i=
        boost::multi_index::get<left>(_names).find(name);
    return i==boost::multi_index::get<left>(_names).end() ? NULL : i->second;
  }
  const std::string* func2name(func_t f) const { 
    name_index::index<right>::type::const_iterator i=
        boost::multi_index::get<right>(_names).find(f);
    return i==boost::multi_index::get<right>(_names).end() ? NULL : &i->first;
  }


  //fixme,const type& t);
  //func& create_func(const type& t);

  //takes ownership of the definition
  //fixme
  //  void bind(func& f,def* d);

  //  const func_name_map& func2name() const { return _func2name; }
  //  const func_name_map& name2func() const { return _name2func; }

 protected:
  typedef boost::ptr_vector<func> func_vector;
  struct left {};
  struct right {};
  typedef std::pair<std::string,func*> name_t;
  typedef boost::multi_index_container
  <name_t,
   boost::multi_index::indexed_by<
     boost::multi_index::hashed_unique<
       boost::multi_index::tag<left>,
       boost::multi_index::member<name_t,std::string,&name_t::first> >,
     boost::multi_index::hashed_unique<
       boost::multi_index::tag<right>,
       boost::multi_index::member<name_t,func*,&name_t::second> > > >
  name_index;
  func_vector _funcs;
  name_index _names;
};

}} //namespace plap::lang
#endif //PLAP_LANG_ENVIRONMENT_H__
