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

#include <tr1/unordered_map>
#include <boost/noncopyable.hpp>
#include "slist.h"
#include "vertex_aux.h"
#include "types.h"
#include "func.h"
#include "def.h"

namespace lang {

struct environment : public boost::noncopyable {
  func& create_func(const std::string& name);
  func& create_func();

  //takes ownership of the definition
  void bind(func& f,def* d);

  template<typename T>
  void eval(const_vsubtree tr,vsubtree dst) {
    assert(dst.childless());
    if (tr.childless())
      dst.root()=tr.root();
    else
      (*vertex_cast<def_t>(tr.root()))(tr,dst,*this);
  }

  template<typename T>
  void eval<list_of<T> >(const_vsubtree tr,vsubtree dst) {
    assert(dst.childless());
    if (tr.childless()) { //an actual list
      list_t
      dst.root()=tr.root();
    else
      (*vertex_cast<def_t>(tr.root()))(tr,dst,*this);
  }
}

  
  // void eval(const_vsubtree tr,vsubtree dst);

 protected:
  typedef std::tr1::unordered_map<func*,std::string> func_name_map;
  typedef util::slist<func> func_list;

  func_name_map _names;
  func_list _funcs; 
};

template<typename T>
struct evaluator {
  evaluator() {}
  evaluator(environment& env) : _env(&env) {}

  const T& operator()(const_vsubtree tr) {
    vtree tmp(0);
    _env->eval(tr,tmp);
    assert(tmp.childless());
    return vertex_cast<T>(tmp.root());
  }
 protected:
  environment* _env;
};

} //~namespace lang

#endif  // PLAP_LANG_ENVIRONMENT_H__
