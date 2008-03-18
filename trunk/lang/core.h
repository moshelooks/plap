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

#ifndef PLAP_LANG_CORE_H__
#define PLAP_LANG_CORE_H__

#include <boost/bind.hpp>
#include "algorithm.h"
#include "context.h"
#include "cast.h"

namespace plap { namespace lang {

struct lang_list : public stateless_func<lang_list,variadic_arity> {
  void operator()(context& c,const_subvtree s,subvtree d) const { 
    assert(!s.childless());
    assert(d.childless());
    d.root()=call(this);
    d.append(s.arity(),vertex());
    util::for_each(s.begin_sub_child(),s.end_sub_child(),d.begin_sub_child(),
                   boost::bind(&context::eval,&c,_1,_2));
  }
};

#define LANG_CORE_make_func(name,arity)                                   \
  struct lang_ ## name : public stateless_func<lang_ ## name,arity> {     \
    inline void operator()(context& c,const_subvtree s,subvtree d) const; \
  };                                                                      \
  void lang_ ## name::operator()(context& c,const_subvtree s,subvtree d) const


LANG_CORE_make_func(def,3) {}

LANG_CORE_make_func(decl,2) {}

LANG_CORE_make_func(lambda,1) {}

LANG_CORE_make_func(arrow,2) {}

#undef LANG_CORE_make_func

}} //namespace plap::lang

#endif //PLAP_LANG_CORE_H__

#if 0
void lang_apply2(const_subvtree l,list_of<const_subvtree> r) {


  vtree tmp=vtree(vertex());
  c.eval(s.front_sub(),tmp);
  util::foreach
  tmp.append(s.back_sub().begin_sub_child(),s.back_sub().end_sub_child());
}

{
  
  func_of<T(U)> l=get_type(s[0]);
#endif  
