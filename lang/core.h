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

namespace plap { namespace lang {

template<typename T>
struct lang_list
    : public stateless_func<lang_list<T>,variadic_arity> {
  void operator()(context& c,const_subvtree s,subvtree d) const { 
    assert(d.childless());
    d.root()=func_t(this);
    d.append(s.arity(),vertex());
    util::for_each(s.begin_sub_child(),s.end_sub_child(),d.begin_sub_child(),
                   boost::bind(&context::eval,&c,_1,_2));
  }
};

typedef lang_list<bool>           bool_list;
typedef lang_list<char>           char_list;
typedef lang_list<id_t>           symbol_list;
typedef lang_list<number_t>       number_list;
typedef lang_list<func_t>         any_list;

typedef any_list nil;

#define LANG_CORE_make_func(name,arity)                                   \
  struct name : public stateless_func<name,arity> {                       \
    inline void operator()(context& c,const_subvtree s,subvtree d) const; \
  };                                                                      \
  void name::operator()(context& c,const_subvtree s,subvtree d) const

LANG_CORE_make_func(apply,2) {
  c.eval(s.front_sub(),d);
  d.append(s.begin_sub_child(),s.end_sub_child());
}

LANG_CORE_make_func(def,3) {}
//LANG_CORE_make_func(,);

#undef LANG_CORE_make_func

}} //namespace plap::lang

#endif //PLAP_LANG_CORE_H__
