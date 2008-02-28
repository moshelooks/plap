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
    : public stateless_func<lang_list<T>,variadic_arity,lang_io::list_name> {
  void operator()(context& c,const_subvtree s,subvtree d) const { 
    assert(d.childless());
    d.root()=func_t(this);
    d.append(s.arity(),vertex());
    util::for_each(s.begin_sub_child(),s.end_sub_child(),d.begin_sub_child(),
                   boost::bind(&context::eval<T>,&c,_1,_2));
  }
};

typedef lang_list<bool>           bool_list;
typedef lang_list<char>           char_list;
typedef lang_list<disc_t>         symbol_list;
typedef lang_list<contin_t>       number_list;
typedef lang_list<const_subvtree> any_list;

struct nil : public stateless_func<nil,0,lang_io::nil_name> {
  void operator()(context&,const_subvtree s,subvtree d) const { 
    d.root()=any_list::instance();
  }
};

struct arg_func : public narg_func<0> {
  arg_func(arity_t i) : _idx(i) {}
  void operator()(context& c,const_subvtree,subvtree d) const { 
    d=c.lookup_arg(_idx); 
  }
  static arg_func* instance(arity_t);
 protected:
  arity_t _idx;
};    

//def(name list(arg1 arg2 ...) body)
#if 0
struct def : public narg_func<3> {
  void operator()(const_subvtree s,subvtree d) const {
    
  }
};
template<typename T>
struct eval : public func_base {
  void operator()(const_subvtree s,subvtree d) const {
    assert(d.childless());
    if (s.childless())
      d.root()=s.root();
    else
    (*vertex_cast<def_t>(s.root()))(s,d);
  }
};
#endif

}} //namespace plap::lang

#endif //PLAP_LANG_CORE_H__
