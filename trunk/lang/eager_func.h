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

#ifndef PLAP_LANG_EAGER_FUNC_H__
#define PLAP_LANG_EAGER_FUNC_H__

#include <boost/preprocessor.hpp>
#include "context.h"
#include "cast.h"
#include "type.h"

//helpers for generating eager_def structs
#define LANG_LIMIT_ARITY_INC BOOST_PP_INC(LANG_LIMIT_ARITY)
#define PLAP_LANG_type_params(n) BOOST_PP_ENUM_PARAMS(n,typename Input)
#define PLAP_LANG_name(n) \
  eager_func<Func,func_of<Return(BOOST_PP_ENUM_PARAMS(n,Input))> >
#define PLAP_LANG_vtree_decl(z,n,u) vtree tr ## n=vtree(vertex());
#define PLAP_LANG_vtree_eval(z,n,u) c.eval(*child++,tr ## n);
#define PLAP_LANG_call_arg(z,n,u) literal_cast<Input ## n>(tr ## n)
#define PLAP_LANG_case(z,n,u)                                   \
  case n: return type_placeholder<Input ## n>::instance();      \

//generates an eager_func struct for some arity
#define PLAP_LANG_eager_func(z,n,u)                                     \
  template<typename Func,typename Return,                               \
           PLAP_LANG_type_params(n)>                                    \
  struct PLAP_LANG_name(n)                                              \
      : public stateless_func<PLAP_LANG_name(n),n> {                    \
    eager_func(const Func& f) : _base(f) {}                             \
    void operator()(context& c,const_subvtree loc,subvtree dst) const { \
      assert(loc.arity()==n);                                           \
      BOOST_PP_REPEAT(n,PLAP_LANG_vtree_decl,~);                        \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      c.eval(*child,tr0);                                               \
      BOOST_PP_REPEAT_FROM_TO(1,n,PLAP_LANG_vtree_eval,~);              \
      dst.root()=_base(BOOST_PP_ENUM(n,PLAP_LANG_call_arg,~));          \
    }                                                                   \
    func_t arg_type(arity_t a) const {                                  \
      switch(a) {                                                       \
        BOOST_PP_REPEAT(n,PLAP_LANG_case,~);                            \
      }                                                                 \
      assert(false);                                                    \
    }                                                                   \
   protected:                                                           \
    Func _base;                                                         \
  };

namespace plap { namespace lang { //the actual code generation occurs here

template<typename Func,typename FuncOf>
struct eager_func;
BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,PLAP_LANG_eager_func,~);

//convenience caller, returns a new eager_func on the heap
template<typename FuncOf,typename Func>
eager_func<Func,FuncOf>* make_eager(const Func& f) { 
  return new eager_func<Func,FuncOf>(f);
}

}} //namespace plap::lang

//clean up our macro mess
#undef PLAP_LANG_eager_func
#undef PLAP_LANG_case
#undef PLAP_LANG_call_arg
#undef PLAP_LANG_vtree_eval
#undef PLAP_LANG_vtree_decl
#undef PLAP_LANG_name
#undef PLAP_LANG_type_params
#undef PLAP_LANG_LIMIT_INC

#endif //PLAP_LANG_EAGER_FUNC_H__
