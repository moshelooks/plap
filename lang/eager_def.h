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

#ifndef PLAP_LANG_EAGER_DEF_H__
#define PLAP_LANG_EAGER_DEF_H__

//there are two publically exposed interfaces here. The first, make_eager_def,
//is for functions that return a single value. The second, make_eager_list_def,
//is for functions the return lists of values, which should be implemented to
//take an output iterator as their last argument.

#include <boost/preprocessor.hpp>
#include "core.h"
#include "cast.h"

//this determines the maximal arity of defs in the language
#ifndef LANG_LIMIT_ARITY
#  define LANG_LIMIT_ARITY 4
#endif
#define LANG_LIMIT_ARITY_INC BOOST_PP_INC(LANG_LIMIT_ARITY)

//helper functions for generating eager_def structs
#define LANG_DEF_type_params(n) \
  BOOST_PP_ENUM_PARAMS(n,typename Input),typename Base
#define LANG_DEF_params(n) \
  BOOST_PP_ENUM_PARAMS(n,Input),Base
#define LANG_DEF_vtree_decl(z,n,u) vtree tr ## n=vtree(vertex());
#define LANG_DEF_vtree_eval(z,n,u) eval<Input ## n>()(child++,tr ## n);
#define LANG_DEF_call_arg(z,n,u) literal_cast<Input ## n>(tr ## n)
//generates an eager_def struct for some arity
#define LANG_DEF_eager_def(z,n,u)                                       \
  template<LANG_DEF_type_params(n)>                                     \
  struct eager_def ## n : public def {                                  \
    eager_def ## n(const Base& fun) : base(fun) {}                      \
    void operator()(const_vsubtree loc,vsubtree dst) const {            \
      BOOST_PP_REPEAT(n,LANG_DEF_vtree_decl,~);                         \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      eval<Input0>()(*child,tr0);                                       \
      BOOST_PP_REPEAT_FROM_TO(1,n,LANG_DEF_vtree_eval,~);               \
      dst.root()=base(BOOST_PP_ENUM(n,LANG_DEF_call_arg,~));            \
    }                                                                   \
    Base base;                                                          \
  };
//generates an overload of the convenience function make_eager_def
#define LANG_DEF_make_eager(z,n,u)                                      \
  template<LANG_DEF_type_params(n)>                                     \
  def* make_eager_def(const Base& base) {                               \
    return new lang_private::eager_def ## n<LANG_DEF_params(n)>(base);  \
  }

namespace lang { //the actual code generation occurs here
namespace lang_private {
BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,LANG_DEF_eager_def,~);
} //~namespace lang_private

BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,LANG_DEF_make_eager,~);
} //~namespace lang

//clean up our macro mess
#undef LANG_DEF_make_eager
#undef LANG_DEF_eager_def
#undef LANG_DEF_call_arg
#undef LANG_DEF_vtree_eval
#undef LANG_DEF_vtree_decl
#undef LANG_DEF_params
#undef LANG_DEF_type_params
#undef LANG_DEF_LIMIT_INC

#endif  // PLAP_LANG_EAGER_DEF_H__
