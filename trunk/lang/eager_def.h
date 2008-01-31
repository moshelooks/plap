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

#include <boost/preprocessor.hpp>
#include <boost/utility/result_of.hpp>
#include "iterator_shorthands.h"
#include "environment.h"
#include "vertex_cast.h"

//this determines the maximal arity of defs in the language
#ifndef LANG_LIMIT_ARITY
#  define LANG_LIMIT_ARITY 4
#endif
#define LANG_LIMIT_ARITY_INC BOOST_PP_INC(LANG_LIMIT_ARITY)

//define eager defintion wrappers up to limit arity
#define LANG_DEF_type_params(n) \
  typename Base,BOOST_PP_ENUM_PARAMS(n,typename Input)
#define LANG_DEF_params(n) \
  Base,BOOST_PP_ENUM_PARAMS(n,Input)
#define LANG_DEF_vtree_decl(z,n,u) vtree tr ## n(0);
#define LANG_DEF_vtree_eval(z,n,u) env.eval<Input ## n>(child++,tr ## n)
#define LANG_DEF_call_arg(z,n,u) value_caster<Input ## n>()(tr ## n)
#define LANG_DEF_eager_def(z,n,u)                                       \
  template<LANG_DEF_type_params(n)>                                     \
  struct eager_def ## n : public def {                                  \
    eager_def ## n(const Base& fun) : base(fun) {}                      \
    void operator()(const_vsubtree loc,vsubtree dst,                    \
                    environment& env) const {                           \
      BOOST_PP_REPEAT(n,LANG_DEF_vtree_decl,~);                         \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      env.eval<Input0>(*child,tr0);                                     \
      BOOST_PP_REPEAT_FROM_TO(1,n,LANG_DEF_vtree_eval,~);               \
      dst.root()=base(BOOST_PP_ENUM(n,LANG_DEF_call_arg,~));            \
    }                                                                   \
    Base base;                                                          \
  };
#define LANG_DEF_eager_maker(z,n,u)                                     \
  template<LANG_DEF_type_params(n)>                                     \
  struct eager_maker ## n {                                             \
    eager_maker ## n(const Base& b) {                                   \
      d=new eager_def ## n<LANG_DEF_params(n)>(b);                      \
    }                                                                   \
    def* d;                                                             \
  };

//helper macros for LANG_DEF_list_overload
#define LANG_DEF_list_emit_if0(z,u,i,elem) \
  BOOST_PP_IF(elem,(list_of<Input ## i>),(Input ## i))
#define LANG_DEF_list_emit_if1(z,u,i,elem)                              \
  BOOST_PP_IF                                                           \
  (elem,                                                                \
   (typename util::slist<Input ## i>::const_iterator)                   \
   (typename util::slist<Input ## i>::const_iterator),                  \
   (Input ## i))
#define LANG_DEF_list_emit_if2(z,u,i,elem)                              \
  BOOST_PP_IF                                                           \
  (elem,                                                                \
   (list_t input ## i),                                                 \
   (Input ## i input ## i))
#define LANG_DEF_list_emit_if3(z,u,i,elem)                              \
  BOOST_PP_IF                                                           \
  (elem,                                                                \
   (util::transform_it(input ## i->begin(),evaluator<Input ## i>(env))) \
   (util::transform_it(input ## i->end(),evaluator<Input ## i>())),     \
   (input ## i))
#define LANG_DEF_or(s,p,q) BOOST_PP_OR(p,q)
#define LANG_DEF_not_all_zeros(seq) \
  BOOST_PP_SEQ_FOLD_LEFT(LANG_DEF_or,0,seq)
#define LANG_DEF_list_specialization(naz,seq)                           \
  BOOST_PP_IF(naz,<Base,)                                               \
  BOOST_PP_COMMA_IF(naz)                                                \
  BOOST_PP_SEQ_ENUM                                                     \
  (BOOST_PP_IF(naz,BOOST_PP_SEQ_FOR_EACH_I                              \
               (LANG_DEF_list_emit_if0,~,seq),()))                      \
  BOOST_PP_IF(naz,>,)

//this is to be called on all binary seqs on length n
#define LANG_DEF_list_overload(z,seq)                                   \
  template<LANG_DEF_type_params(BOOST_PP_SEQ_SIZE(seq))>                \
  struct BOOST_PP_CAT(list_adapter,BOOST_PP_SEQ_SIZE(seq))              \
  LANG_DEF_list_specialization(LANG_DEF_not_all_zeros(seq),seq)         \
  {                                                                     \
    BOOST_PP_CAT(list_adapter,BOOST_PP_SEQ_SIZE(seq))(const Base& b)    \
        : base(b) {}                                                    \
    typename boost::result_of<Base(                                     \
        BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_FOR_EACH_I(                      \
                              LANG_DEF_list_emit_if1,~,seq)))>          \
    operator()(BOOST_PP_SEQ_ENUM(                                       \
                   BOOST_PP_SEQ_FOR_EACH_I(                             \
                       LANG_DEF_list_emit_if2,~,seq))) const {          \
      return base(                                                      \
          BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_FOR_EACH_I(                    \
                                LANG_DEF_list_emit_if3,~,seq)));        \
    }                                                                   \
    Base base;                                                          \
  };

//helper macros for LANG_DEF_list_adapter
#define LANG_DEF_identity(z,n,u) (u)
#define LANG_DEF_repeat_seq(v,n) BOOST_PP_REPEAT(n,LANG_DEF_identity,v)
#define LANG_DEF_exp(n)                                               \
  BOOST_PP_SEQ_FOR_EACH_PRODUCT(                                      \
      LANG_DEF_list_overload,LANG_DEF_repeat_seq((1)(0),n))

//the emits the prototype, followed by 2^n overloads
#define LANG_DEF_list_adapter(z,n,u)                           \
  template<LANG_DEF_type_params(n)>                            \
  struct list_adapter ## n;                                    \
  LANG_DEF_exp(n)

#define LANG_DEF_make_eager(z,n,u)                                            \
  template<LANG_DEF_type_params(n)>                                           \
  def* make_eager_def(const Base& base,BOOST_PP_ENUM_PARAMS(n,Input)) {       \
    return lang_private::eager_maker ## n<                                    \
    lang_private::list_adapter ## n<LANG_DEF_params(n)>,                      \
        BOOST_PP_ENUM_PARAMS(n,Input)>                                        \
        (lang_private::list_adapter ## n<LANG_DEF_params(n)>(base)).d;        \
  }

//the actual code generation occurs here
namespace lang {

template<typename T>
struct value_caster {
  T operator()(vconst_subtree s) { return vertex_cast<T>(s.root()); }
};

template<typename T>
struct value_caster<list_of<T> > {
  vconst_subtree operator()(vconst_subtree s) { return s; }
};

namespace lang_private {

BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,LANG_DEF_eager_def,~);

BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,LANG_DEF_eager_maker,~);

BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,LANG_DEF_list_adapter,~);

} //~namespace lang_private

BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,LANG_DEF_make_eager,~);

//clean up our macro mess
#undef LANG_DEF_eager_maker
#undef LANG_DEF_param_basis
#undef LANG_DEF_eager_maker
#undef LANG_DEF_make_overload
#undef LANG_DEF_make_eager
#undef LANG_DEF_list_adapter
#undef LANG_DEF_exp
#undef LANG_DEF_repeat_seq
#undef LANG_DEF_identity
#undef LANG_DEF_list_overload
#undef LANG_DEF_list_specialization
#undef LANG_DEF_not_all_zeros
#undef LANG_DEF_and
#undef LANG_DEF_list_emit_if3
#undef LANG_DEF_list_emit_if2
#undef LANG_DEF_list_emit_if1
#undef LANG_DEF_list_emit_if0
#undef LANG_DEF_eager_maker
#undef LANG_DEF_eager_def
#undef LANG_DEF_call_arg
#undef LANG_DEF_vtree_eval
#undef LANG_DEF_vtree_decl
#undef LANG_DEF_params
#undef LANG_DEF_type_params
#undef LANG_DEF_LIMIT_INC

#endif  // PLAP_LANG_EAGER_DEF_H__
