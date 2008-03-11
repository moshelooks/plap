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

// All library functions should appear in initialize_lib, which is called to
// add library functions to an context.

#ifndef PLAP_LANG_BUILTIN_H__
#define PLAP_LANG_BUILTIN_H__

#include <numeric>
#include "context.h"
#include "cast.h"

namespace plap { namespace lang {

void initialize_lib(context& c);

typedef const_subvtree   any;
typedef list_of<any> any_list;

template<typename>
struct builtin;

//generates a builtin struct for some arity
#define LANG_LIMIT_ARITY_INC BOOST_PP_INC(LANG_LIMIT_ARITY)
#define PLAP_LANG_type_params(n) BOOST_PP_ENUM_PARAMS(n,typename Input)
#define PLAP_LANG_name(n) builtin<Subtype(BOOST_PP_ENUM_PARAMS(n,Input))>
#define PLAP_LANG_vtree_decl(z,n,u) vtree tr ## n=vtree(vertex());
#define PLAP_LANG_vtree_eval(z,n,u) c.eval(*child++,tr ## n);
#define PLAP_LANG_call_arg(z,n,u) literal_cast<Input ## n>(tr ## n)
#define PLAP_LANG_builtin(z,n,u)                                        \
  template<typename Subtype,PLAP_LANG_type_params(n) >                  \
  struct PLAP_LANG_name(n)                                              \
      : public stateless_func<Subtype,n> {                              \
    void operator()(context& c,const_subvtree loc,subvtree dst) const { \
      assert(loc.arity()==n);                                           \
      BOOST_PP_REPEAT(n,PLAP_LANG_vtree_decl,~);                        \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      c.eval(*child,tr0);                                               \
      BOOST_PP_REPEAT_FROM_TO(1,n,PLAP_LANG_vtree_eval,~);              \
      (*static_cast<Subtype*>(this))                                    \
          (c,BOOST_PP_ENUM(n,PLAP_LANG_call_arg,~),dst);                \
    }                                                                   \
    template<BOOST_PP_ENUM_PARAMS(n,typename T)>                        \
    void operator()(context&,BOOST_PP_ENUM_BINARY_PARAMS(n,const T,&t), \
                    subvtree dst) const {                               \
      (*static_cast<Subtype*>(this))(BOOST_PP_ENUM_PARAMS(n,t),dst);    \
    }                                                                   \
  };
BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,PLAP_LANG_builtin,~)

template<typename>
struct builtin_vararg;

template<typename Subtype,typename Argtype>
struct builtin_vararg<Subtype(Argtype)> :public builtin<Subtype(Argtype)> {
  arity_t arity() const { return variadic_arity; }
  void operator()(context& c,const_subvtree loc,subvtree dst) const {
    (*static_cast<Subtype*>(this))(c,list_of<Argtype>(loc),dst);
  }
};
  

//conditionals
struct lang_if : public builtin<lang_if(bool,any,any)> {
  void operator()(context& c,bool cond,any if_br,any else_br,subvtree dst) {
    c.eval(cond ? if_br : else_br,dst);
  }
};

//arithmetic operators
struct lang_plus : public builtin_vararg<lang_plus(number_t)> {
  void operator()(list_of<number_t> l,subvtree dst) const {
    dst.root()=arg(std::accumulate(l.begin(),l.end(),number_t(0)));
  }
};

//list operators
struct lang_concat : public builtin<lang_concat(any_list,any_list)> {
  void operator()(any_list a,any_list b,subvtree dst) const {
    assert(a.root()==b.root());
    dst=a;
    dst.append(b.begin_sub_child(),b.end_sub_child());
  }
};    
#if 0
//functional programming constructs
inline disc_t lang_foreach(list_of<const_subvtree> l,
                           func_of<const_subvtree(const_subvtree)> f) {
  //fixmestd::for_each(l.begin(),l.end(),f);
  return 0;
}

disc_t lang_print(const_subvtree v) { 
  std::cout << "yuk ";
  //foreach (const_subvtree s,l) std::cout << "yuk ";
  return 0;
}
#endif
}} //namespace plap::lang

#endif //PLAP_LANG_BUILTIN_H__
