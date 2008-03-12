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
#include "core.h"

namespace plap { namespace lang {

void initialize_lib(context& c);

typedef const_subvtree   any;
typedef list_of<any> any_list;

template<typename T>
struct arg_helper {
  vtree arg;
  arg_helper(context& c,const_subvtree s) : arg(vertex()) { c.eval(s,arg); }
  T operator()() const { return literal_cast<T>(arg); }
};
template<>
struct arg_helper<any> {
  any arg;
  arg_helper(context& c,const_subvtree s) : arg(s) {}
  any operator()() const { return arg; }
};

// every builtin function should be a class C that inherits from 
// builtin<C(Arg0,Arg1,...,ArgN)>, or from builtin_varidatic<C(Argtype)>,
// and defines either eval(context&,Arg0,...,ArgN,subvtree) or
// cfeval(Arg0,...,ArgN,subvtree) (cf = context-free)

template<typename>
struct builtin;
//generates a builtin struct for some arity
#define LANG_LIMIT_ARITY_INC BOOST_PP_INC(LANG_LIMIT_ARITY)
#define PLAP_LANG_type_params(n) BOOST_PP_ENUM_PARAMS(n,typename Input)
#define PLAP_LANG_name(n) builtin<Subtype(BOOST_PP_ENUM_PARAMS(n,Input))>
#define PLAP_LANG_arg_helper(z,n,u)                                     \
  arg_helper<Input ## n> arg ## n=arg_helper<Input ## n>(c,*++child);
#define PLAP_LANG_call_arg(z,n,u) arg ## n ()
#define PLAP_LANG_builtin(z,n,u)                                        \
  template<typename Subtype,PLAP_LANG_type_params(n) >                  \
  struct PLAP_LANG_name(n)                                              \
      : public stateless_func<Subtype,n> {                              \
    void operator()(context& c,const_subvtree loc,subvtree dst) const { \
      assert(loc.arity()==n);                                           \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      arg_helper<Input0> arg0=arg_helper<Input0>(c,*child);             \
      BOOST_PP_REPEAT_FROM_TO(1,n,PLAP_LANG_arg_helper,~);              \
      static_cast<const Subtype*>(this)->eval                           \
          (c,BOOST_PP_ENUM(n,PLAP_LANG_call_arg,~),dst);                \
    }                                                                   \
    void eval(context&,BOOST_PP_ENUM_BINARY_PARAMS(n,Input,t),          \
              subvtree dst) const {                                     \
      static_cast<const Subtype*>(this)->cfeval                         \
          (BOOST_PP_ENUM_PARAMS(n,t),dst);                              \
    }                                                                   \
  };
BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,PLAP_LANG_builtin,~)

template<typename>
struct builtin_variadic;
template<typename Subtype,typename Argtype>
struct builtin_variadic<Subtype(Argtype)>
    : public builtin<Subtype(list_of<Argtype>)> {
  arity_t arity() const { return variadic_arity; }
  void operator()(context& c,const_subvtree loc,subvtree dst) const {
    vtree tmp=vtree(vertex());
    (*lang_list::instance())(c,loc,tmp);
    static_cast<const Subtype*>(this)->eval
        (c,literal_cast<list_of<Argtype> >(tmp),dst);
  }
};

//conditionals
struct lang_if : public builtin<lang_if(bool,any,any)> {
  void eval(context& c,bool cond,any if_br,any else_br,subvtree dst) const {
    c.eval(cond ? if_br : else_br,dst);
  }
};

//arithmetic operators
template<typename Func,int Init>
struct lang_acc : public builtin_variadic<lang_acc<Func,Init>(number_t)> {
  void cfeval(list_of<number_t> l,subvtree dst) const {
    std::cout << "XX" << std::endl;
    std::cout << "XX" << l.size() << std::endl;
    dst.root()=arg(std::accumulate(l.begin(),l.end(),number_t(Init),Func()));
  }
};
typedef lang_acc<std::plus<number_t>,0>       lang_plus;
typedef lang_acc<std::multiplies<number_t>,1> lang_times;

//comparison operators - can take any type
template<typename Func>
struct lang_cmp : public builtin<lang_cmp<Func>(any,any)> {
  void eval(context& c,any a,any b,subvtree dst) const {
    vtree tmp1=vtree(vertex()),tmp2=vtree(vertex(vertex()));
    c.eval(a,tmp1);
    c.eval(b,tmp2);
    dst.root()=arg(Func()(tmp1,tmp2));
  }
};
typedef lang_cmp<std::less<any> >          lang_less;
typedef lang_cmp<std::less_equal<any> >    lang_less_equal;
typedef lang_cmp<std::greater<any> >       lang_greater;
typedef lang_cmp<std::greater_equal<any> > lang_greater_equal;

typedef lang_cmp<std::equal_to<any> >      lang_equal;
typedef lang_cmp<std::not_equal_to<any> >  lang_not_equal;

//list operators
struct lang_concat : public builtin<lang_concat(any_list,any_list)> {
  void cfeval(any_list a,any_list b,subvtree dst) const {
    if (a.empty() && b.empty()) {
      dst.root()=nil();
    } else {
      dst.root()=call(lang_list::instance());
      dst.append(a.begin(),a.end());
      dst.append(b.begin(),b.end());
    }
  }
};

//functional programming constructs
struct lang_apply : public builtin<lang_apply(any,any_list)> {
  void eval(context& c,any f,any_list args,subvtree dst) const {
    if (f.childless()) {
      (*arg_cast<func_t>(f.root()))(c,args.src,dst);
    } else {
      assert(false);
      //vtree tmp=vtree(vertex());
      //c.eval(f,tmp);
    }
  }
};

#if 0
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
