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

#ifndef PLAP_LANG_LAZY_FUNC_H__
#define PLAP_LANG_LAZY_FUNC_H__

#define LANG_LIMIT_ARITY_INC BOOST_PP_INC(LANG_LIMIT_ARITY)
#define PLAP_LANG_lazy_func(z,n,u)                                      \
  template<n,const char* Name>                                          \
  struct lazy_func<n,Name>                                              \
      : public stateless_func<lazy_func<n,Name>,n,Name> {               \
    lazy_func(const Func& f) : _base(f) {}                              \
    void operator()(context& c,const_subvtree loc,subvtree dst) const { \
      assert(loc.arity()==n);                                           \
      BOOST_PP_REPEAT(n,PLAP_LANG_vtree_decl,~);                        \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      BOOST_PP_REPEAT_FROM_TO(1,n,PLAP_LANG_vtree_eval,~);              \
      _base(c,BOOST_PP_ENUM(n,PLAP_LANG_call_arg,~),dst);               \
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
namespace plap { namespace lang {

template<arity_t Arity,const char* Name>
struct lazy_func;
BOOST_PP_REPEAT_FROM_TO(1,LANG_LIMIT_ARITY_INC,PLAP_LANG_eager_func,~);

//convenience caller, returns a new eager_func on the heap
template<arity_t Arity,const char* Name>
lazy_func* make_lazy(const Func& f) { return new lazy_func(f) }

}} //namespace plap::lang
#endif //PLAP_LANG_LAZY_FUNC_H__