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

#ifndef PLAP_LANG_FUNC_H__
#define PLAP_LANG_FUNC_H__

namespace lang {

struct func {};

#if 0

typedef boost::variant<
  BOOST_PP_ENUM
  boost:::function<disc_t(disc_t)>,
  boost:::function<contin_t(disc_t)>,
  util::slist< boost::recursive_variant_ >,
  boost::function<boost::recursive_variant_(boost::recursive_variant_)>,
  boost::function<boost::recursive_variant_(boost::recursive_variant_,
                                            boost::recursive_variant_)> 
  >::type typed_vertex;

typedef boost::function<typed_vertex

struct function {
  void call(vchild_sub_it f,vchild_sub_it l,vsubtree dst) {
    if (lazy())
      boost::apply_visitor(lazy_function_call(f,l,dst),_func);
  }

 protected:
  struct function_call : public boost::static_visitor<> {
    function_call(vchild_sub_it f,vchild_sub_it l,vsubtree dst);
  };
  struct lazy_function_call : public function_call {
    lazy_function_call(vchild_sub_it f,vchild_sub_it l,vsubtree dst)
        : function_call(f,l,dst) {}

    void operator()(const unary_lazy_f& f) const { f(*_f,dst); }
    void operator()(const binary_lazy_f& f) const { 
      f(*_f,*bost::next(_f),dst);
    }
  };

  struct eager_function_caller : public function_call {
    eager_function_call(vchild_sub_it f,vchild_sub_it l,vsubtree dst)
        : function_call(f,l,dst) {}

    template<typename T>
    void operator()(const unary_eager_returns<T>& f) const { 
      _dst.root()=f(*_f);
    }
    void operator()(const binary_lazy_f& f) const { 
      f(*_f,*bost::next(_f),dst);
    }
  };

};

  //////



  struct func_call : public boost::static_visitor<> {
    func_call(vchild_sub_it f_,vchild_sub_it l_,vsubtree dst_,environment& env)
        : f(f_),l(l_),dst(dst_),env(env_) { assert(f!=l); }

    void operator()(boost::function<disc_t(list_t)>& fun) const {
      if (boost::next(f)==l)
      

#define ENVIRONMENT_vtree_decl(z,n,u) vtree v ## n (0);
#define ENVIRONMENT_subtree_eval(z,n,u) _env.eval(*f++,v ## n);
#define ENVIRONMENT_operator(z,n,u)                                \
    template<typename Output,BOOST_PP_ENUM_PARAMS(n,typename Input)>    \
    void operator()                                                     \
    (const boost::function<                                             \
     Output(BOOST_PP_ENUM_PARAMS(n,typename Input)>& fun) {             \
      BOOST_PP_REPEAT(n,ENVIRONMENT_vtree_decl,~);                      \
      BOOST_PP_REPEAT(n,ENVIRONMENT_subtree_eval,~);                    \
      _dst.root()=f(BOOST_PP_ENUM_PARAMS(n,vertex_cast<Input>(v));
    }
    template<typename Output,typename Input1,typename Input2>
    void operator()(const boost::function<Output(Input1,Input2)>& fun) {
      _env.eval
      vtree v
      /*_env.eval(*_f,_dst)*boost::next(_f)
        _dst.root()=f(_env.eval_to<Input>(*_f));*/
    }

    vchild_sub_it f,l;
    vsubtree dst;
    environment& env;
  };

#endif
} //~namespace lang

#endif  // PLAP_LANG_FUNCTION_H__
