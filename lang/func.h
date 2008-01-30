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

#include <boost/preprocessor.hpp>
#include <boost/utility/result_of.hpp>
#include "environment.h"
#include "iterator_shorthands.h"


#ifndef LANG_FUNC_LIMIT_ARITY
#  define LANG_FUNC_LIMIT_ARITY 4
#endif
#define LANG_FUNC_LIMIT_ARITY_INC BOOST_PP_INC(LANG_FUNC_LIMIT_ARITY)

namespace lang {

struct func_def {
  virtual ~func_def() {}
  virtual void operator()(const_vsubtree loc,vsubtree dst,
                          environment& env) const=0;
};

template<typename T>
struct list_of {};

namespace lang_private {
//define eager function wrappers up to limit arity
#define LANG_FUNC_type_params(n) \
  typename Base,BOOST_PP_ENUM_PARAMS(n,typename Input)
#define LANG_FUNC_params(n) \
  Base,BOOST_PP_ENUM_PARAMS(n,Input)
#define LANG_FUNC_vtree_decl(z,n,u) vtree tr ## n(0);
#define LANG_FUNC_vtree_eval(z,n,u) \
  env.eval(child++,tr ## n); assert(tr ## n.childless());
#define LANG_FUNC_call_arg(z,n,u) vertex_cast<Input ## n>(tr ## n.root())
#define LANG_FUNC_eager_def(z,n,u)                                      \
  template<LANG_FUNC_type_params(n)>                                    \
  struct eager_func_def ## n {                                          \
    eager_func_def ## n(const Base& fun) : base(fun) {}                 \
    void operator()(const_vsubtree loc,vsubtree dst,                    \
                    environment& env) const {                           \
      BOOST_PP_REPEAT(n,LANG_FUNC_vtree_decl,~);                        \
      const_vsub_child_it child=loc.begin_sub_child();                  \
      env.eval(child,tr0);                                              \
      assert(tr0.childless());                                          \
      BOOST_PP_REPEAT_FROM_TO(1,n,LANG_FUNC_vtree_eval,~);              \
      dst.root()=base(BOOST_PP_ENUM(n,LANG_FUNC_call_arg,~));           \
    }                                                                   \
    Base base;                                                          \
  };
BOOST_PP_REPEAT_FROM_TO(1,LANG_FUNC_LIMIT_ARITY_INC,LANG_FUNC_eager_def,~);

#define LANG_FUNC_eager_maker(z,n,u)                                    \
  template<LANG_FUNC_type_params(n)>                                    \
  struct eager_maker ## n {                                             \
    eager_maker ## n(const Base& b) {                                   \
      def=new eager_func_def ## n<LANG_FUNC_params(n)>(b);              \
    }                                                                   \
    func_def* def;                                                      \
  };
BOOST_PP_REPEAT_FROM_TO(1,LANG_FUNC_LIMIT_ARITY_INC,LANG_FUNC_eager_maker,~);

template<typename T>
struct nested_list_of { };
template<typename T>
struct nested_list_of<list_of<T> > { typedef T type; };

//helper macros for LANG_FUNC_list_overload
#define LANG_FUNC_list_emit_if0(z,u,i,elem)                             \
  BOOST_PP_IF(elem,(list_of<Input ## i>),(Input ## i))
#define LANG_FUNC_list_emit_if1(z,u,i,elem)                             \
  BOOST_PP_IF                                                           \
  (elem,                                                                \
   (typename util::slist<Input ## i>::const_iterator)                   \
   (typename util::slist<Input ## i>::const_iterator),                  \
   (Input ## i))
#define LANG_FUNC_list_emit_if2(z,u,i,elem)                             \
  BOOST_PP_IF                                                           \
  (elem,                                                                \
   (list_t input ## i),                                                 \
   (Input ## i input ## i))
#define LANG_FUNC_list_emit_if3(z,u,i,elem)                             \
  BOOST_PP_IF                                                           \
  (elem,                                                                \
   (util::transform_it(input ## i->begin(),&vertex_cast<                \
                       typename nested_list_of<Input ## i>::type>))     \
   (util::transform_it(input ## i->end(),&vertex_cast<                  \
                       typename nested_list_of<Input ## i>::type>)),    \
   (input ## i))
#define LANG_FUNC_or(s,p,q) BOOST_PP_OR(p,q)
#define LANG_FUNC_not_all_zeros(seq) \
  BOOST_PP_SEQ_FOLD_LEFT(LANG_FUNC_or,0,seq)
#define LANG_FUNC_list_specialization(naz,seq)                          \
  BOOST_PP_IF(naz,<Base,)                                               \
  BOOST_PP_COMMA_IF(naz)                                                \
  BOOST_PP_SEQ_ENUM                                                     \
  (BOOST_PP_IF(naz,BOOST_PP_SEQ_FOR_EACH_I                              \
               (LANG_FUNC_list_emit_if0,~,seq),()))                     \
  BOOST_PP_IF(naz,>,)

//this is to be called on all binary seqs on length n
#define LANG_FUNC_list_overload(z,seq)                                  \
  template<LANG_FUNC_type_params(BOOST_PP_SEQ_SIZE(seq))>               \
  struct BOOST_PP_CAT(list_adapter,BOOST_PP_SEQ_SIZE(seq))              \
  LANG_FUNC_list_specialization(LANG_FUNC_not_all_zeros(seq),seq)       \
  {                                                                     \
    typename boost::result_of<Base(                                     \
        BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_FOR_EACH_I(                      \
                              LANG_FUNC_list_emit_if1,~,seq)))>         \
    operator()(BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_FOR_EACH_I(               \
                                     LANG_FUNC_list_emit_if2,~,seq))) { \
      return base(                                                      \
          BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_FOR_EACH_I(                    \
                                LANG_FUNC_list_emit_if3,~,seq)));       \
    }                                                                   \
    Base base;                                                          \
  };

//helper macros for LANG_FUNC_list_adapter
#define LANG_FUNC_identity(z,n,u) (u)
#define LANG_FUNC_repeat_seq(v,n) BOOST_PP_REPEAT(n,LANG_FUNC_identity,v)
#define LANG_FUNC_exp(n)                                               \
  BOOST_PP_SEQ_FOR_EACH_PRODUCT(                                       \
      LANG_FUNC_list_overload,LANG_FUNC_repeat_seq((1)(0),n))

//the emits the prototype, followed by 2^n overloads
#define LANG_FUNC_list_adapter(z,n,u)                           \
  template<LANG_FUNC_type_params(n)>                            \
  struct list_adapter ## n;                                     \
  LANG_FUNC_exp(n)
BOOST_PP_REPEAT_FROM_TO(1,LANG_FUNC_LIMIT_ARITY_INC,
                        LANG_FUNC_list_adapter,~);

} //~namespace lang_private

#define LANG_FUNC_make_eager(z,n,u)                                       \
  template<LANG_FUNC_type_params(n)>                                      \
  func_def* make_eager(const Base& base,BOOST_PP_ENUM_PARAMS(n,Input)) {  \
    return lang_private::eager_maker ## n<                                \
    lang_private::list_adapter ## n<LANG_FUNC_params(n)>,                 \
        BOOST_PP_ENUM_PARAMS(n,Input)>                                    \
        (lang_private::list_adapter ## n<LANG_FUNC_params(n)>(base)).def; \
  }
BOOST_PP_REPEAT_FROM_TO(1,LANG_FUNC_LIMIT_ARITY_INC,LANG_FUNC_make_eager,~);

//clean up our mess
#undef LANG_FUNC_eager_maker
#undef LANG_FUNC_param_basis
#undef LANG_FUNC_eager_maker
#undef LANG_FUNC_make_overload
#undef LANG_FUNC_make_eager
#undef LANG_FUNC_list_adapter
#undef LANG_FUNC_exp
#undef LANG_FUNC_repeat_seq
#undef LANG_FUNC_identity
#undef LANG_FUNC_list_overload
#undef LANG_FUNC_list_specialization
#undef LANG_FUNC_not_all_zeros
#undef LANG_FUNC_and
#undef LANG_FUNC_list_emit_if3
#undef LANG_FUNC_list_emit_if2
#undef LANG_FUNC_list_emit_if1
#undef LANG_FUNC_list_emit_if0
#undef LANG_FUNC_eager_maker
#undef LANG_FUNC_eager_def
#undef LANG_FUNC_call_arg
#undef LANG_FUNC_vtree_eval
#undef LANG_FUNC_vtree_decl
#undef LANG_FUNC_params
#undef LANG_FUNC_type_params
#undef LANG_FUNC_LIMIT_INC
#undef LANG_FUNC_LIMIT_ARITY

#if 0

use filter for typename list

needs to be parameterized by n and then exponetiated

#define LANG_FUNC_make_overload

template<typename Base,typename Input0,typename Input1>
struct list_adapter2<Base,Input0,list_of<Input1> > {
  list_adapter2(const Base& b) : base(b) {}
  
  typename boost::result_of<Base(Input0,
                                 vlist::const_iterator,
                                 vlist::const_iterator)>::type
  operator()(Input0 i0,list_t i1) const {
    return base(i0,
                util::transform_it(i1->begin(),&vertex_cast<
                                   typename nested_list_of<Input1>::type>),
                util::transform_it(i1->end(),&vertex_cast<
                                   typename nested_list_of<Input1>::type>));
  }
  Base base;
};

/////////
template<typename Base,typename Input0,typename Input1>       
struct list_adapter2;

template<typename Base,typename Input0,typename Input1>
struct list_adapter2<Base,Input0,list_of<Input1> > {
  list_adapter2(const Base& b) : base(b) {}
  
  typename boost::result_of<Base(Input0,
                                 vlist::const_iterator,
                                 vlist::const_iterator)>::type
  operator()(Input0 i0,list_t i1) const {
    return base(i0,
                util::transform_it(i1->begin(),&vertex_cast<
                                   typename nested_list_of<Input1>::type>),
                util::transform_it(i1->end(),&vertex_cast<
                                   typename nested_list_of<Input1>::type>));
  }
  Base base;
};
///////////////////

#define LANG_FUNC_eager_maker \
template<LANG_FUNC_params(n)>                                         \
  struct eager_maker {                                                  \
    eager_maker(const Base& b) {                                        \
      def=new eager_func_def ## n<LANG_FUNC_params(n)>(b);              \
    }                                                                   \
    func_def* def;                                                      \
  };

//define convenience (heap-based) creators for eager functions
//we need an 2^limit_arity overloads to handle lists properly

#define LANG_FUNC_param_basis(n)                             \
  BOOST_PP_LIST_CONS(Input ## n,                             \
                     BOOST_PP_LIST_CONS(list_of<ListOf ## n, \
                                        BOOST_PP_NIL))




#define LANG_FUNC_eager_maker(z,n,u)                               \
  BOOST_PP_LIST_FOR_EACH(LANG_FUNC_eager_maker,~,LANG_FUNC_enum(n))

     \
  



template<typename Base,typename Input0,typename Input1>
struct list_adapter {
  list_adapter(Base& b) : base(sub) {}
  
  const vertex& operator(const Input0& i0,const Input1& i1) { 
    return b(i0,LIST_ADAPT(Input0,
  }

  list_adapter<list_adapter<Base,Input1> > sub;
};

template<typename Base,typename Input0,typename Input1>
struct list_adapter {
  list_adapter(Base& b) : base(b) {}
  
  const vertex& operator(const Input0& i0,const Input1& i1) { 
    return b(i0,i1);
  }

  Base b;
};
  



template<typename ListOf,typename Base>
struct eager_maker<list_of<ListOf>,Base> : public func_def {
  eager_maker(const Base& b) : { 
    def=new list_adapter<0>(b);
  }
  eager_maker(const Base& b,bool) : base(b) {}
  func_def* def;
};

template<typename Result,typename Input0,typename Base>
func_def* make_eager(const Base& base) {
  return lang_private::eager_maker<Result,Input0,Base>(base).def;
}

template<typename Base,typename Result,typename OfType>
func_def* make_eager<list_ty(Base&);










template<typename Input0,typename Input1,typename Base>
struct eager_func_def2 : public func_def {
  eager_func_def2(const Base& fun) : base(fun) {}

  void operator()(const_vsubtree loc,vsubtree dst,env) const {
    vtree tr0(0);
    vtree tr1(0);

    const_vchild_sub_it child=loc.begin_child_sub();
    _env.eval(child,tr0);
    assert(tr0.childless());
    _env.eval(++child,tr1);
    assert(tr1.childless());
    
    dst.root()=base(vertex_cast<Input0>(tr0.root()),
                    vertex_cast<Input1>(tr1.root()));
  }

  Base base;
};


template<int Index,typename Base,typename OfType>
typename boost::result_of<Base(

struct unary_list_dispatcher : public boost::function<list_t(disc_t)> {
  disc_t operator()(

struct func_def {
  

  template<typename Func>
  func_def(const Func& fun) : _base(fun) {}

  
typedef boost::
/*
foo(list_t* x) {
  return base(transform_it(x->begin(),&vertex_cast<disc_t>),
       transform_it(x->end(),&vertex_cast<disc_t>),T(0));
}

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
*/

namespace lang_private { struct lang_void {}; }

template<typename Func,typename SubType0,
         typename SubType1 = lang_private::lang_void,

           >
struct list_adapter {
  list_adapter(const Func& f) : base(f) {}

  template<typename Input0,typename Input1>
  typename boost::result_of<Func(Input0,Input1)>::type
  operator()(list_t l,Input0 i0,Input1 i1) const {
    return base(l->begin(),l->end(),i0,i1);
  }

  Func base;

  how to handle +(1 2) as a list? can we assume that this is done at parse 
      time? what about pretty-printing
  
  


te
  




  void operator(const_vsubtree arg,vsubtree dst) {
    
    

template<typename Input,typename Output>
struct unary_def {
  
  void operator()(const unary_func& f) {
    f(_f,dst);
  }

struct func {
  void call(const_vchild_sub_it f,const_vchild_sub_it l,vsubtree dst) {
    boost::apply_visitor(arity_dispatch(f,l,dst),this);
};

struct func_def {

  void operator()(
 protected:
  boost::any _f;
}

template<typename T1>
//function& register_function(disc_t boost::function<

function lambda_function(const_vsubtree);


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
