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

#include "core.h"
#include <vector>
#include <boost/bind.hpp>
#include "iterator_shorthands.h"
#include "foreach.h"
#include "algorithm.h"
#include "context.h"

#include "pretty_print.h"//fixme
#include "checkpoint.h"

namespace plap { namespace lang {

using boost::bind;
using util::for_each;

void lang_list::operator()(context& c,const_subvtree s,subvtree d) const { 
  assert(!s.childless());
  assert(d.childless());
  d.root()=call(this);
  d.append(s.arity(),vertex());
  util::for_each(s.begin_sub_child(),s.end_sub_child(),d.begin_sub_child(),
                 boost::bind(&context::eval,&c,_1,_2));
}

void lang_ident::operator()(context& c,const_subvtree s,subvtree d) const {
  //std::cout << s.arity() << " " << int(_arity) << std::endl;
  assert(s.arity()==_arity || s.childless());
  if (_closure) {
    if (!s.childless()) {
      assert(_offset==0);
      c.scalar_bind(0,s.begin_sub_child(),s.end_sub_child());
      expand_closure(c,d,_arity);
      assert(d.arity()==1);
      std::swap(d.root(),d.front());
      d.erase(d.flatten(d.begin_child()));
      c.scalar_unbind(_arity);
      
      if (!d.childless() && call_cast(d.root())!=lang_closure::instance()) {
        vtree tmp=vtree(vertex());
        c.eval(d,tmp);
        std::swap(d,tmp);
      }
    } else {
      expand_closure(c,d,0);
    }
  } else if (_arity==0) {
    d=c.ident_binding(this);
  } else {
    c.scalar_bind(_offset,s.begin_sub_child(),s.end_sub_child());
    c.eval(c.ident_binding(this),d);
    c.scalar_unbind(_arity);
  }
}

void lang_ident::expand_closure(context& c,subvtree d,arity_t m) const {
  d=c.ident_binding(this);
  std::cout << "YY to " << d << std::endl;
  //checkpoint();
  foreach(subvtree s,sub_leaves(d)) {
    arity_t a=test_lang_arg_cast(s.root());
    if (a<m) {
      //checkpoint();
      s=c.scalar(a);
      //checkpoint();
    } else if (m!=0 && a!=variadic_arity) {
      s.root()=lang_arg(a-m);
    } else if (a==variadic_arity) {
      if (func_t f=test_func_arg_cast(s.root())) {
        if (const lang_ident* closure=f->closure()) {
          //if (closure->nested()) {
          std::cout << "XX to " << d << std::endl;
            closure->expand_closure(c,s,m);
            //}
        }
      }
    }
    //checkpoint();
  }
  d.append(call(lang_closure::instance()));
  d.splice(d.back_sub().end_child(),d.begin_sub_child(),--d.end_sub_child());
  std::swap(d.root(),d.front());
  std::cout << "goes to " << d << std::endl;
}

bool lang_ident::set_body(context& c,subvtree b,
                          bool contains_closure,bool nested) {  
  assert(_body.empty()); //fixeme for overriding
  _nested=nested;

  _body.splice(_body.end(),b); 

  if (_closure=contains_closure)
    return true;

  foreach (vertex v,leaves(_body)) {
    arity_t a=test_lang_arg_cast(v);
    if (a!=variadic_arity && a<_offset) {
      _closure=true;
      return true;
    }
  }
  if (_arity==0) { //evaluate it now
    vtree tmp=vtree(vertex());
    c.eval(_body,tmp);
    std::swap(_body,tmp);
  }
  return false;
}

void lang_closure::operator()(context& c,const_subvtree s,subvtree d) const {
  assert(s.arity()==1);
  d=s;//[0];

  foreach (subvtree s,sub_leaves(d)) {
    arity_t a=test_lang_arg_cast(s.root());
    if (a<c.scalar_arity()) {
      s=c.scalar(a);
    } else if (a!=variadic_arity) {
      s.root()=lang_arg(a-c.scalar_arity());
    }
  }
}

}} //namespace plap::lang
