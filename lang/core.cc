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
#include "names.h"
#include <boost/lexical_cast.hpp>

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
  /** if (_closure) {
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
    } else**/ if (_arity==0) {
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
    }/* else if (a==variadic_arity) {
      if (func_t f=test_func_arg_cast(s.root())) {
        if (const lang_ident* closure=f->closure()) {
          //if (closure->nested()) {
          std::cout << "XX to " << d << std::endl;
            closure->expand_closure(c,s,m);
            //}
        }
      }
      }**/
    //checkpoint();
  }
  d.append(call(lang_closure::instance()));
  d.splice(d.back_sub().end_child(),d.begin_sub_child(),--d.end_sub_child());
  std::swap(d.root(),d.front());
  std::cout << "goes to " << d << std::endl;
}

void lang_ident::set_body(context& c,subvtree b) {  
  assert(_body.empty());
  _body.splice(_body.end(),b); 
  _closure=has_var_outside_range(_body);
  std::cout << "_closure for id#" << id() << ", name=" 
            << boost::lexical_cast<std::string>(func_t(this))
            << ", '" << _body << "' is " 
            << _closure << std::endl;
  if (!_closure && _arity==0) { //evaluate it now
    vtree tmp=vtree(vertex());
    c.eval(_body,tmp);
    std::swap(_body,tmp);
  }
}

bool lang_ident::has_var_outside_range(const_subvtree s) const {
  foreach (vertex v,leaves(s)) {
    arity_t a=test_lang_arg_cast(v);
    if (a!=variadic_arity) {
      if (a<_offset)
      return true;
    } else if (func_t f=test_func_arg_cast(v)) {
      if (const lang_ident* d=f->closure()) {
        assert(d->body());
        if (has_var_outside_range(*d->body()))
          return true;
      }
    }
  }
  return false;
}

void lang_closure::operator()(context& c,const_subvtree s,subvtree d) const {
  d=s;
  bool ready=true;
  bool rewrap=d.childless() || call_cast(d.root())!=this;
  rec_instantiate(c,d,ready);
  if (ready) {
    vtree tmp=vtree(vertex());
    c.eval(d,tmp);
    std::swap(d,tmp);
  }
  if (rewrap) {
    d.append(call(lang_closure::instance()));
    d.splice(d.back_sub().end_child(),d.begin_sub_child(),--d.end_sub_child());
    std::swap(d.root(),d.front());
  }
}

void lang_closure::rec_instantiate(context& c,subvtree d,bool& ready) const {
  foreach (subvtree s,sub_leaves(d)) {
    arity_t a=test_lang_arg_cast(s.root());
    if (a<c.scalar_arity()) {
      s=c.scalar(a);
    } else if (a!=variadic_arity) {
      s.root()=lang_arg(a-c.scalar_arity());
      ready=false;
    } else {
      if (func_t f=test_func_arg_cast(s.root())) {
        if (const lang_ident* cl=f->closure()) {
          s.root()=call(lang_closure::instance());
          s.append(*cl->body());
          rec_instantiate(c,s.front_sub(),ready);
        }
      }
    }
  }
}

}} //namespace plap::lang
