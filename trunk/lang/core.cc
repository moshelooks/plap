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
#include "foreach.h"
#include "context.h"

namespace plap { namespace lang {

void lang_ident::operator()(context& c,const_subvtree s,subvtree d) const {
  //  assert(s.arity()==_arity || s.childless() || _closure);fixme
  if (_closure) {
    assert(s.arity()==1);
    d=s.front_sub();
    instantiate_closure(c,d);
  } else {
    assert(s.arity()==arity());
    c.scalar_bind(_offset,s.begin_sub_child(),s.end_sub_child());
    c.eval(c.ident_binding(this),d);
    c.scalar_unbind(_arity);
  }
}

void lang_ident::eval_leaf(context& c,subvtree d,bool expand_bindings) const {
  if (_closure) {
    if (arity()==0) {
      vtree tmp=_body;
      instantiate_closure(c,tmp);
      c.eval(tmp,d);
    } else {          
      d.root()=call(this);
      d.append(_body);
      instantiate_closure(c,d.front_sub());
    }
  } else if (expand_bindings && arity()==0) {
    d=c.ident_binding(this);
  } else {
    d=arg(this);
  }
}

void lang_ident::set_body(context& c,subvtree b) {  
  assert(_body.empty());
  _body.splice(_body.end(),b); 
  _closure=has_var_outside_range(_body);
  if (!_closure && _arity==0) { //evaluate it now
    vtree tmp=vtree(vertex());
    c.eval(_body,tmp);
    std::swap(_body,tmp);
  }
  std::cout << "set body #" << id() << " _closure=" << _closure << " "
            << _body << std::endl;
}

bool lang_ident::has_var_outside_range(const_subvtree s) const {
  foreach (vertex v,leaves(s)) {
    arity_t a=test_lang_arg_cast(v);
    if (a!=variadic_arity) {
      if (a<_offset)
        return true;
    } else if (const lang_ident* ident=test_ident_arg_cast(v)) {
      if (ident->_closure && has_var_outside_range(*ident->body()))
          return true;
    }
  }
  return false;
}

void lang_ident::instantiate_closure(context& c,subvtree d) const {
  std::cout << "instantiating " << d << std::endl;
  bool ready=true;
  rec_instantiate(c,d,ready);
  if (ready) {
    vtree tmp=vtree(vertex());
    c.eval(d,tmp);
    std::swap(d,tmp);
  }
  std::cout << "instantiated " << d << std::endl;
}

void lang_ident::rec_instantiate(context& c,subvtree d,bool& ready) const {
  //arity_t _offset=0;//fixme
  foreach (subvtree s,sub_leaves(d)) {
    arity_t a=test_lang_arg_cast(s.root());
    if (a<c.scalar_arity()+c.scalar_offset()) {
      std::cout << "XX" << (int)a << ">=?" << (int)_offset << std::endl;
      assert(a>=c.scalar_offset());
      std::cout << "PPP" << std::endl;
      s=c.scalar(a);//-c.scalar_offset());
      std::cout << "ooo" << std::endl;
    } else if (a!=variadic_arity) {
      //s.root()=lang_arg(a-c.scalar_arity());
      ready=false;
    } else if (const lang_ident* ident=test_ident_arg_cast(s.root())) {
      //  ident->eval_leaf(c,s,false);
      if (ident->_closure) {
        s.root()=call(ident);
        s.append(*ident->body());
        rec_instantiate(c,s.front_sub(),ready);
      }
    }
  }
}

}} //namespace plap::lang
