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
  assert(s.arity()==_arity || s.childless());
  assert(!_closure);
  if (_arity==0) {
    d=c.ident_binding(this);
  } else {
    c.scalar_bind(_offset,s.begin_sub_child(),s.end_sub_child());
    c.eval(c.ident_binding(this),d);
    c.scalar_unbind(_arity);
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
  std::cout << "doing " << s << std::endl; 
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
  std::cout << "done " << d << std::endl;  
}

void lang_closure::rec_instantiate(context& c,subvtree d,bool& ready) const {
  arity_t _offset=0;//fixme
  foreach (subvtree s,sub_leaves(d)) {
    arity_t a=test_lang_arg_cast(s.root());
    if (a<c.scalar_arity()+_offset) {
      assert(a>=_offset);
      std::cout << "PPP" << std::endl;
      s=c.scalar(a-_offset);
      std::cout << "ooo" << std::endl;
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
