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
  if (_closure) {
    assert(s.childless());
    checkpoint();
    d=c.ident_binding(this);
    checkpoint();
    //foreach(subvtree s,d) { //fixme leaves
    for (subvtree::sub_pre_iterator i=d.begin_sub();i!=d.end_sub();++i) {
      checkpoint();
      subvtree s=*i;
      if (!s.childless())
        continue;

      arity_t a=test_lang_arg_cast(s.root());
      if (a<_offset) {
        checkpoint();
        s=c.scalar(a);
        checkpoint();
      }
      else if (a<_offset+_arity)
        s.root()=lang_arg(a-_offset);
      else
        assert(a==variadic_arity);
      checkpoint();
    }
    checkpoint();
    d.insert_above(d.begin(),call(lang_closure::instance()));
  } else {
    std::cout << s.arity() << " " << int(_arity) << std::endl;
    assert(s.arity()==_arity);
    if (_arity==0) {
      d=c.ident_binding(this);
    } else {
      c.scalar_bind(_offset,s.begin_sub_child(),s.end_sub_child());
      c.eval(c.ident_binding(this),d);
      c.scalar_unbind(_arity);
    }
  }
}

void lang_ident::set_body(context& c,subvtree b) {  
  assert(_body.empty()); //fixeme for overriding

  _body.splice(_body.end(),b); 
  _closure=false;
  //foreach (vertex v,_body) {//fixme leaves
  for (subvtree::sub_pre_iterator i=_body.begin_sub();i!=_body.end_sub();++i) {
    vertex v=i->root();
    if (!i->childless())
        continue;
  
    arity_t a=test_lang_arg_cast(v);
    if (a!=variadic_arity && a<_offset) {
      _closure=true;
      return;
    }
    if (func_t f=test_func_arg_cast(v)) {
      if (const lang_ident* d=f->closure()) {
        if (c.owns(this,d) && d-
        _contains_closure=true;
      }
    }
  }
  if (!_closure && !_contains_closure && _arity==0) { //evaluate it now
    vtree tmp=vtree(vertex());
    c.eval(_body,tmp);
    std::swap(_body,tmp);
  }
}

}} //namespace plap::lang
