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

//identifiers, when bound to lambdas, inject scalar bindings
void lang_ident::operator()(context& c,const_subvtree s,subvtree d) const {
  if (_arity==0) {
    d=c.ident_binding(this);
  } else {
    assert(s.arity()==_arity);
    c.scalar_bind(_offset,s.begin_sub_child(),s.end_sub_child());
    c.eval(c.ident_binding(this),d);
    c.scalar_unbind(_arity);
  }
}

void lang_ident::closure(context& c,subvtree d) {
  d=_body;
  foreach(subvtree s,sub_leaves(d)) {
    arity_t a=test_lang_arg_cast(s.root());
    if (a<_offset)
      s=c.scalar(a);
    else if (a<_offset+_arity)
      s.root()=arg(a-_offset);
    else
      assert(a==variadic_arity);
  }
}

}} //namespace plap::lang
