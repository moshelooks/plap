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

#include "context.h"
#include "core.h"

namespace plap { namespace lang {

//we must check for two special cases: (1) evaluation of an argument; and
//(2) evaluation of a closure
 void eval(const_subvtree src,subvtree dst) {
   if (src.childless()) {
     arity_t a=test_lang_arg_cast(src.root());
     if (a!=variadic_arity) { //case 1
       assert(a<bindings().size());
       assert(!bindings()[a].empty());
       dst=bindings()[a];
     } else if (func_t f=test_func_arg_cast(src.root())) { //case 2
       ident_bindings::const_iterator i=_idents.find(f);
       if (i!=idents.end())
         dst=i->second;
     }
   } else {
     (*call_cast(src.root()))(*this,src,dst);
   }
 }

lang_def* context::declare(arity_t a,arity_t o) {
  _defs.push_back(new lang_def(a,o));
  return &_defs.back();
}
//this splices out body - so if you want to keep it, make a copy
void context::define(subvtree body,lang_def* d) { d->set_body(body); }

void context::bind_def(func_t f) {
  let_map::iterator i=_lets.find(f);
  if (i==_lets.end())
    i=_lets.insert(make_pair(f,util::slist<vtree>(1,vtree(vertex())))).first;
  else
    i->second.push_front(vtree(vertex()));
  this->eval(f->body(),

}} //namespace plap::lang
