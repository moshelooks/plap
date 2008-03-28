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
#include "foreach.h"
#include "names.h"
#include "checkpoint.h"

#include "pretty_print.h" //fixme

namespace plap { namespace lang {

lang_ident* context::declare(arity_t a,arity_t o) {
  _decls.push_back(new lang_ident(a,o));
  return &_decls.back();
}
//this splices out body - so if you want to keep it, make a copy
void context::define(lang_ident* d,subvtree body) { d->set_body(*this,body); }
void context::erase_last_decl() { _decls.pop_back(); }

//we must check for some special cases: (1) evaluation of an argument;
//(2) evaluation of a closure; and (3) a def that's not a function
void context::eval(const_subvtree src,subvtree dst) {
  checkpoint();
  std::cout << src << std::endl;

  vertex v=src.root();
  if (src.childless()) {
    arity_t a=test_lang_arg_cast(v);
    if (a!=variadic_arity) { //case 1
      a-=_scalars.front().second;
      assert(a<_scalars.front().first.size());
      assert(!_scalars.front().first[a].empty());
      dst=_scalars.front().first[a];
    } else { 
      if (func_t f=test_func_arg_cast(v)) { //case 2
        /**ident_map::const_iterator i=_idents.find(f);
        if (i!=_idents.end()) {
          eval(i->second.front(),dst);
          return;**/
        checkpoint();
        if (f->closure()) {
          (*f)(*this,src,dst);
          return;
        } else if (f->arity()==0) { //case 3
          checkpoint();
          assert(f->body());
          assert(!f->body()->empty());
          dst=*f->body();
          return;
        }
      }
      dst.root()=v;
    }
  } else {
    (*call_cast(v))(*this,src,dst);
  }
}

void context::ident_bind(func_t f,const_subvtree binding) {
  ident_map::iterator i=_idents.find(f);
  if (i==_idents.end())
    i=_idents.insert(make_pair(f,vtree_list(1,vtree(vertex())))).first;
  else
    i->second.push_front(vtree(vertex()));
  this->eval(binding,i->second.front());
}

void context::ident_unbind(func_t f) {
  assert(_idents.find(f)!=_idents.end());
  _idents.find(f)->second.pop_front();
}

}} //namespace plap::lang
