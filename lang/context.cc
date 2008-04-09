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
using namespace std;

namespace plap { namespace lang {

lang_ident* context::declare(arity_t a,arity_t o) {
  _decls.push_back(new lang_ident(a,o));
  return &_decls.back();
}
//this splices out body - so if you want to keep it, make a copy
void context::define(lang_ident* d,subvtree body) {
  d->set_body(*this,body);
}
void context::erase_last_decl() { _decls.pop_back(); }

void context::eval(const_subvtree src,subvtree dst) {
  std::cout << "evaling " << src << std::endl;
  vertex v=src.root();
  if (src.childless()) {
    arity_t a=test_lang_arg_cast(v);
    if (a!=variadic_arity) { //argument
      a-=_scalars.front().second;
    
      assert(a<_scalars.front().first.size());
      assert(!_scalars.front().first[a].empty());
      
      dst=_scalars.front().first[a];
    } else if (const lang_ident* ident=test_ident_arg_cast(v)) { //ident
      ident->eval_leaf(*this,dst);
    } else {  //number/char/symbol/builtin
      dst.root()=v;
    }
  } else {
    (*call_cast(v))(*this,src,dst);
  }
  std::cout << "evaled to " << dst << std::endl;
  /*  if (!dst.childless() && call_cast(dst.root())!=lang_list::instance() &&
      call_cast(dst.root())!=lang_tuple::instance()) {
    for (vtree tmp=vtree(vertex());tmp!=dst;std::swap(tmp,dst))
      eval(dst,tmp);
      }*/
}

const_subvtree context::scalar(arity_t idx) const {
  assert(!_scalars.empty());
  assert(_scalars.front().first.size()+_scalars.front().second>idx);
  assert(idx>=_scalars.front().second);

  return _scalars.front().first[idx-_scalars.front().second];
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
