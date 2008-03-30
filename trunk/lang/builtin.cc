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

#include "builtin.h"
#include <boost/lexical_cast.hpp>
#include <iostream>
#include "foreach.h"
#include "checkpoint.h"
#include "pretty_print.h"

namespace plap { namespace lang {

//let injects identifier bindings into the context
/**
void lang_let::operator()(context& c,const_subvtree s,subvtree d) const { 
  assert(s.arity()==2);
  assert(s.front()==call(lang_list::instance()));
  foreach (vertex v,children(s.front_sub())) {
    func_t f=arg_cast<func_t>(v);
    assert(f->body());
    c.ident_bind(f,*f->body());
  }
  c.eval(s.back_sub(),d);
  foreach (vertex v,children(s.front_sub()))
    c.ident_unbind(arg_cast<func_t>(v));
    }**/

//closure(f,list(args))
/**
void lang_closure::operator()(context& c,const_subvtree s,subvtree d) const {
  assert(s.arity()==2);
  assert(s[0].childless());
  assert(arg_cast<func_t>(s.front())->body());
  assert(s[1].flat());

  func_t f=arg_cast<func_t>(s.front())->body();
  d=*arg_cast<func_t>(s.front())->body();
  foreach (vertex v,leaves(s)) {
    if (
}
**/
  
void lang_do::eval(context& c,any_list l,subvtree dst) const {
  foreach(any a,l) {
    dst.prune();
    c.eval(a,dst);
  }
}

void lang_apply::eval(context& c,any f,any_list args,subvtree dst) const {
  vtree tmp=vtree(vertex());
  checkpoint();
  c.eval(f,tmp);
  checkpoint();
  if (!tmp.childless() && call_cast(tmp.root())==lang_closure::instance()) {
    checkpoint();
    assert(tmp.arity()==1);
    c.scalar_bind(0,args.begin(),args.end());
    c.eval(tmp.front_sub(),dst);
    c.scalar_unbind(args.size());
    checkpoint();
  } else {
    assert(tmp.childless());
    checkpoint();
    (*arg_cast<func_t>(tmp.root()))(c,args.src,dst);
    checkpoint();
  }
}

void lang_accumulate::eval(context& c,
                          any ft,any_list l,any a,subvtree dst) const {
  func_t f=c.eval_to<func_t>(ft);
  vtree tmp=util::tree_of(vertex())(vertex(),vertex());
  c.eval(a,dst);
  foreach(const_subvtree s,l) {
    tmp[0]=s;
    assert(!tmp[1].empty());
    assert(!dst.empty());
    std::swap(tmp[1],dst);
    std::swap(tmp[0],tmp[1]);
    (*f)(c,tmp,dst);
  }
}

void lang_assert::eval(context& c,any a,subvtree dst) const {
  using boost::lexical_cast;
  using std::string;
  if (a.root()==call(lang_equal::instance())) {
    vtree tmp=vtree(vertex());
    c.eval(a[0],tmp);
    c.eval(a[1],dst);
    if (tmp!=dst)
      throw std::runtime_error("Failed equality assertion: "+
                               lexical_cast<string>(tmp)+" doesn't equal "+
                               lexical_cast<string>(dst)+".");
  } else {
    c.eval(a,dst);
    if (!dst.childless() || !arg_cast<bool>(dst.root()))
      throw std::runtime_error("Failed assertion: "+
                               lexical_cast<string>(a)+".");
  }
  dst.prune();
  dst.root()=nil();
}

std::ostream* lang_print::print_to=&std::cout;
void lang_print::eval(context& c,any a,subvtree dst) const {
  c.eval(a,dst);
  if (dst.flat() && dst.root()==call(lang_list::instance()) &&
      is_char(dst.front()))
    foreach(char c,list_of<char>(dst))
      (*print_to) << c;
  else
    (*print_to) << dst;
  dst.prune();
  dst.root()=nil();
}
void  lang_println::eval(context& c,any a,subvtree dst) const {
  lang_print::instance()->eval(c,a,dst);
  (*lang_print::print_to) << std::endl;
}

}} //namespace plap::lang
