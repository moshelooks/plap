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
#include "pretty_print.h"

namespace plap { namespace lang {

void lang_do::eval(context& c,any_list l,subvtree dst) const {
  foreach(any a,l) {
    dst.prune();
    c.eval(a,dst);
  }
}

//three cases for f: f takes a single arg (eval args and pass it), f takes
//multiple args (pass args one-by-one, f is variadic (behaves like a single
//arg, but make sure its a list)
//we must also consider the possibility that f is a closure
void lang_apply::eval(context& c,any f,any_list args,subvtree dst) const {
  vtree func=vtree(vertex());
  c.eval(f,func);
  
  if (func.childless()) {
    if (args.src.childless()) {
      assert(args.src.root()==nil());
      dst=func;
    } else {
      assert(call_cast(args.src.root())==lang_list::instance() ||
             call_cast(args.src.root())==lang_tuple::instance());
      (*arg_cast<func_t>(func.root()))(c,args.src,dst);
    }
  } else { //closure
    assert(func.arity()==1);
    assert(!args.empty());

    c.scalar_bind(call_cast(func.root())->offset(),args.begin(),args.end());
    c.eval(func,dst);
    c.scalar_unbind(args.size());
  }
}

void lang_accumulate::eval(context& c,
                          any ft,any_list l,any a,subvtree dst) const {
  vtree tmp=util::tree_of(call(lang_apply::instance()))
      (vertex(),util::tree_of(call(lang_list::instance()))
       (vertex(),vertex()));
  tmp[0]=ft;
  c.eval(a,dst);
  foreach(const_subvtree s,l) {
    tmp[1][0]=s;
    assert(!tmp[1][1].empty());
    assert(!dst.empty());
    std::swap(tmp[1][1],dst);
    std::swap(tmp[1][0],tmp[1][1]);
    c.eval(tmp,dst);
  }
}

void lang_assert::operator()(context& c,const_subvtree s,subvtree dst) const {
  using boost::lexical_cast;
  using std::string;
  foreach (const_subvtree a,sub_children(s)) {
    if (a.root()==call(lang_equal::instance())) {
      vtree tmp=vtree(vertex());
      dst=tmp;
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
