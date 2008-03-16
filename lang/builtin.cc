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
#include "foreach.h"
#include "checkpoint.h"

namespace plap { namespace lang {

void lang_apply::eval(context& c,any f,any_list args,subvtree dst) const {
  if (f.childless()) {
    (*arg_cast<func_t>(f.root()))(c,args.src,dst);
  } else {
    assert(false);
    //vtree tmp=vtree(vertex());
    //c.eval(f,tmp);
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

}} //namespace plap::lang
