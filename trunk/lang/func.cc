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

#include "func.h"
#include <boost/bind.hpp>
#include "iterator_shorthands.h"
#include "algorithm.h"
#include "context.h"

namespace plap { namespace lang {

void func::operator()(context& c,const_subvtree loc,subvtree dst) const {
  assert(loc.arity()==_arity);
  context::bind_seq b=context::bind_seq(_arity,vtree(vertex()));
  util::for_each(loc.begin_sub_child(),loc.end_sub_child(),b.begin(),
                 boost::bind(&context::eval,&c,_1,_2));
  c.push_bindings(b);
  c.eval(_body,dst);
  c.pop_bindings();
}

}} //namespace plap::lang
