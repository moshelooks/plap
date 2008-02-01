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

#ifndef PLAP_LANG_CORE_H__
#define PLAP_LANG_CORE_H__

#include "algorithm.h"
#include "cast.h"
#include "def.h"

namespace lang {

template<typename T>
struct eval : public def {
  void operator()(const_vsubtree s,vsubtree d) const {
    assert(d.childless());
    if (s.childless())
      d.root()=s.root();
    else
    (*vertex_cast<def_t>(s.root()))(s,d);
  }
};

template<typename T>
struct cons : public def {
  void operator()(const_vsubtree s,vsubtree d) const {
    assert(d.childless());
    d.root()=def_t(this);
    d.append(d.begin(),s.arity(),vertex());
    util::for_each(s.begin_sub_child(),s.end_sub_child(),
                   d.begin_sub_child(),eval<T>());
  }
};

} //~namespace lang

#endif  // PLAP_LANG_CORE_H__
