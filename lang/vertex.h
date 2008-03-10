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

#ifndef PLAP_LANG_VERTEX_H__
#define PLAP_LANG_VERTEX_H__

#include "func_base.h"

namespace plap { namespace lang {

typedef const func_base* func_t;
typedef float            number_t;

struct vertex {
  vertex() {  //junk
#ifndef NDEBUG
    v.f=NULL;
#endif
  }
  friend vertex call(func_t); //factories
  friend vertex arg(func_t);
  friend vertex arg(id_t);
  friend vertex arg(number_t);

  friend func_t call_cast(vertex v); //accessors
  template<typename T>
  friend T arg_cast(vertex v);
 protected:
  vertex(func_t f_)   { v.f=f_; }
  vertex(id_t d_)   { v.d=d_; }
  vertex(number_t n_) { v.n=n_; }
  union { func_t f; id_t d; number_t n;  } v;
  
  static const id_t funcarg_mask;
  static const id_t symbolarg_mask;
};

inline vertex call(func_t f) { return vertex(f); }
inline vertex arg(func_t f) { return vertex(f->id() | vertex::funcarg_mask); }
inline vertex arg(id_t d)   { return vertex(d | vertex::symbolarg_mask); }
inline vertex arg(number_t n) { return vertex(n); }

inline func_t call_cast(vertex v) { 
  assert(v.v.f);
  return v.v.f;
}

template<>
inline func_t arg_cast<func_t>(vertex v) { 
  assert((v.v.d & vertex::funcarg_mask)==vertex::funcarg_mask);
  return id2func(v.v.d ^ vertex::funcarg_mask);
}
template<>
inline id_t arg_cast<id_t>(vertex v) {
  assert(v.v.d & vertex::symbolarg_mask);
  return (v.v.d ^ vertex::symbolarg_mask);
}
template<>
inline number_t arg_cast<number_t>(vertex v) {
  assert(!(v.v.d & vertex::funcarg_mask) && !(v.v.d & vertex::symbolarg_mask));
  return v.v.n;
}

template<>
inline bool arg_cast<bool>(vertex v) { 
  assert(arg_cast<id_t>(v)==0 || arg_cast<id_t>(v)==LANG_ARG_MAX+1);
  return arg_cast<id_t>(v);
}
template<>
inline char arg_cast<char>(vertex v) {
  assert(arg_cast<id_t>(v)>LANG_ARG_MAX+1 &&
         arg_cast<id_t>(v)<LANG_ARG_MAX+258);
  return arg_cast<id_t>(v)-LANG_ARG_MAX-129;
}

}} //namespace plap::lang
#endif //PLAP_LANG_VERTEX_H__
