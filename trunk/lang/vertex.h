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
  friend vertex call(func_t); //creators
  friend vertex arg(func_t);
  friend vertex arg(id_t);
  friend vertex arg(number_t);

  friend vertex arg(bool);
  friend vertex nil();
  friend vertex lang_arg(arity_t);
  friend vertex arg(char);

  friend func_t call_cast(vertex); //accessors
  template<typename T>
  friend T arg_cast(vertex);
  friend bool is_lang_arg(id_t);
  friend arity_t lang_arg_cast(id_t);

  //important: these don't work on calls, only args
  friend bool is_func(vertex);
  friend bool is_symbol(vertex);
  friend bool is_number(vertex);
 protected:
  vertex(func_t f_)   { v.f=f_; }
  vertex(id_t d_)   { v.d=d_; }
  vertex(number_t n_) { v.n=n_; }
  union { func_t f; id_t d; number_t n;  } v;
  
  static const id_t funcarg_mask;
  static const id_t symbolarg_mask;

  static const id_t false_id;
  static const id_t true_id;
  static const id_t nil_id;

  static const id_t smallest_lang_arg;
  static const id_t largest_lang_arg;

  static const id_t smallest_char;
  static const id_t largest_char;
  static const id_t char_offset;
};

inline vertex call(func_t f) { return vertex(f); }
inline vertex arg(func_t f) { return vertex(f->id() | vertex::funcarg_mask); }
inline vertex arg(id_t d)   { return vertex(d | vertex::symbolarg_mask); }
inline vertex arg(number_t n) { return vertex(n); }

inline vertex arg(bool b) { 
  return arg(b ? vertex::true_id : vertex::false_id);
}
inline vertex nil() { return arg(vertex::nil_id); }
inline vertex lang_arg(arity_t a) { 
  assert(a<LANG_ARG_MAX);
  return arg(a+vertex::smallest_lang_arg);
}
inline vertex arg(char c) { return arg(id_t(c)+vertex::char_offset); }

inline func_t call_cast(vertex v) {
  assert(v.v.f);
  return v.v.f;
}

template<>
inline func_t arg_cast<func_t>(vertex v) { 
  assert(is_func(v));
  return id2func(v.v.d ^ vertex::funcarg_mask);
}
template<>
inline id_t arg_cast<id_t>(vertex v) {
  assert(is_symbol(v));
  return (v.v.d ^ vertex::symbolarg_mask);
}
template<>
inline number_t arg_cast<number_t>(vertex v) {
  assert(is_number(v));
  return v.v.n;
}

template<>
inline bool arg_cast<bool>(vertex v) { 
  assert(arg_cast<id_t>(v)==0 || arg_cast<id_t>(v)==vertex::true_id);
  return arg_cast<id_t>(v);
}
inline bool is_lang_arg(id_t d) { 
  return (d>=vertex::smallest_lang_arg && 
          d<=vertex::largest_lang_arg);
}
inline arity_t lang_arg_cast(id_t d) {
  assert(is_lang_arg(d));
  return (d-vertex::smallest_lang_arg);
}
template<>
inline char arg_cast<char>(vertex v) {
  assert(arg_cast<id_t>(v)>=vertex::smallest_char &&
         arg_cast<id_t>(v)<=vertex::largest_char);
  return arg_cast<id_t>(v)-vertex::char_offset;
}

inline bool is_func(vertex v) { 
  return ((v.v.d & vertex::symbolarg_mask)==vertex::funcarg_mask);
}
inline bool is_symbol(vertex v) { 
  return ((v.v.d & vertex::symbolarg_mask)==vertex::symbolarg_mask);
}
inline  bool is_number(vertex v) { return (!(v.v.d & vertex::funcarg_mask)); }

}} //namespace plap::lang
#endif //PLAP_LANG_VERTEX_H__
