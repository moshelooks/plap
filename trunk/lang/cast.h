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

#ifndef PLAP_LANG_CAST_H__
#define PLAP_LANG_CAST_H__

#include "type.h"

namespace plap { namespace lang {

//needed to do template partial specialization
template<typename T>
struct literal_caster { 
  T operator()(const_subvtree s) { 
    assert(s.childless());
    return arg_cast<T>(s.root());
  }
};

template<>
struct literal_caster<const_subvtree> {
  const_subvtree operator()(const_subvtree s) { return s; }
};
template<typename T>
struct literal_caster<list_of<T> > {
  list_of<T> operator()(const_subvtree s) { return list_of<T>(s); }
};
template<typename T>
struct literal_caster<func_of<T> > {
  func_of<T> operator()(const_subvtree s) { 
    assert(s.childless());
    return func_of<T>(arg_cast<func_t>(s.root()));
  }
};

template<typename T> //for convenience
T literal_cast(const_subvtree s) { return literal_caster<T>()(s); }

template<typename T>
struct arg_visitor { typedef T result_type; };

template<typename Visitor>
typename Visitor::result_type arg_visit(const Visitor& visit,vertex v) {
  if (is_func(v))
    return visit(arg_cast<func_t>(v));
  if (is_symbol(v))
    return visit(arg_cast<id_t>(v));
  return visit(arg_cast<number_t>(v));
}

}} //namespace plap::lang
#endif //PLAP_LANG_CAST_H__

#if 0

#include <boost/static_assert.hpp>
#include "vtree.h"
#include "type.h"

#ifndef PLAP_LANG_VERTEX_UNION
#  include <string>
#  include <stdexcept>
#endif //ifndef PLAP_LANG_VERTEX_UNION

namespace plap { namespace lang {

inline bool is_number(vertex v);
inline bool is_symbol(vertex v);
inline bool is_func(vertex v);

bool is_arg(disc_t d);


#ifdef PLAP_LANG_VERTEX_UNION

#define LANG_CAST_vertex_cast(type,name)                                \
  template<>                                                            \
  inline type vertex_cast<type>(const vertex& v) { return v. name; }    \
  template<>                                                            \
  inline type& vertex_cast<type>(vertex& v) { return v. name; }
LANG_CAST_vertex_cast(disc_t,d)
LANG_CAST_vertex_cast(contin_t,c)
LANG_CAST_vertex_cast(world_t,w)
LANG_CAST_vertex_cast(func_t,f)
#undef LANG_CAST_vertex_cast

namespace lang_private {
BOOST_STATIC_ASSERT(std::numeric_limits<contin_t>::has_quiet_NaN);
BOOST_STATIC_ASSERT(sizeof(contin_t)==sizeof(disc_t));
extern const disc_t arg_mask;
extern const disc_t arg_idx_mask;
} //namespace lang_private

template<typename T>
inline vertex arg(arity_t a);


bool is_arg(vertex v) {
  (v.d & lang_private::arg_mask)

template<>
inline bool is_arg<disc_t>(vertex v) { return (v.d & lang_private::arg_mask); }
template<>inline bool is_arg<func_t>(vertex v) { return v.f->is_arg(); };

template<>inline arity_t arg_idx<disc_t>(vertex v) { 
  return (v.d & lang_private::arg_idx_mask); 
}
template<>inline arity_t arg_idx<func_t>(vertex v) { return v.f->arg_idx(); }

template<>inline vertex arg<disc_t>(arity_t a) { 
  return (disc_t(a) | lang_private::arg_mask); 
}
template<>inline vertex arg<func_t>(arity_t a) { 
  return arg_func::instance(a);
}


#else //ifdef PLAP_LANG_VERTEX_UNION

namespace lang_private {
std::string type_name(const vertex& v);
std::string type_value(const vertex& v);
} //namespace lang_private

#define LANG_vertex_cast(ref_marker,const_marker)                       \
  template<typename T>                                                  \
  T ref_marker vertex_cast(const_marker vertex& v) {                    \
    if (const_marker T* t=boost::get<T>(&v)) {                          \
      return *t;                                                        \
    } else {                                                            \
      throw std::runtime_error                                          \
          ("expected a "+lang_private::type_name(T())+                  \
           ", got a "+lang_private::type_name(v)+                       \
           " ("+lang_private::type_value(v)+")");                       \
    }                                                                   \
  }
LANG_vertex_cast(,const)
LANG_vertex_cast(&,)
#undef LANG_vertex_cast

template<typename T>
inline bool is_arg(const vertex& v) { return boost::get<actual_arg>(&v); }
template<typename T>
inline arity_t arg_idx(const vertex& v) { 
  assert(is_arg<T>(v));
  return boost::get<actual_arg>(v).idx; 
}

#endif //ifdef PLAP_LANG_VERTEX_UNION ... else


template<>
inline const_subvtree literal_cast<const_subvtree>(const_subvtree s) { 
  return s; 
}

#endif