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

//if assertions are enabled then vertices (individual nodes in program trees)
//are implemented using boost::variant, which allows for runtime
//type-checking. Otherwise, a union is used, which will fail nastily in the
//case of a bug or a syntactically incorrect program, but is faster and uses
//less memory (1-4 bytes less per program tree node)
#ifdef NDEBUG
#  define PLAP_LANG_VERTEX_UNION
#else //~ifndef NDEBUG
#  include <string>
#  include <iostream>
#  include <boost/variant.hpp>
#  include <boost/lexical_cast.hpp>
#endif //~ifdef NDEBUG

#include "slist.h"
#include "tree.h"

namespace lang {

struct world;
struct func_def;
//fixmestruct rewrite;

typedef int       disc_t;
typedef float     contin_t;
typedef world*    world_t;
typedef func_def* function_t;
//fixme typedef rewrite*  rewrite_t;

#ifdef PLAP_LANG_VERTEX_UNION
union vertex { //we mirror the behavior of the boost::variant 1-arg ctors
  vertex(util::slist<util::tree<vertex> >* l_) : l(l_) {}
  vertex(disc_t d_) : d(d_) {}
  vertex(contin_t c_) : c(c_) {}
  vertex(world_t w_) : w(w_) {}
  vertex(function_t f_) : f(f_) {}

  vertex() {} //junk

  util::slist<util::tree<vertex> >* l;
  disc_t d; contin_t c; world_t w; function_t f; 
};
#else //~ifndef PLAP_LANG_VERTEX_UNION
typedef boost::make_recursive_variant<util::slist<
                                        util::tree<
                                          boost::recursive_variant_> >*,
                                      disc_t,
                                      contin_t,
                                      world_t,
                                      function_t>::type vertex;
#endif //~ifdef PLAP_LANG_VERTEX_UNION

typedef util::slist<util::tree<vertex> > vlist;
typedef vlist* list_t;

#ifdef PLAP_LANG_VERTEX_UNION

template<typename T>
T vertex_cast(const vertex& v);
template<typename T>
T& vertex_cast(vertex& v);

template<>
disc_t vertex_cast<disc_t>(const vertex& v) { return v.d; }
template<>
disc_t& vertex_cast<disc_t>(vertex& v) { return v.d; }

#else //~ifndef PLAP_LANG_VERTEX_UNION

namespace lang_private {
std::string type_name(const vertex& v);
std::string type_value(const vertex& v);
} //~namespace lang_private

#define VERTEX_CAST \
  if (const T* t=boost::get<T>(&v)) {                                   \
    return *t;                                                          \
  } else {                                                              \
    std::cerr << "expected a " << lang_private::type_name(T())          \
              << ", got a " << lang_private::type_name(v)               \
              << " (" << lang_private::type_value(v) << ")"             \
              << std::endl;                                             \
    exit(1);                                                            \
  }
template<typename T>
T vertex_cast(const vertex& v) { VERTEX_CAST }
template<typename T>
T& vertex_cast(vertex& v) { VERTEX_CAST }
#undef VERTEX_CAST

#endif //~ifdef PLAP_LANG_VERTEX_UNION

typedef util::tree<vertex>               vtree;
typedef util::subtree<vertex>            vsubtree;
typedef util::const_subtree<vertex>      const_vsubtree;
typedef vtree::sub_child_iterator        vsub_child_it;
typedef vtree::const_sub_child_iterator  const_vsub_child_it;

} //~namespace lang

#endif  // PLAP_LANG_LANG_H__
