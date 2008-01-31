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

#include "slist.h"

//if assertions are enabled then vertices (individual nodes in program trees)
//are implemented using boost::variant, which allows for runtime
//type-checking. Otherwise, a union is used, which will fail nastily in the
//case of a bug or a syntactically incorrect program, but is faster and uses
//less memory (1-4 bytes less per program tree node)
#ifdef NDEBUG
#  define PLAP_LANG_VERTEX_UNION
#else //~ifndef NDEBUG
#  include <boost/variant.hpp>
#endif //~ifdef NDEBUG

//fwd declarations
namespace util { 
template<typename>
struct tree;
template<typename>
struct subtree;
template<typename>
struct const_subtree;
} //~namespace util

namespace lang {

struct world;
struct def;
//fixmestruct rewrite;

typedef int       disc_t;
typedef float     contin_t;
typedef world*    world_t;
typedef def*      def_t;
//fixme typedef rewrite*  rewrite_t;

#ifdef PLAP_LANG_VERTEX_UNION
union vertex { //we mirror the behavior of the boost::variant 1-arg ctors
  vertex(util::slist<util::tree<vertex> >* l_) : l(l_) {}
  vertex(disc_t d_) : d(d_) {}
  vertex(contin_t c_) : c(c_) {}
  vertex(world_t w_) : w(w_) {}
  vertex(def_t f_) : f(f_) {}

  vertex() {} //junk

  util::slist<util::tree<vertex> >* l;
  disc_t d; contin_t c; world_t w; def_t f; 
};
#else //~ifndef PLAP_LANG_VERTEX_UNION
typedef boost::make_recursive_variant<util::slist<
                                        util::tree<
                                          boost::recursive_variant_> >*,
                                      disc_t,
                                      contin_t,
                                      world_t,
                                      def_t>::type vertex;
#endif //~ifdef PLAP_LANG_VERTEX_UNION

typedef util::slist<util::tree<vertex> > list_t;
typedef util::tree<vertex>               vtree;
typedef util::subtree<vertex>            vsubtree;
typedef util::const_subtree<vertex>      const_vsubtree;

} //~namespace lang

#endif  // PLAP_LANG_VERTEX_H__
