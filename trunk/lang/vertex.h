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
#else //ifndef NDEBUG
#  include <boost/variant.hpp>
#  include <ostream>
#endif //ifdef NDEBUG

namespace plap { namespace lang {

struct world;
struct func_base;
//fixmestruct rewrite;

typedef int        disc_t;
typedef float      contin_t;
typedef world*     world_t;
typedef func_base* func_t;
//fixme typedef rewrite*  rewrite_t;

typedef unsigned char arity_t;

#ifdef PLAP_LANG_VERTEX_UNION
union vertex { //we mirror the behavior of the boost::variant 1-arg ctors
  vertex(disc_t d_)   : d(d_) {}
  vertex(contin_t c_) : c(c_) {}
  vertex(world_t w_)  : w(w_) {}
  vertex(func_t f_)    : f(f_) {}

  vertex() {} //junk

  disc_t d; contin_t c; world_t w; func_t f; 
};
#else //ifndef PLAP_LANG_VERTEX_UNION
struct arg { 
  explicit arg(arity_t a) : idx(a) {} 
  arity_t idx; 
  bool operator==(const arg& rhs) const { return idx==rhs.idx; }
};
typedef boost::variant<disc_t,
                       contin_t,
                       world_t,
                       func_t,
                       arg> vertex;
#endif //ifdef PLAP_LANG_VERTEX_UNION

}} //namespace plap::lang
#ifndef PLAP_LANG_VERTEX_UNION
//very nasty...
namespace boost { namespace detail { namespace variant {
inline std::ostream& operator<<(std::ostream& out,
                                const plap::lang::arg& a) {
  return out << "#" << a.idx; 
}
}}}
#endif //ifndef PLAP_LANG_VERTEX_UNION
#endif //PLAP_LANG_VERTEX_H__
