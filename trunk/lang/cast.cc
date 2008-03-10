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

#include "vertex.h"
#if 0

#ifndef PLAP_LANG_VERTEX_UNION
#include <boost/lexical_cast.hpp>
//fixme#include "world.h"
#include "func.h"
#endif //ifdef PLAP_LANG_VERTEX_UNION


namespace plap { namespace lang {
namespace lang_private {
#ifdef PLAP_LANG_VERTEX_UNION
const disc_t arg_mask=
#if (sizeof(contin_t)==4)
    4;
#else
    8;
  BOOST_STATIC_ASSERT(sizeof(contin_t)==8);
#endif
const disc_t arg_idx_mask=0xFF;
#else //ifdef PLAP_LANG_VERTEX_UNION
std::string type_name(const vertex& v) {
  if (boost::get<disc_t>(&v))   return "disc_t";
  if (boost::get<contin_t>(&v)) return "contin_t";
  if (boost::get<world_t>(&v)) return "world_t";
  assert(boost::get<func_t>(&v));
  return "func_t";
}
std::string type_value(const vertex& v) {
#if 0 //fixme
  if (const list_t* l=boost::get<list_t>(&v)) {
    assert(*l);
    return boost::lexical_cast<std::string>(**l);
  } else if (const world_t* w=boost::get<world_t>(&v)) {
    assert(*w);
    return boost::lexical_cast<std::string>(**w);
  } else if (const func_t* f=boost::get<func_t>(&v)) {
    assert(*f);
    return boost::lexical_cast<std::string>(**f);
  }
  assert(boost::get<disc_t>(&v) || boost::get<contin_t>(&v));
  return boost::lexical_cast<std::string>(v);
#endif
  return "foobar";
}
#endif //ifdef PLAP_LANG_VERTEX_UNION ... else

} //namespace plap::lang_private
}} //namespace plap::lang

#endif
