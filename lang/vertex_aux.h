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

#ifndef PLAP_LANG_VERTEX_AUX_H__
#define PLAP_LANG_VERTEX_AUX_H__

#include "vtree.h"
#include "tree.h"
#include "types.h"

#ifndef PLAP_LANG_VERTEX_UNION
#  include <string>
#  include <iostream>
#  include <boost/lexical_cast.hpp>
#endif //~ifndef NDEBUG

namespace lang {

#ifdef PLAP_LANG_VERTEX_UNION

template<typename T>
const T& vertex_cast(const vertex& v);
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

template<typename T>
struct vc_type { typedef T type; };
template<typename T>
struct vc_type<list_of<T> > { typedef list_t type; };

#define LANG_VERTEX_AUX_CAST(const_marker)                              \
  if (const_marker typename vc_type<T>::type* t=                        \
      boost::get<typename vc_type<T>::type>(&v)) {                      \
    return *t;                                                          \
  } else {                                                              \
    std::cerr << "expected a " << lang_private::type_name(T())          \
              << ", got a " << lang_private::type_name(v)               \
              << " (" << lang_private::type_value(v) << ")"             \
              << std::endl;                                             \
    exit(1);                                                            \
  }
template<typename T>
const typename vc_type<T>::type& vertex_cast(const vertex& v) { 
  LANG_VERTEX_AUX_CAST(const) ;
}
template<typename T>
typename vc_type<T>::type& vertex_cast(vertex& v) { 
  LANG_VERTEX_AUX_CAST();
}
#undef LANG_VERTEX_AUX_CAST

#endif //~ifdef PLAP_LANG_VERTEX_UNION

typedef vtree::sub_child_iterator        vsub_child_it;
typedef vtree::const_sub_child_iterator  const_vsub_child_it;

} //~namespace lang

#endif  // PLAP_LANG_VERTEX_AUX_H__
