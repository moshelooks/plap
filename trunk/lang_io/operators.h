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

#ifndef PLAP_LANG_IO_OPERATORS_H__
#define PLAP_LANG_IO_OPERATORS_H__

#include <vector>
#include <set>
#include "bimap.h"

namespace plap { namespace lang_io {

namespace lang_io_private {
//name <-> operator (e.g. cons <-> :)
typedef util::bimap<std::string,std::string>::type infix_map;
extern const std::vector<infix_map> infix_by_arity;
extern const infix_map infix_variadic;
typedef std::set<std::string> vararg_set;
extern vararg_set varargs;
inline const std::string& name2operator(const std::string& s,
                                        std::string::size_type a) {
  infix_map::nth_index<0>::type::const_iterator i;
  if (a<infix_by_arity.size()) {
    i=infix_by_arity[a].get<0>().find(s);
    if (i!=infix_by_arity[a].get<0>().end())
      return i->second;
  }
  i=infix_variadic.get<0>().find(s);
  return i==infix_variadic.get<0>().end() ? s : i->second;
}
inline const std::string& operator2name(const std::string& s,
                                        std::string::size_type a) {
  infix_map::nth_index<1>::type::const_iterator i;
  if (a<infix_by_arity.size()) {
    i=infix_by_arity[a].get<1>().find(s);
    if (i!=infix_by_arity[a].get<1>().end())
      return i->first;
  }
  i=infix_variadic.get<1>().find(s);
  return i==infix_variadic.get<1>().end() ? s : i->first;
}
inline bool vararg(const std::string& name) {
  return varargs.find(name)!=varargs.end();
}
inline void set_vararg(const std::string& name) {
  varargs.insert(name);
}
} //namespace lang_io_private
using lang_io_private::name2operator;
using lang_io_private::operator2name;
using lang_io_private::vararg;
using lang_io_private::set_vararg;

}} //namespace plap::lang_io
#endif //PLAP_LANG_IO_OPERATORS_H__
