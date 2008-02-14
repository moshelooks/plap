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

#include "bimap.h"
#include <vector>

namespace plap { namespace lang_io {

namespace lang_io_private {
struct op_name {};
struct op_symbol {};
typedef util::bimap<std::string,std::string,op_name,op_symbol>::type infix_map;
extern const std::vector<infix_map> infix_by_arity;
extern const infix_map infix_vararg;
inline const std::string& name2symbol(const std::string& s,
                                      std::string::size_type a) {
  infix_map::index<op_name>::type::const_iterator i;
  if (a<infix_by_arity.size()) {
    i=infix_by_arity[a].get<op_name>().find(s);
    if (i!=infix_by_arity[a].get<op_name>().end())
      return i->second;
  }
  i=infix_vararg.get<op_name>().find(s);
  return i==infix_vararg.get<op_name>().end() ? s : i->second;
}
inline const std::string& symbol2name(const std::string& s,
                                      std::string::size_type a) {
  infix_map::index<op_symbol>::type::const_iterator i;
  if (a<infix_by_arity.size()) {
    i=infix_by_arity[a].get<op_symbol>().find(s);
    if (i!=infix_by_arity[a].get<op_symbol>().end())
      return i->first;
  }
  i=infix_vararg.get<op_symbol>().find(s);
  return i==infix_vararg.get<op_symbol>().end() ? s : i->first;
}
} //namespace lang_io_private
using lang_io_private::name2symbol;
using lang_io_private::symbol2name;
extern const std::string def_symbol;
extern const std::string list_name;
extern const std::string strlit_symbol;
extern const std::string apply_symbol;
extern const std::string apply_name;
}} //namespace plap::lang_io
#endif //PLAP_LANG_IO_OPERATORS_H__
