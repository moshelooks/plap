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

#ifndef PLAP_LANG_IO_NAMES_H__
#define PLAP_LANG_IO_NAMES_H__

#include <string>
#include <tr1/unordered_map>
#include <vector>
#include "bimap.h"
#include "func.h"

namespace plap { namespace lang_io {

typedef std::vector<std::string> argname_seq;
namespace lang_io_private {
using namespace lang;
struct left {};
struct right {};
typedef util::bimap<std::string,func_t,left,right>::type func_index;
typedef std::tr1::unordered_map<func_t,argname_seq> arg_index;
typedef util::bimap<std::string,disc_t,left,right>::type symbol_index;

extern func_index func_names;
extern arg_index arg_names;
extern symbol_index symbol_names;
} //namespace lang_io_private

inline void name_func(lang::func_t f,const std::string& name) {
  lang_io_private::func_names.insert(make_pair(name,f));
}
template<typename Iterator>
inline void name_args(lang::func_t f,Iterator name_f,Iterator name_l) {
  lang_io_private::arg_names.insert(make_pair(f,argname_seq(name_f,name_l)));
}

inline lang::func_t name2func(const std::string& name) {
  using namespace lang_io_private;
  func_index::index<left>::type::const_iterator i=
      boost::multi_index::get<left>(func_names).find(name);
  return i==boost::multi_index::get<left>(func_names).end()
      ? NULL : i->second;
}
inline const std::string* func2name(lang::func_t f) { 
  using namespace lang_io_private;
  func_index::index<right>::type::const_iterator i=
      boost::multi_index::get<right>(func_names).find(f);
  return i==boost::multi_index::get<right>(func_names).end()
      ? NULL : &i->first;
}
inline const argname_seq& func2arg_names(lang::func_t f) {
  using namespace lang_io_private;
  assert(arg_names.find(f)!=arg_names.end());
  return arg_names.find(f)->second;
}

inline void name_symbol(lang::disc_t s,const std::string& name) {
  lang_io_private::symbol_names.insert(make_pair(name,s));
}

inline lang::disc_t name2symbol(const std::string& name) { 
  using namespace lang_io_private;
  assert(boost::multi_index::get<left>(symbol_names).find(name)!=
         boost::multi_index::get<left>(symbol_names).end());
  return boost::multi_index::get<left>(symbol_names).find(name)->second;
}
inline const std::string& symbol2name(lang::disc_t s) {
  using namespace lang_io_private;
  assert(boost::multi_index::get<right>(symbol_names).find(s)!=
         boost::multi_index::get<right>(symbol_names).end());
  return boost::multi_index::get<right>(symbol_names).find(s)->first;
}

extern const char def_symbol[];
extern const char strlit_symbol[];
extern const char apply_symbol[];
extern const char cons_symbol[];

extern const char def_name[];
extern const char strlit_name[];
extern const char apply_name[];

extern const char list_name[];
extern const char lambda_name[];
extern const char let_name[];
extern const char decl_name[];
extern const char pair_name[];

extern const char nil_name[];
extern const char true_name[];
extern const char false_name[];

extern const char plus_name[];

extern const char if_name[];

extern const char func_name[];

}} //namespace plap::lang_io
#endif //PLAP_LANG_IO_NAMES_H__
