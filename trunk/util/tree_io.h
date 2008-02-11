// Copyright 2007 Google Inc. All Rights Reserved.
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

#ifndef PLAP_UTIL_TREE_IO_H__
#define PLAP_UTIL_TREE_IO_H__

#include <istream>
#include <ostream>
#include <string>
#include <exception>
#include <vector>
#include "tree.h"
#include "bimap.h"

namespace plap { namespace util {

struct tree_io_modifier {};
extern const tree_io_modifier sexpr_format;   //(trees (like this) for io)
extern const tree_io_modifier funcall_format; //trees(like(this) for io)
namespace util_private {
extern bool sexpr_io;
struct op_name {};
struct op_symbol {};

typedef bimap<std::string,std::string,op_name,op_symbol>::type infix_map;
extern const std::vector<infix_map> imap;
inline const std::string& name2symbol(const std::string& s,std::size_t a) {
  if (a>=imap.size())
    return s;
  infix_map::index<op_name>::type::const_iterator i=
        boost::multi_index::get<op_name>(imap[a]).find(s);
    return i==boost::multi_index::get<op_name>(imap[a]).end() ? s : i->second;
  }
  inline const std::string& symbol2name(const std::string& s,std::size_t a) {
    infix_map::index<op_symbol>::type::const_iterator i=
        boost::multi_index::get<op_symbol>(imap[a]).find(s);
    return i==boost::multi_index::get<op_symbol>(imap[a]).end() ? s : i->first;
  }
} //namespace util_private
using util_private::name2symbol;
using util_private::symbol2name;

std::ostream& operator<<(std::ostream& out,const tree_io_modifier& m);

template<typename T>
std::ostream& operator<<(std::ostream& out,const_subtree<T> tr) {
  if (tr.childless())
    return out << tr.root();

  if (util_private::sexpr_io)
    out << "(" << tr.root() << " ";
  else
    out << tr.root() << "(";
  for (typename const_subtree<T>::const_sub_child_iterator 
           it=tr.begin_sub_child();it!=tr.end_sub_child();++it)
    out << *it << (boost::next(it)==tr.end_sub_child() ? ")" : " ");
  return out;
}
template<typename T>
std::ostream& operator<<(std::ostream& out,subtree<T> tr) {
  return out << const_subtree<T>(tr);
}
template<typename T>
std::ostream& operator<<(std::ostream& out,const tree<T>& tr) {
  if (!tr.empty())
    out << const_subtree<T>(tr);
  return out;
}

std::istream& operator>>(std::istream&,
                         tree<std::string>&) throw (std::runtime_error);

}} //namespace plap::util

#endif //PLAP_UTIL_TREE_IO_H__