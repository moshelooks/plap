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

#include <iostream>
#include "util/tree.h"

namespace util {

template<typename T>
std::ostream& operator<<(std::ostream& out,const_subtree<T> tr) {
   out << tr.root();
   if (!tr.childless()) {
     out << "(";
     for (typename const_subtree<T>::const_sub_child_iterator 
              it=tr.begin_sub_child();it!=tr.end_sub_child();++it)
       out << *it << (boost::next(it)==tr.end_sub_child() ? ")" : " ");
   }
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

} //~namespace util

#endif  // PLAP_UTIL_TREE_IO_H__
