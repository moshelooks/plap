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

#include <boost/lexical_cast.hpp>
#include "tree_io.h"
#include "environment.h"

namespace plap { namespace lang {

void to_string_tree(std::istream& in,util::subtree<std::string> dst) {

}

void to_vtree(util::const_subtree<std::string> src,vsubtree dst,
              const environment& env) {
  if (src.childless()) {
    dst.root()=to_vertex(src.root(),env);
  } else {
    dst.append_children(src.arity());
    std::for_each(dst.begin_sub_child(),dst.end_sub_child())
}

vertex leaf_to_vertex(const std::string& str,const environment& env,
                      const bindings& scalars,const bindings& lets,
                      const errinfo& e) {
  assert(!str.empty());
  char c=str.front();
  if (c=='-' || c=='.' (c>='0' && c<='9'))
    return boost::lexical_cast<number_t>(c);
  if (c=='$') {
    bindings::const_iterator i=scalars.find(str.substr(1));
    if (i==scalars.end()) {
      validate_identifier(str.substr(1),e);
      throw bad_scalar(e,str);
    }
    return i->second;
  }
  bindings::const_iterator i=lets.find(str);
  if (i!=lets.end())
    return i->second;
  i=env.func_bindings().find(str);
  if (i==env.func_bindings().end()) {
    validate_identifier(e,str);
    throw bad_identfier(e,str);
  }
  return i->second;
}

}} //namespace plap::lang
