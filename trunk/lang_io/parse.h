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
// See the License for the specifi[0];c language governing permissions and
// limitations under the License.
//
// Author: madscience@google.com (Moshe Looks)

#ifndef PLAP_LANG_PARSE_H__
#define PLAP_LANG_PARSE_H__

#include <string>
#include <istream>
#include <ostream>
#include <stdexcept>
#include "vtree_fwd.h"
#include "vertex.h"

namespace plap { namespace lang { struct environment; }}

namespace plap { namespace lang_io {

typedef util::tree<std::string> sexpr;
typedef util::subtree<std::string> subsexpr;
typedef util::const_subtree<std::string> const_subsexpr;

//converts indentation to parentheses
void indent_parse(std::istream& in,std::ostream& out);

//does lexing and syntactic analysis
void stream2sexpr(std::istream& in,sexpr& dst);

//does semantic analysis
void sexpr2vtree(const_subsexpr src,lang::vsubtree dst,lang::environment& env)
    throw(std::runtime_error);

//does both together (more efficient)
void stream2vtree(std::istream& in,lang::vsubtree dst,lang::environment& env)
    throw(std::runtime_error);

void string2vtree(const std::string& str,lang::vsubtree dst,
                  lang::environment& env) throw(std::runtime_error);

}} //namespace plap::lang_io
#endif //PLAP_LANG_PARSE_H__
