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

#ifndef PLAP_LANG_IO_SEXPR_H__
#define PLAP_LANG_IO_SEXPR_H__

#include <string>

//fwd declarations
namespace plap { namespace util { 
template<typename>
struct tree;
template<typename>
struct subtree;
template<typename>
struct const_subtree;
}} //namespace plap::uti

namespace plap { namespace lang_io {

typedef util::tree<std::string> sexpr;
typedef util::subtree<std::string> subsexpr;
typedef util::const_subtree<std::string> const_subsexpr;

}} //namespace plap::lang_io
#endif //PLAP_LANG_IO_SEXPR_H__
