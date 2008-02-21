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

// the indent parser reads expressions in indent format:
// foo
//  bar baz
//  bla
// 
// and transforms into paren-format:
// (foo (bar baz) bla)
//
// it also discards comments (# to newline)
//
// a comma at the end of a line causes the follow indent to be ignored, e.g.:
// foo [this list,
//      spans,
//      many lines] fooarg1
//  fooarg2
//
// goes to
// (foo [this list,spans,many lines] fooarg1 fooarg2)

#ifndef PLAP_UTIL_INDENT_H__
#define PLAP_UTIL_INDENT_H__

#include <istream>
#include <ostream>

namespace plap { namespace util {

void indent2parens(std::istream&,std::ostream&,std::string::size_type=0);

}} //namespace plap::util
#endif //PLAP_UTIL_INDENT_H__
