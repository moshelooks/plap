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

#ifndef PLAP_LANG_IO_REPL_H__
#define PLAP_LANG_IO_REPL_H__

#include <string>
#include <istream>
#include <ostream>
#include "sexpr.h"

namespace plap { namespace lang {
struct context;
}} //namespace plap::lang


namespace plap { namespace lang_io {

//reads from in and writes to out as long as in is good, 
//throws on language errors
void repl(std::istream& in,std::ostream& out,
          const std::string& prompt="\033[22;32m> \033[00;m\n");

void eval_print(std::ostream& out,const_subsexpr s,lang::context& c);

}} //namespace plap::lang_io
#endif //PLAP_LANG_IO_REPL_H__
