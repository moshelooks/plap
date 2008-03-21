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

#ifndef PLAP_LANG_IO_PRETTY_PRINT_H__
#define PLAP_LANG_IO_PRETTY_PRINT_H__

#include <ostream>
#include "vtree_fwd.h"

namespace plap { namespace lang { struct func; }}

namespace plap { namespace lang_io {

void pretty_print(std::ostream&,lang::const_subvtree,
                  std::size_t indent=0,std::size_t line_max=80);

void pretty_print(std::ostream&,const lang::func*,
                  std::size_t indent=0,std::size_t line_max=80);

std::ostream& operator<<(std::ostream&,lang::const_subvtree);
std::ostream& operator<<(std::ostream& o,lang::subvtree s);
std::ostream& operator<<(std::ostream& o,const lang::vtree& v);

}} //namespace plap::lang_io

namespace boost { namespace detail { using plap::lang_io::operator<<; }}
namespace plap { namespace lang { using plap::lang_io::operator<<; }}
#endif //PLAP_LANG_IO_PRETTY_PRINT_H__
