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

//#define BOOST_SPIRIT_DEBUG

#include "tree_io.h"
#include <algorithm>
#include <boost/spirit/core.hpp>
#include "io.h"

namespace plap { namespace util {

const tree_io_modifier sexpr_format=tree_io_modifier();
const tree_io_modifier funcall_format=tree_io_modifier();

namespace util_private {
bool sexpr_io=false;
} //~namespace util_private
std::ostream& operator<<(std::ostream& out,const tree_io_modifier& m) {
  assert(&m==&sexpr_format || &m==&funcall_format);
  if (&m==&sexpr_format)
    util_private::sexpr_io=true;
  if (&m==&funcall_format)
    util_private::sexpr_io=false;
  return out;
}

namespace {
using namespace boost::spirit;
using std::string;

tree<string>* tr;
tree<string>::sub_child_iterator at;
void begin_internal(const char* from, const char* to) {
  if (util_private::sexpr_io)
    ++from;
  else
    --to;
  at=tr->insert(at,string(from,to))->begin_child();
}
void end_internal(const char) { ++++at; }
void add_leaf(const char* from, const char* to) {
  tr->insert(at,string(from,to));
}
struct tree_grammar : public grammar<tree_grammar> {
  template<typename ScannerT>
  struct definition {
    definition(const tree_grammar&) {
      term=lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p))]
          [&add_leaf];
      if (util_private::sexpr_io)
        beg=lexeme_d['(' >> (+( anychar_p - ch_p('(') - ch_p(')') - space_p))];
      else
        beg=lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p)) >> '('];
      expr=(beg[&begin_internal] >> +expr >> ch_p(')')[&end_internal]) |
          term;
    }
    rule<ScannerT> expr,beg,term;
    
    const rule<ScannerT>& start() const { return expr; }
  };
};
} //~namespace

std::istream& operator>>(std::istream& in,tree<std::string>& dst) {
  std::string str;
  read_balanced(in,str);
  
  dst.clear();
  tr=&dst;
  at=dst.begin();
  parse(str.c_str(),tree_grammar(),space_p);

  return in;
}

}} //namespace plap::util
