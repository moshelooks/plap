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

#include "tree_io.h"
#include <algorithm>
#include <boost/spirit/core.hpp>

namespace plap { namespace util {

const tree_io_modifier sexpr_format=tree_io_modifier();
const tree_io_modifier funcall_format=tree_io_modifier();
namespace util_private {
bool sexpr_io=false;
} //namespace util_private

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
  at=tr->insert(at,string(from,to-1))->begin_child();
}
void end_internal(const char) {  ++++at; }
void add_leaf(const char* from, const char* to) {
  tr->insert(at,string(from,to));
}

struct TreeGrammar : public grammar<TreeGrammar> {
  std::vector<string> v;
  
  template<typename ScannerT>
  struct definition {
    definition(const TreeGrammar&) {
      term=lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p))]
          [&add_leaf];
      beg=lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p)) >> '('];
      expr=(beg[&begin_internal] >> +expr >> ch_p(')')[&end_internal]) |
          term;
    }
    rule<ScannerT> expr,beg,term;
      
    const rule<ScannerT>& start() const { return expr; }
  };
};

} //~namespace

std::istream& operator>>(std::istream& in,
                         tree<std::string>& dst) throw (std::runtime_error) {
  std::string str,tmp;
  int nparen=0;
  do {
    in >> tmp;
    nparen+=count(tmp.begin(),tmp.end(),'(')-count(tmp.begin(),tmp.end(),')');
    str+=tmp+' ';
  } while (in.good() && nparen>0);
  if (nparen!=0)
    throw std::runtime_error("paren mismatch parsing tree: '"+str+"'");

  tr=&dst;
  dst.clear();
  at=dst.begin();

  TreeGrammar tg;
  parse(str.c_str(),tg,space_p);

  return in;
}

}} //namespace plap::util
