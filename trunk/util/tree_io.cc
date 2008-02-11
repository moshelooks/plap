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
#include <boost/spirit/tree/ast.hpp>
#include <boost/assign/list_of.hpp>
#include "algorithm.h"
#include "tree_iterator.h"

namespace plap { namespace util {

const tree_io_modifier sexpr_format=tree_io_modifier();
const tree_io_modifier funcall_format=tree_io_modifier();

namespace util_private {
bool sexpr_io=false;
const infix_map imap=boost::assign::map_list_of 
    //nullary operators
    ("list","[]")
   
    //unary operators
    ("not","!")
    ("negative","-")
    ("lambda","\\")

    //binary operators
    ("plus","+")
    ("minus","-")
    ("times","*")
    ("div","/")

    ("equal","==")
    ("less","<")
    ("less_equal","<=")
    ("greater",">")
    ("greater_equal",">=")

    ("and","&&")
    ("or","||")

    ("arrow","->")
    ("cons",":")
    //ternary operators
    ("def","=")

    //variadic operators
    ("list","")
    ("list","[");

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

void to_sexpr(const tree_node<node_val_data<> >& s,subtree<string> d) {
  d.root()=symbol2name(string(s.value.begin(),s.value.end()));
  d.append(s.children.size(),string());
  for_each(s.children.begin(),s.children.end(),d.begin_sub_child(),&to_sexpr);
}
struct sexpr_grammar : public grammar<sexpr_grammar> {
  template<typename Scanner>
  struct definition {
    definition(const sexpr_grammar&) {
      sexpr    = inner_node_d[ch_p('(') >> list_x >> ch_p(')')];

      list_x   = def_x
               | root_node_d[ch_p('[')] >> infix_node_d[(list_x|sexpr) % ',']
                                        >> no_node_d[ch_p(']')];
      def_x    = !(term >> terms >> root_node_d[ch_p('=')])        >> lambda_x;
      lambda_x =           !root_node_d[ch_p('\\')]                >> arrow_x;
      arrow_x  = or_x   >> !(root_node_d[str_p("->")]              >> or_x);

      or_x     = and_x  >> *(root_node_d[str_p("||")]              >> and_x);
      and_x    = cons_x >> *(root_node_d[str_p("&&")]              >> cons_x);
      cons_x   = eq_x   >> *(root_node_d[ch_p(':')]                >> eq_x);
      eq_x     = cmp_x  >> *(root_node_d[str_p("==")|"!="]         >> cmp_x);
      cmp_x    = add_x  >> *(root_node_d[str_p("<=")|'<'|'>'|">="] >> add_x);
      add_x    = mlt_x  >> *(root_node_d[ch_p('+')|'-']            >> mlt_x);
      mlt_x    = neg_x  >> *(root_node_d[ch_p('*')|'/']            >> neg_x);
      neg_x    =           !root_node_d[ch_p('!')|ch_p('-')]       >> seq;

      seq     = root_node_d[term] >> *prime;
      prime   = sexpr | term;
      term    = token_node_d[lexeme_d[+(anychar_p-'|'-'&'-'='-'!'-'<'-'>'-','-
                                         '['-']'-'+'-'-'-'*'-'/'-'('-')'-':'-
                                        space_p)]]
              | inner_node_d[ch_p('(') >> term >> ch_p(')')] | str_p("[]");
      terms   = root_node_d[eps_p] >> *term;
    }
    rule<Scanner> sexpr,list_x,def_x,lambda_x,arrow_x;
    rule<Scanner> or_x,and_x,cons_x,eq_x,cmp_x,add_x,mlt_x,neg_x;
    rule<Scanner> seq,prime,term,terms;
    const rule<Scanner>& start() const { return sexpr; }
  };
};

tree<string>* tr;
tree<string>::sub_child_iterator at;
void begin_internal(const char* from, const char* to) {
  at=tr->insert(at,string(from,to-1))->begin_child();
}
void end_internal(const char) { ++++at; }
void add_leaf(const char* from, const char* to) {
  tr->insert(at,string(from,to));
}
struct funcall_grammar : public grammar<funcall_grammar> {
  template<typename ScannerT>
  struct definition {
    definition(const funcall_grammar&) {
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

void read_balanced(std::istream& in,string& str) {
  int nparen=0;
  do {
    string tmp;
    in >> tmp;
    nparen+=count(tmp.begin(),tmp.end(),'(')-count(tmp.begin(),tmp.end(),')');
    str+=tmp+' ';
  } while (in.good() && nparen>0);
  if (nparen!=0)
    throw std::runtime_error("paren mismatch parsing tree: '"+str+"'");
  str.erase(str.length()-1);
}

} //~namespace

std::istream& operator>>(std::istream& in,
                         tree<std::string>& dst) throw(std::runtime_error) {
  std::string str;
  read_balanced(in,str);
  string2sexpr(str,dst);
  return in;
}

void string2sexpr(const std::string& str,tree<std::string>& dst)
    throw(std::runtime_error) {
  if (str.empty())
    return;
  if (util_private::sexpr_io) {
    dst=tree<std::string>(std::string());
    const char* begin=str.c_str();
    tree_parse_info<> t=ast_parse(begin,begin+str.length(),
                                  sexpr_grammar(),space_p);

    if (!t.match || !t.full || t.trees.size()!=1)
      throw std::runtime_error("bad tree structure parsing '"+str+"'");
    to_sexpr(t.trees.front(),dst);
  } else {
    dst.clear();
    tr=&dst;
    at=dst.begin();
    parse(str.c_str(),funcall_grammar(),space_p);
  }
}

}} //namespace plap::util
