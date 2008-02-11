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

#define BOOST_SPIRIT_DEBUG

#include "tree_io.h"
#include <algorithm>
#include <boost/spirit/core.hpp>
#include <boost/spirit/tree/ast.hpp>
#include <boost/assign/list_of.hpp>
#include "algorithm.h"
#include "tree_iterator.h"

#include <iostream>
using namespace std;//fixme

namespace plap { namespace util {

const tree_io_modifier sexpr_format=tree_io_modifier();
const tree_io_modifier funcall_format=tree_io_modifier();

namespace util_private {
bool sexpr_io=false;
const std::vector<infix_map> imap=boost::assign::list_of<infix_map>
    (boost::assign::map_list_of //nullary operators
     ("empty_list","[]"))
    (boost::assign::map_list_of //unary operators
     ("not","!")
     ("negative","-")
     ("lambda","\\"))

    (boost::assign::map_list_of //binary operators
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

     ("arrow","->"))
    (boost::assign::map_list_of //ternary operators
     ("def","="));
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
  std::size_t a=s.children.size();
  tree_node<node_val_data<> >::children_t::const_iterator from=
      s.children.begin();
  std::vector<char>::const_iterator nm_f=s.value.begin(),nm_l=s.value.end();

  //if (nm_f==nm_l) {
  if (a==0) {
    if (s.children.empty())
      throw std::runtime_error("Can't resolve empty leaf in parse");
    --a;
    nm_f=s.children.begin()->value.begin();
    nm_l=s.children.begin()->value.end();
    ++from;
  }

  d.root()=symbol2name(string(nm_f,nm_l),a);
  d.append(a,string());
  for_each(from,s.children.end(),d.begin_sub_child(),&to_sexpr);
}
struct sexpr_grammar : public grammar<sexpr_grammar> {
  template<typename Scanner>
  struct definition {
    definition(const sexpr_grammar&) {
      prime=sexpr | term;

      or_expr=prime >> *(str_p("||") >> prime);
      and_expr=or_expr >> *(str_p("&&") >> or_expr);
      eq_expr=and_expr >> *(str_p("==")|"!=" >> and_expr);
      cmp_expr=eq_expr >> *(str_p("<=")|"<"|">"|">=" >> eq_expr);
      add_expr=cmp_expr >> *(ch_p('+')|'-' >> cmp_expr);
      mlt_expr=add_expr >> *(ch_p('*')|'/' >> add_expr);
      unary_expr=!ch_p('!')|ch_p('-') >> mlt_expr;

      sexpr=inner_node_d[ch_p('(') >> *unary_expr >> ch_p(')')];

      term = token_node_d[lexeme_d[+(anychar_p-
                                     '|'-'&'-'='-'!'-'<'-'+'-'-'-'*'-'/'-
                                     '('-')'-space_p)]] |
          inner_node_d[ch_p('(') >> term >> ch_p(')')];


      BOOST_SPIRIT_DEBUG_RULE(sexpr);
      BOOST_SPIRIT_DEBUG_RULE(seq);
      BOOST_SPIRIT_DEBUG_RULE(term);
      BOOST_SPIRIT_DEBUG_RULE(prime);
      BOOST_SPIRIT_DEBUG_RULE(or_expr);
      BOOST_SPIRIT_DEBUG_RULE(and_expr);
      BOOST_SPIRIT_DEBUG_RULE(eq_expr);
      BOOST_SPIRIT_DEBUG_RULE(cmp_expr);
      BOOST_SPIRIT_DEBUG_RULE(add_expr);
      BOOST_SPIRIT_DEBUG_RULE(mlt_expr);
      BOOST_SPIRIT_DEBUG_RULE(unary_expr);
    }
    rule<Scanner> sexpr,seq,term,prime,or_expr,and_expr,eq_expr,cmp_expr,
      add_expr,mlt_expr,unary_expr;
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
                         tree<std::string>& dst) throw (std::runtime_error) {
  std::string str;
  read_balanced(in,str);
  if (str.empty())
    return in;
  dst.clear();

  if (util_private::sexpr_io) {
    dst.insert(dst.begin(),std::string());
    const char* begin=str.c_str();
    tree_parse_info<> t=ast_parse(begin,begin+str.length(),
                                  sexpr_grammar(),space_p);

    cout << t.match << " " << t.full << " " << t.trees.size() << endl;
    for (unsigned int i=0;i<t.trees.size();++i) {
      tree<string> tmp=dst;
      to_sexpr(t.trees.front(),tmp);
      cout << tmp.size() << " | '" << tmp << "'" << endl;
    }
    cout << "XX" << endl;

    if (!t.match || !t.full || t.trees.size()!=1)
      throw std::runtime_error("bad tree structure parsing '"+str+"'");
    to_sexpr(t.trees.front(),dst);
  } else {
    tr=&dst;
    at=dst.begin();
    parse(str.c_str(),funcall_grammar(),space_p);
  }

  return in;
}

}} //namespace plap::util
