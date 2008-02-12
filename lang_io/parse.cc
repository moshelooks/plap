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

#include "parse.h"
#include "algorithm.h"
#include <sstream>
#include <boost/spirit/core.hpp>
#include <boost/spirit/tree/ast.hpp>
#include <boost/spirit/tree/parse_tree.hpp>
#include "tree.h"
#include "operators.h"
#include "indent.h"

namespace plap { namespace lang_io {

namespace {
using namespace boost::spirit;
using std::string;

void tosexpr(const tree_node<node_val_data<> >& s,subsexpr d) {
  string::size_type arity=s.children.size();
  string name=string(s.value.begin(),s.value.end());

  if (name==def_symbol) { //special case
    if (arity!=2u)
      throw std::runtime_error("Malformed def of '"+name+"'.");
    ++arity;
    d.append(string(s.children[0].value.begin(),s.children[0].value.end()));
    d.append(string());
    if (!s.children[0].children.empty())
      tosexpr(s.children[0],d.back_sub());
    d.back()=list_name;
    d.append(string());
    tosexpr(s.children[1],d.back_sub());
  } else {
    d.append(s.children.size(),string());
    for_each(s.children.begin(),s.children.end(),d.begin_sub_child(),&tosexpr);
  }
  d.root()=symbol2name(name,arity);
}
struct sexpr_grammar : public grammar<sexpr_grammar> {
  template<typename Scanner>
  struct definition {
    definition(const sexpr_grammar&) {
      sexpr    = inner_node_d[ch_p('(') >> range_x >> ch_p(')')];

      range_x  = list_x
               | (no_node_d[ch_p('[')] >> (seq|sexpr)
                  >> root_node_d[str_p("..")] 
                  >> (seq|sexpr) >> no_node_d[ch_p(']')]);
      list_x   = def_x
               | root_node_d[ch_p('[')] >> infix_node_d[(list_x|sexpr) % ',']
                                        >> no_node_d[ch_p(']')];

      def_x    = lambda_x  >> !(root_node_d[ch_p('=')] >>
                                (eps_p(~ch_p('=') >> *anychar_p) >> lambda_x));

      lambda_x =            !root_node_d[ch_p('\\')]               >> arrow_x;
      arrow_x  = seq   >> *(root_node_d[str_p("->")]              >> seq);

      seq      = (root_node_d[or_x] >> *or_x);

      or_x     = and_x  >> *(root_node_d[str_p("||")]              >> and_x);
      and_x    = cons_x >> *(root_node_d[str_p("&&")]              >> cons_x);
      cons_x   = eq_x   >> *(root_node_d[ch_p(':')]                >> eq_x);
      eq_x     = cmp_x  >> *(root_node_d[str_p("==")|"!="]         >> cmp_x);
      cmp_x    = add_x  >> *(root_node_d[str_p("<=")|">="|'<'|'>'] >> add_x);
      add_x    = mlt_x  >> *(root_node_d[ch_p('+')|'-']            >> mlt_x);
      mlt_x    = neg_x  >> *(root_node_d[ch_p('*')|'/']            >> neg_x);
      neg_x    =           *root_node_d[ch_p('!')|ch_p('-')]       >> prime;


      prime   = sexpr | term;
      term    = token_node_d[lexeme_d[!ch_p('$') >> (alpha_p | '_')
                                      >> *(alnum_p | '_')]]
              | lexeme_d[real_p]
              | inner_node_d[ch_p('(') >> term >> ch_p(')')] | str_p("[]");
      
      ident   = scalar | name;
      scalar  = '$' >> name;
      // name    = 
      //+(anychar_p-'|'-'&'-'='-'!'-'<'-'>'-','-'['-']'-'+'-'-'-'*'-
      //      '/'-'('-')'-':'-'.'-space_p);
    }
    rule<Scanner> sexpr,range_x,list_x,def_x,lambda_x,arrow_x,seq;
    rule<Scanner> or_x,and_x,cons_x,eq_x,cmp_x,add_x,mlt_x,neg_x,prime,term;
    rule<typename lexeme_scanner<Scanner>::type> ident,scalar,name;
    const rule<Scanner>& start() const { return sexpr; }
  };
};
} //namespace

void parse(std::istream& in,sexpr& dst) {
  std::stringstream ss;
  util::indent2parens(in,ss);
  parse(ss.str(),dst);
}

void parse(const std::string& str,sexpr& dst) {
  if (str.empty()) {
    dst.clear();
    return;
  }

  dst=sexpr(string());
  const char* s=str.data();
  tree_parse_info<> t=ast_parse(s,s+str.length(),sexpr_grammar(),space_p);
  if (!t.match || !t.full || t.trees.size()!=1)
    throw std::runtime_error("bad tree structure parsing '"+str+"'");
  tosexpr(t.trees.front(),dst);
}
  
}} //namespace plap::lang
