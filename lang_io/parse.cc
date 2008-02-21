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
#include <boost/spirit/utility/confix.hpp>
#include "tree.h"
#include "operators.h"
#include "indent.h"

#include <iostream>
#include "tree_io.h"
using namespace std;

namespace plap { namespace lang_io {

namespace {
using namespace boost::spirit;
using std::string;

void tosexpr(const tree_node<node_val_data<> >& s,subsexpr d);

inline string tostr(const tree_node<node_val_data<> >& s,int idx=-1) {
  if (idx==-1)
    return string(s.value.begin(),s.value.end());
  return string(s.children[idx].value.begin(),s.children[idx].value.end());
}
void sexpr_rec(const tree_node<node_val_data<> >& s,subsexpr d,int n=0) {
  d.append(s.children.size()-n,string());
  for_each(s.children.begin()+n,s.children.end(),d.begin_sub_child(),&tosexpr);
}

void tosexpr(const tree_node<node_val_data<> >& s,subsexpr d) {
  sexpr_rec(s,d);

  string name=tostr(s);
  if (name==")") {
    assert(!d.childless());
    if (++d.begin_child()==d.end_child()) {
      std::swap(d.root(),d.front());
      d.erase(d.flatten(d.begin_child()));
    }
  } else if (name=="") {
    if (d.front_sub().childless()) {
      std::swap(d.root(),d.front());
      d.erase(d.begin_child());
      /* } else if (++d.begin_child()==d.end_child()) {
      std::swap(d.root(),d.front());
      d.erase(d.flatten(d.begin_child()));*/
    } else {
      assert(d.arity()>1);
      d.root()=apply_name;
      d.insert(d[1],list_name);
      d.splice(d[1].begin_child(),d[2].begin(),d.end_child());
    }
  } else if (name==def_symbol) {
      d.prepend(d[0].root());
      d[1].root()=list_name;
      d.root()=def_name;
  } else {
    d.root()=symbol2name(name,s.children.size());
  }



  /* if (name=="(")
    name="lparen";
  if (name==")")
    name="rparen";
  if (name=="")
    name="blank";
    d.root()=name;
  if (name=="" || name==")") {
    assert(!d.childless());
    if (d.front_sub().childless()) {
      std::swap(d.root(),d.front());
      d.erase(d.begin_child());
    }
  } else {
    string::size_type arity=s.children.size();
    if (name==def_symbol) {
      arity=3;
      d.prepend(d[0].root());
      d[1].root()=list_name;
    }
    d.root()=symbol2name(name,arity);
    }*/


  /*  if (!d.childless() && d.back_sub().root()==")") {
    assert(d.back_sub().childless());
    d.erase(d.back_sub());
  }

  string name=tostr(s);
  string::size_type arity=s.children.size();
  if (name=="") {
    assert(!d.childless());
    if (d.front_sub().childless()) {
      std::swap(d.root(),d.front());
      d.erase(d.begin_child());
    } else if (++d.begin_child()==d.end_child()) {
      std::swap(d.root(),d.front());
      d.erase(d.flatten(d.begin_child()));
    } else {
      d.root()=apply_name;
    }
  } else if (name==apply_symbol) {
    if (d.front_sub().childless()) {
      std::swap(d.root(),d.front());
      d.erase(d.begin_child());
      d.flatten(d.begin_child());
    } else {
      d.root()=apply_name;
      d.insert_above(d[1],list_name);
      d.flatten(d[1][0]);
    }
  } else { 
    if (name==def_symbol) {
      arity=3;
      d.prepend(d[0].root());
      d[1].root()=list_name;
    }
    d.root()=symbol2name(name,arity);
    }*/


  /* if (name==apply_symbol && d.front_sub().childless()) {
    std::swap(d.root(),d.front());
    d.erase(d.begin_child());
    d.flatten(d.begin_child());
    } else*/ 

    /*
  if (name=="" && s.children[0].children.empty()) {
    name=tostr(s,0);
    sexpr_rec(s,d,1);
  } else {
    sexpr_rec(s,d);
  }

  if (name==apply_symbol && d.front_sub().childless()) {
    std::swap(d.root(),d.front());
    d.erase(d.begin_child());
    d.flatten(d.begin_child());
  } else {
    if (name==def_symbol) {
      arity=3;
      d.prepend(d[0].root());
      d[1].root()=list_name;
    } else if (name==apply_symbol) {
      if (d[1].root()=="") {
        d[1].root()=list_name;
      } else {
        d.insert_above(d[1],list_name);
        d.flatten(d[1][0]);
      }
      }*/
    //    d.root()=symbol2name(name,arity);
    //}
  //d.root()=name;
}
struct sexpr_grammar : public grammar<sexpr_grammar> {
  template<typename Scanner>
  struct definition {
    definition(const sexpr_grammar&) {
      sexpr  = no_node_d[ch_p('(')] >> *list >> root_node_d[ch_p(')')];
      /*          >> !(root_node_d[ch_p('(')] >> list >> 
               lexeme_d[no_node_d[ch_p(')')] >> eps_p(space_p | '(')])
          >> list //% ch_p(','))//fixme tuples
          >> ch_p(')');*/

      /*          >> !(root_node_d[ch_p('(')] >> list >> 
               lexeme_d[no_node_d[ch_p(')')] >> eps_p(space_p | '(')])
          >> list //% ch_p(','))//fixme tuples
          >> ch_p(')');*/


      list   = range    |  listh;
      range  = def      |  rangeh;

      def    = lambda   >> !(root_node_d[ch_p('=')] >>
                             eps_p(~ch_p('=') >> *anychar_p)       >> lambda);
      lambda = fact     |  lambdah;
      fact   =             ! root_node_d[str_p("<-")]              >> arrow;
      arrow  = seq      >> *(root_node_d[str_p("->")]              >> lambda);
      seq    = or_op    >> *(lambdah|or_op);
      or_op  = and_op   >> *(root_node_d[str_p("||")]              >> and_op);
      and_op = cons     >> *(root_node_d[str_p("&&")]              >> cons);
      cons   = eq       >> *(root_node_d[ch_p(':')]                >> eq);
      eq     = cmp      >> *(root_node_d[str_p("==")|"!="]         >> cmp);
      cmp    = add      >> *(root_node_d[str_p("<=")|">="|'<'|'>'] >> add);
      add    = cat      >> *(root_node_d[ch_p('+')|'-']            >> cat);
      cat    = mlt      >> *(root_node_d[ch_p('~')]                >> mlt);
      mlt    = neg      >> *(root_node_d[ch_p('*')|'/']            >> neg);
      neg    =             ! root_node_d[ch_p('!')|ch_p('-')]      >> prime;

      prime  = sexpr | term | listh | rangeh;
      term   = inner_node_d[ch_p('(') >> term >> ch_p(')')] | "[]" | str | chr
             | lexeme_d[token_node_d
                        [!ch_p('$') >> (alpha_p | '_') >> *(alnum_p | '_') 
                         >> ~eps_p(('.' >> ~ch_p('.')) | alnum_p | '$')]
                 | (real_p >> ~eps_p('.' | alnum_p | '$'))];
      str    = lexeme_d[root_node_d[ch_p('\"')] 
                        >> *((ch_p('\\') >> '\"') | anychar_p - '\"')
                        >> no_node_d[ch_p('\"')]];
      chr    = lexeme_d['\'' >> !ch_p('\\') >> anychar_p 
                        >> no_node_d[ch_p('\'')]];

      listh  = (root_node_d[ch_p('[')] >> (list % no_node_d[ch_p(',')])
                >> no_node_d[ch_p(']')]);
      rangeh = (no_node_d[ch_p('[')] >> (int_p|seq|sexpr) 
                >> root_node_d[str_p("...")|".."]
                >> (int_p|seq|sexpr) >> no_node_d[ch_p(']')]);
      lambdah= root_node_d[ch_p('\\')] >> arrow;
    }
    rule<Scanner> sexpr,list,range,def,lambda,fact,arrow,seq;
    rule<Scanner> or_op,and_op,cons,eq,cmp,add,cat,mlt,neg;
    rule<Scanner> prime,term,str,chr,listh,rangeh,lambdah;
    const rule<Scanner>& start() const { return sexpr; }
  };
};
} //namespace

bool indent_parse(std::istream& in,sexpr& dst) {
  std::stringstream ss;
  util::indent2parens(in,ss);
  return paren_parse(ss.str(),dst);
}

bool paren_parse(const std::string& str,sexpr& dst) {
  if (str.empty()) {
    dst.clear();
    return false;
  }

  //  if (str.size()>3 && str.substr(str.size()-2)==".)") //a fact

  dst=sexpr(string());
  const char* s=str.data();
  tree_parse_info<> t=ast_parse(s,s+str.length(),sexpr_grammar(),space_p);
  if (!t.match || !t.full || t.trees.size()!=1)
    throw std::runtime_error("bad tree structure parsing '"+str+"'");
  tosexpr(t.trees.front(),dst);
  return true;
}
  
}} //namespace plap::lang
