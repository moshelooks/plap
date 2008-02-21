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

#include "analyze.h"
#include <map>
#include <boost/lexical_cast.hpp>
#include "iterator_shorthands.h"
#include "dorepeat.h"
#include "algorithm.h"
#include "foreach.h"
#include "tree_io.h"
#include "context.h"
#include "core.h"
#include "operators.h"

#include "algorithm.h"
#include "tree_iterator.h"

namespace plap { namespace lang_io {

using namespace lang;
using namespace util;
using boost::lexical_cast;
using boost::bind;
using std::string;
using std::stringstream;

namespace {

void throw_bad_arity(const string& name,arity_t actual, arity_t tgt) {
  throw std::runtime_error(string("Bad arity for ")+name+" - expected "+
                           lexical_cast<string>((int)tgt)+", got "+
                           lexical_cast<string>((int)actual)+".");
}

#define make_exception(nm,dsc)                   \
  void throw_ ## nm(const string& str) {         \
    throw std::runtime_error(dsc);               \
  }
make_exception(undeclared_name,"Bad nested definition of '"+str+
               "' - name must be declared first at global scope.")
make_exception(bad_identifier,"Bad identifier '"+str+"'.")
make_exception(bad_number,"Expected a number, got '"+str+"'.")
make_exception
    (bad_arity_decl,
     "Arity in declarion must be a positive integer literal - instead got '"+
     str+"'.");
make_exception
    (bad_args,
     "Bad argument list '"+str+"' - should be unbound scalars (e.g.$foo).");
make_exception(unbound_scalar,"Unbound scalar '"+str+"'.");
make_exception(bad_def,
               "Bad definition of function '"+str+"'");
make_exception(bad_arg_name,
               "Bad argument name '"+str+"'");

#define process(name) \
  void process_ ## name(const_subsexpr src,subvtree dst)
#define special_case(name)                       \
  if (src.root()==name ## _name) {               \
    process_ ## name(src,dst);                   \
    return;                                      \
  }

struct semantic_analyzer {
  semantic_analyzer(context& co,const_subsexpr r) : c(co),root(r) {}
  context& c;
  const_subsexpr root;
  
  process(sexpr) {
    if (src.childless())
      process_leaf(src,dst);
    else
      process_internal(src,dst);
  }

  process(leaf) { 
    if (dst==root) //avoid creating a singleton leaf that's not a func_t
      if (func_t f=string2func(src.root())) {
        dst.root()=f;
      } else {
        dst.append(string2vertex(src.root()));
          c.to_tuple(dst);
      }
    else
      dst.root()=string2vertex(src.root());
  }

  process(internal) {
    special_case(def);
    special_case(lambda);
    special_case(let);
    special_case(decl);

    if (func_t f=string2func(src.root())) {
      validate_arity(src,f);
      dst.root()=f;
      process_children(src,dst);
    } else { //see if its a scalar - if so, need to introduce an apply node
      dst.append(string2scalar(src.root()));
      dst.append(vertex());
      process_children(src,dst.back_sub());
      c.to_list(dst.back_sub());
      c.to_apply(dst);
    }
  }

  process(children) {
    dst.append(src.arity(),vertex());
    for_each(src.begin_sub_child(),src.end_sub_child(),dst.begin_sub_child(),
             bind(&semantic_analyzer::process_sexpr,this,_1,_2));
  }

  process(def) { //def(name list(arg1 arg2 ...) body)
    //validate and set up arguments
    const string& name=sexpr2identifier(src[0]);
    if (src[1].root()!=list_name) 
      throw_bad_def(name);/**
    for (const_subsexpr::child_sub_iterator i=src[1].begin_sub_child();
         i!=src[1].end_sub_child();++i) {
      if (!i->childless())
        throw_bad_def(name);
      if (i->root()[0]!='$')
        throw_bad_arg_name(i->root());
        bindings[i->root()**/
    
    //fixmereplace_args(src[1],src[2]);
    vtree body=vtree(vertex());
    process_sexpr(src[2],body);

    if (func_t f=c.name2func(name)) { //an already-declared function?
      validate_arity(src[1],f);
      if (nested(src)) { //set to be created at runtime - def(func args body)
#if 0
        dst.root()=id::def;
        dst.append(i->second);
        dst.append(id::list);
        dst.back_sub();//fixeme what do do with args???
        dst.splice(dst.end_child(),body);
#endif
      } else { //create it now and return unit
        c.define_func(src[1].begin_child(),src[1].end_child(),
                        body,f);
        dst.root()=id::unit;
      }
    } else { //a previously undeclared function
      if (nested(src)) //error - defs must first be declared at global scope
        throw_undeclared_name(name);
      //otherwise, create a new function and return unit
      c.define_func(src[1].begin_child(),src[1].end_child(),body,name);
      dst.root()=id::unit;
    }
  }
  
  process(lambda) { //lambda(args body)
#if 0
    //fixme - create a closure if variables are included
    argument_list args=bind_arguments(src[0],scalars); //fixme
    vtree body(vertex());
    process_sexpr(src[1],body);
    scalars.pop(args); //fixme
    dst.root()=c.create_func(args,body);
#endif
  }

  process(let) { //let(list(def1 ...) body)
#if 0
    argu..;//fixme
#endif
  }

  process(decl) { //decl(name arity)
    c.declare_func(sexpr2arity(src[1]),sexpr2identifier(src[0]));
  }
 
  //parsing an individual node always unambiguous (i.e. can be done without any
  //context other than the given context+bindings)
  //throws if invalid
  vertex string2vertex(const string& str) {
    assert(!str.empty());
    if (func_t f=string2func(str))
      return f;
    char c=str[0];
    if (c=='-' || c=='.' || (c>='0' && c<='9')) {
      try {
        return lexical_cast<contin_t>(c);
      } catch (...) {
        throw_bad_number(str);
      }
    }
    return string2scalar(str);
  }

  //returns a func_t if available, else NULL
  func_t string2func(const string& str) {
#if 0
    bindings::const_iterator i=lets.find(str);
    if (i!=lets.end())
      return i->second;
#endif
    if (func_t f=c.name2func(str))
      return f;
    return NULL;
  }

  //returns a scalar if available, else throws
  vertex string2scalar(const string& str) {
    if (str[0]!='#')
      throw_bad_identifier(str);
    assert(str.size()>1u);
    return arg(arity_t(str[1]));
  }

  const string& sexpr2identifier(const_subsexpr src) {
    if (!src.childless())
      throw_bad_identifier(lexical_cast<string>(src));
    return src.root();
  }

  lang::arity_t sexpr2arity(const_subsexpr src) {
    if (!src.childless())
      throw_bad_arity_decl(lexical_cast<string>(src));
    try {
      return lexical_cast<arity_t>(src.root());
    } catch (...) {
      throw_bad_arity_decl(lexical_cast<string>(src));
    }
    return arity_t(0);
  }

  void validate_arity(const_subsexpr src,func_t f) {
    if (src.arity()!=f->arity())
      if (const string* name=c.func2name(f))
        throw_bad_arity(*name,src.arity(),f->arity());
      else
        throw_bad_arity("anonymous function",src.arity(),f->arity());
  }
  
  /**  void bind_arguments(const_subsexpr src,bindings& scalars) {

       }**/

  bool nested(const_subsexpr src) { return src.begin()!=root.begin(); }

  bool scalar(const string& s) { return (s[0]=='$'); }

  void replace_args(const_subsexpr args,subsexpr body) {
    if (!args.flat())
      throw_bad_args(lexical_cast<string>(args));
    std::map<string,arity_t> arg2idx(pair_it(args.begin_child(),count_it(0)),
                                     pair_it(args.end_child(),count_it(-1)));
    if (!scalar(arg2idx.begin()->first) || !scalar(arg2idx.rbegin()->first))
      throw_bad_args(lexical_cast<string>(args));

    foreach (string& s,body) {
      if (s[0]=='#')
        throw_bad_identifier(s);
      if (scalar(s)) {
        std::map<string,arity_t>::const_iterator i=arg2idx.find(s);
        if (i==arg2idx.end())
          throw_unbound_scalar(s);
        s[0]='#';
        s[1]=char(i->second);
      }
    }
  }
};

        
  
  /**
  string lastline,s;
  string::size_type indent=0;
  std::stack<string::size_type> oldindents;
  for (string s=process_line(in,s);!s.empty();process_line(in,s)) {
    string::size_type newindent=count_whitespace(s);
    if (newindent>indent)
      oldindents.push(indent);
    if (newindent>indent || has_whitespace(lastline))
      out << lparen;
    out << lastline << " ";
    while (newindent<indent) {
      if (oldindents.empty())
        throw_bad_indent(s);
      out << rparen;
      indent=oldindents.top();
      oldindents.pop();
    }
    bool has=has_whitespace(s,newindent);
    if (has)
      out << lparen;
      out << s;
      if (has)
        out << rparen;
    }    
    if (!is_whitespace(in.peek()))
      break;
    indent=newindent;
  }
finalize:
  if (!oldindents.empty())
    throw_bad_indent(s);
  **/

} //namespace


void analyze(const_subsexpr src,lang::subvtree dst,lang::context& c) {
  semantic_analyzer sa(c,src);
  sa.process_sexpr(src,dst);
}

}} //namespace plap::lang_io
