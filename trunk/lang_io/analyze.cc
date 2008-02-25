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
#include <tr1/unordered_map>
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

#include <iostream>
using namespace std;//fixme

namespace plap { namespace lang_io {

namespace {
using namespace lang;
using namespace util;
using boost::lexical_cast;
using boost::bind;
using std::string;
using std::stringstream;

void throw_bad_arity(const string& name,arity_t actual, arity_t tgt) {
  throw std::runtime_error(string("Bad arity for ")+name+" - expected "+
                           lexical_cast<string>((int)tgt)+" arguments, got "+
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
make_exception(arg_shadow,"Argument '"+str+"' shadows existing argument.");
make_exception(arg_unbound,"Invalid reference to unbound argument '"+str+"'.");
make_exception(arity_exceeded,"Maximum allowed arity "+
               lexical_cast<string>(LANG_LIMIT_ARITY)+" exceeded by "+str+".");

#define process(name) \
  void process_ ## name(const_subsexpr src,subvtree dst)
#define special_case(name,arity)                 \
  if (src.root()==name ## _name) {               \
    validate_arity(src,arity_t(arity));          \
    process_ ## name(src,dst);                   \
    return;                                      \
  }

bool scalar(const string& s) { return (s[0]=='$' && s.size()>1); }
bool number(const string& s) { 
  char c=s[0];
  return ((c=='-' || c=='.' || (c>='0' && c<='9')) && s.size()>1);
}
bool identifier(const string& s) { return (!scalar(s) && !number(s)); }
string scalar_name(const string& s) { return s.substr(1); }

struct semantic_analyzer {
  semantic_analyzer(context& co,const_subsexpr r) : c(co),root(r),arg_idx(0) {}
  context& c;
  const_subsexpr root;
  arity_t arg_idx;

  typedef std::tr1::unordered_map<string,arity_t> scalar_map;
  scalar_map scalars;

  bool nested(const_subsexpr src) { return src.begin()!=root.begin(); }

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
      } else if (scalar(src.root())) {
        sexpr tmp(tuple_name);
        tmp.append(src.root());
        process_internal(tmp,dst);

      }
    else
      dst.root()=string2vertex(src.root());
  }

  process(internal) {
    special_case(def,3);
    special_case(lambda,2);
    special_case(let,0);
    special_case(decl,2);
    special_case(tuple,0);

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

  void index_scalar(const std::string& s,arity_t idx) {
    if (!scalar(s))
      throw_bad_arg_name(s);
    if (!scalars.insert(make_pair(scalar_name(s),idx)).second)
      throw_arg_shadow(s);
  }

  process(def) { //def(name list(arg1 arg2 ...) body)
    //validate and set up arguments
    const string& name=sexpr2identifier(src[0]);

    if (src[1].root()!=list_name)
      throw_bad_def(name);
    arity_t a=src[1].arity();
    if (src[1].size()!=a+1u)
      throw_bad_def(name);
    for_each(src[1].begin_child(),src[1].end_child(),count_it(arg_idx),
             bind(&semantic_analyzer::index_scalar,this,_1,_2));
    arg_idx+=a;

    vtree body=vtree(vertex());
    process_sexpr(src[2],body);

    if (func_t f=c.name2func(name)) { //an already-declared function?
      assert(false && f); //fixme
#if 0
      validate_arity(src[1],f);
      if (nested(src)) { //set to be created at runtime - def(func args body)
        dst.root()=id::def;
        dst.append(i->second);
        dst.append(id::list);
        dst.back_sub();//fixeme what do do with args???
        dst.splice(dst.end_child(),body);
      } else { //create it now and return unit
        c.define_func(src[1].begin_child(),src[1].end_child(),
                        body,f);
        //fixmedst.root()=tuple0::instance();
      }
#endif
    } else {
      //a new function
      if (nested(src)) //error - defs must first be declared at global scope
        throw_undeclared_name(name);
      //otherwise, create a new function and return unit
      c.define_func(transform_it(src[1].begin_child(),&scalar_name),
                    transform_it(src[1].end_child(),&scalar_name),
                    body,sexpr2identifier(src[0]));
      dst.root()=tuple0::instance();
    }

    arg_idx-=a;
    for (sexpr::const_child_iterator i=src[1].begin_child();
         i!=src[1].end_child();++i)
      scalars.erase(scalar_name(*i));
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
    if (func_t f=string2func(str))
      return f;
    if (number(str)) {
      try {
        return lexical_cast<contin_t>(str);
      } catch (...) {
        throw_bad_number(str);
      }
    }
    return string2scalar(str);
  }

  process(tuple) { //tuple(arg arg arg ...)
    arity_t a=src.arity();
    process_children(src,dst);
    switch(a) {
      case 0:
        dst.root()=c.get_type<tuple0>();
      case 1:
        //sexpr::sub_child_iterator arg0=src.begin_sub_child();
        //dst.root()=c.get_type<tuple1<number_t> >();
        //break;
      default:
        throw_arity_exceeded(src.root()+"^"+lexical_cast<string>(a));
    }
    /***
        fixme tuple
        dst.append(string2vertex(src.root()));
        c.to_tuple(dst);
    **/



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
  vertex string2scalar(const string& s) {
    if (!scalar(s))
      throw_bad_arg_name(s);
    scalar_map::const_iterator i=scalars.find(s.substr(1));
    if (i==scalars.end())
      throw_arg_unbound(s);
    return arg(i->second);
  }

  const string& sexpr2identifier(const_subsexpr src) {
    if (!src.childless() || !identifier(src.root()))
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
    validate_arity(src,f->arity());
  }
  void validate_arity(const_subsexpr src,arity_t a) {
    if (a!=0 && src.arity()!=a)
      throw_bad_arity(src.root(),src.arity(),a);
  }

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
