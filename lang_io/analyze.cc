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
#include "builtin.h"
#include "operators.h"
#include "names.h"

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
make_exception(bad_decl_exists,
               "Bad declaration of function '"+str+"', which already exists.");
make_exception(bad_arg_name,"Bad name '"+str+"'");
make_exception(arg_shadow,"Argument '"+str+"' shadows existing argument.");
make_exception(arg_unbound,"Invalid reference to unbound argument '"+str+"'.");
make_exception(bad_pair,"Invalid pair '"+str+"' (arity must be >=2).");
make_exception(bad_symbol,"Unrecognized symbol or function '"+str+"'.");
make_exception(bad_redef,"Cannot redifine existing function '"+str+"'.");

#define process(name) \
  void process_ ## name(const_subsexpr src,subvtree dst)
#define special_case(name,arity)                                \
  assert(func2name(name ::  instance()));                       \
  if (src.root()==*func2name(name :: instance())) {             \
    validate_arity(src,arity_t(arity));                         \
    process_ ## name(src,dst);                                  \
    return;                                                     \
  }

//fixme -should be possible to write special case without taking arity argument

bool scalar(const string& s) { return (s[0]=='$' && s.size()>1); }
bool number(const string& s) { 
  char c=s[0];
  return (((c=='-' || c=='.') && s.size()>1) || (c>='0' && c<='9'));
}
bool identifier(const string& s) { return (!scalar(s) && !number(s)); }
bool boolean(const string& s) { 
  return (names_symbol(s) && (name2symbol(s)==true || name2symbol(s)==false));
}
bool character(const string& s) { return (s=="'"); }
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
    if (src.childless()) {
      dst.root()=string2arg(src.root());
      return;
    }

    special_case(lang_def,3);
    /**    special_case(lambda,2);
           special_case(let,variadic_arity);**/
    special_case(lang_decl,2);
    /**special_case(list,variadic_arity);
    special_case(pair,variadic_arity);
    **/
    if (func_t f=string2func(src.root())) {
      validate_arity(src,f);
      dst.root()=call(f);
      process_children(src,dst);
    } else { //see if its a scalar - if so, need to introduce an apply node
      dst.root()=call(lang_apply::instance());
      dst.append(string2scalar(src.root()));
      dst.append(vertex());
      process_list(src,dst.back_sub());
    }
  }

  process(children) {
    assert(!src.childless());
    dst.append(src.arity(),vertex());
    for_each(src.begin_sub_child(),src.end_sub_child(),dst.begin_sub_child(),
             bind(&semantic_analyzer::process_sexpr,this,_1,_2));
  }

  process(lang_def) { //def(name list(arg1 arg2 ...) body)
    //validate and set up arguments
    const string& name=sexpr2identifier(src[0]);

    if (src[1].root()!=*func2name(lang_list::instance()))
      throw_bad_def(name);
    arity_t a=src[1].arity();
    if (src[1].size()!=a+1u)
      throw_bad_def(name);
    for_each(src[1].begin_child(),src[1].end_child(),count_it(arg_idx),
             bind(&semantic_analyzer::index_scalar,this,_1,_2));
    arg_idx+=a;

    func_t f=name2func(name);
    if (f) {
      validate_arity(src[1],f);
      if (const vtree* v=f->body()) {
        if (!v->empty())
          throw_bad_redef(name);
      } else {
        throw_bad_redef(name);
      }
      make_def(src,f);
    } else {
      //a new function
      if (nested(src)) //error - defs must first be declared at global scope
        throw_undeclared_name(name);
      f=c.declare_func(a);
      name_func(f,sexpr2identifier(src[0]));
      try {
        make_def(src,f);
      } catch (std::runtime_error e) {
        erase_func_name(f);
        c.erase_last_func();
        throw e;
      }
    }
    arg_idx-=a;
    name_args(f,transform_it(src[1].begin_child(),&scalar_name),
              transform_it(src[1].end_child(),&scalar_name));
    for (sexpr::const_child_iterator i=src[1].begin_child();
         i!=src[1].end_child();++i)
      scalars.erase(scalar_name(*i));
    dst.root()=nil();
  }

  void make_def(const_subsexpr src,func_t f) {
    vtree body=vtree(vertex());
    process_sexpr(src[2],body);
    if (nested(src)) { //set to be created at runtime - def(func args body)
      assert(false); //fixme
#if 0
        dst.root()=id::def;
        dst.append(i->second);
        dst.append(id::list);
        dst.back_sub();//fixeme what do do with args???
        dst.splice(dst.end_child(),body);
#endif
    } else { //create it now
      c.define_func(body,f);
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

  process(lang_decl) { //decl(name arity)
    const string& name=sexpr2identifier(src[0]);
    if (name2func(name))
      throw_bad_decl_exists(name);
    name_func(c.declare_func(sexpr2arity(src[1])),name);
    dst.root()=nil();
  }

  template<typename Pred>
  void validate_range(sexpr::const_sub_child_iterator f,
                      sexpr::const_sub_child_iterator l,Pred p,string err) {
    for (++f;f!=l;++f)
      if (f->childless() && !p(f->root()))
        throw std::runtime_error("Expected a "+err+", got '"+f->root()+"'.");
  }

#define LANG_ANALYZE_check(predname,typename)                   \
  if (i->childless() && predname(i->root())) {                  \
    validate_range(i,src.end_child(),&predname,#typename);      \
    break;                                                      \
  }
  
  process(list) { //list(arg arg arg ...)
    dst.root()=call(lang_list::instance());
    for (sexpr::const_sub_child_iterator i=src.begin_sub_child();
         i!=src.end_sub_child();++i) {
      LANG_ANALYZE_check(boolean,bool);
      LANG_ANALYZE_check(character,char);
      LANG_ANALYZE_check(number,number);
      //fixme symbol
    }
    process_children(src,dst);
  }

  process(pair) { //pair(arg arg arg ...)
    arity_t a=src.arity();
    if (a==0)
      throw_bad_pair("empty");
    else if (a==1)
      throw_bad_pair(src.front());

    pair_rec(src.begin_sub_child(),src.end_sub_child(),dst);
  }
  //types = { symbol,char,number,func } there is a 4x4 matrix of possibilities
  void pair_rec(sexpr::const_sub_child_iterator f,
                sexpr::const_sub_child_iterator l,subvtree dst) {
#if 0
    const string& left=*f++;
    if (boost::next(f)==l) {
      const string& right=*f++;
      // if (number(left)) {
      //   if (number(right)


    switch(a) {
      case 0:
        dst.root()=nil();
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
#endif
    }
 
  //parsing an individual node always unambiguous (i.e. can be done without any
  //context other than the given context+bindings)
  vertex string2arg(const string& str) {
    if (func_t f=string2func(str))
      return arg(f);
    if (number(str))
      return number2vertex(str);
    if (scalar(str))
      return string2scalar(str);
    if (!names_symbol(str))
      throw_bad_symbol(str);
    return arg(name2symbol(str));
  }

  vertex number2vertex(const string& str) {
    try {
      return arg(lexical_cast<number_t>(str));
    } catch (...) {
      throw_bad_number(str);
    }
    assert(false);
  }

  void index_scalar(const string& s,arity_t idx) {
    if (!scalar(s))
      throw_bad_arg_name(s);
    if (!scalars.insert(make_pair(scalar_name(s),idx)).second)
      throw_arg_shadow(s);
  }

  //returns a func_t if available, else NULL
  func_t string2func(const string& str) {
#if 0
    bindings::const_iterator i=lets.find(str);
    if (i!=lets.end())
      return i->second;
#endif
    //fixme is this correct?
    return name2func(str);
  }

  //returns a scalar if available, else throws
  vertex string2scalar(const string& s) {
    if (!scalar(s))
      throw_bad_arg_name(s);
    scalar_map::const_iterator i=scalars.find(s.substr(1));
    if (i==scalars.end())
      throw_arg_unbound(s);
    return lang_arg(i->second);
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
      return lexical_cast<unsigned int>(src.root());
    } catch (...) {
      throw_bad_arity_decl(lexical_cast<string>(src));
    }
    return arity_t(0);
  }

  void validate_arity(const_subsexpr src,func_t f) {
    validate_arity(src,f->arity());
  }
  void validate_arity(const_subsexpr src,arity_t a) {
    if (a!=variadic_arity && src.arity()!=a)
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
