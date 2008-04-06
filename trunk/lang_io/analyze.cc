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
#include <fstream>
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
#include "io.h"
#include "parse.h"
#include "repl.h"
#include "checkpoint.h"
#include "pretty_print.h"

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
                           lexical_cast<string>((int)tgt)+" argument(s), got "+
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
make_exception(bad_arg_list,"Bad argument list '"+str+"'");
make_exception(arg_shadow,"Argument '"+str+"' shadows existing argument.");
make_exception(arg_unbound,"Invalid reference to unbound argument '"+str+"'.");
make_exception(bad_tuple,"Invalid tuple '"+str+"' (arity must be >=2).");
make_exception(bad_symbol,"Unrecognized symbol or function '"+str+"'.");
make_exception(bad_redef,"Cannot redifine existing function '"+str+"'.");
make_exception(bad_lambda,"Malformed lambda expression '"+str+
               "' - should be (lambda (arrow (arg1 arg2 ..) body)).");
make_exception(bad_import,"Malformed import '"+str+"'.");
make_exception(bad_import_file,"Could not open file '"+str+"' for reading.");
make_exception(bad_let,"Malformed let '"+str+"'.");
make_exception(nested_import,"Error processing directive '"+str+
               "'; imports must be at global scope.")

#define process(name) \
  void process_lang_ ## name(const_subsexpr src,subvtree dst)
#define special_case(name,arity)                                \
  if (src.root()==#name) {                                      \
    validate_arity(src,arity_t(arity));                         \
    process_lang_ ## name(src,dst);                             \
    return;                                                     \
  }

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
  semantic_analyzer(context& co) : c(co),arg_idx(0) {}
  context& c;
  arity_t arg_idx;

  typedef std::tr1::unordered_map<string,arity_t> scalar_map;
  scalar_map scalars;

  typedef std::tr1::unordered_map<string,util::slist<func_t> > let_map;
  let_map lets;
  
  bool nested(const_subsexpr src) { return parent(src.begin())!=src.end(); }

  void process_sexpr(const_subsexpr src,subvtree dst) {
    const string& root=src.root();

    special_case(def,3);
    special_case(decl,2);
    special_case(import,1);
    special_case(lambda,1);
    special_case(let,2);
    special_case(tuple,variadic_arity);
    special_case(list,variadic_arity);

    if (src.childless()) {
      dst.root()=string2arg(root);
      return;
    }

    if (func_t f=string2func(root)) {
      validate_arity(src,f);
      dst.root()=call(f);
      process_children(src,dst);
    } else if (root==char_symbol) { //char literal
      assert(src.arity()==1 && src.front().length()==1);
      dst.root()=arg(src.front()[0]);
    } else if (root==string_symbol) { //string literal
      if (src.childless()) {
        dst.root()=nil();
      } else {
        assert(src.size()==src.arity()+1u);
        dst.root()=call(lang_list::instance());
        foreach(const string& s,children(src)) {
          assert(s.length()==1);
          dst.append(arg(s[0]));
        }
      }        
    } else { //see if its a scalar - if so, need to introduce an apply node
      dst.root()=call(lang_apply::instance());
      dst.append(string2scalar(root));
      dst.append(vertex());
      if (src.arity()==1)
        process_sexpr(src.front_sub(),dst.back_sub());
      else
        process_lang_tuple(src,dst.back_sub());
    }
  }

  void process_children(const_subsexpr src,subvtree dst) {
    assert(!src.childless());
    dst.append(src.arity(),vertex());
    for_each(src.begin_sub_child(),src.end_sub_child(),dst.begin_sub_child(),
             bind(&semantic_analyzer::process_sexpr,this,_1,_2));
  }

  process(import) {
    if (nested(src))
      throw_nested_import(lexical_cast<string>(src));
    process_sexpr(src[0],dst);
    if (dst.root()==call(lang_list::instance())) {
      foreach(subvtree s,sub_children(dst))
        if (!s.childless() || !is_char(s.root()))
          throw_bad_import(lexical_cast<string>(src[0]));
      std::ifstream in(string(list_of<char>(dst).begin(),
                              list_of<char>(dst).end()).c_str());
      if (!in.good())
        throw_bad_import_file(string(list_of<char>(dst).begin(),
                                     list_of<char>(dst).end()));
      dst.prune();
      dst.root()=nil();
      load_lib(in,c);
    } else {
      throw_bad_import(lexical_cast<string>(src[0]));
    }
  }

  process(def) { //def(name list(arg1 arg2 ...) body)
    //validate and set up arguments
    const string& name=sexpr2identifier(src[0]);
    func_t f=string2func(name);
    if (f) {
      if (const vtree* v=f->body()) {
        if (!v->empty())
          throw_bad_redef(name);
      } else {
        throw_bad_redef(name);
      }
      make_def(src,src[1],src[2],f);
    } else {
      //a new function
      if (nested(src)) //error - defs must first be declared at global scope
        throw_undeclared_name(name);
      lang_ident* d=c.declare(src[1].arity(),arg_idx);
      name_func(d,sexpr2identifier(src[0]));
      try {
        make_def(src,src[1],src[2],d);
      } catch (std::runtime_error e) {
        erase_func_name(d);
        c.erase_last_decl();
        throw e;
      }
    }
    dst.root()=nil();
  }

  void make_def(const_subsexpr root,const_subsexpr args,const_subsexpr src,
                func_t f) {
    assert(dynamic_cast<const lang_ident*>(f));
    lang_ident* d=static_cast<lang_ident*>(const_cast<func*>(f));

    //validate and index arguments
    validate_arity(args,f->arity());
    if (args.root()!=*func2name(lang_list::instance()) || 
        !(args.flat() || args.childless()))
      throw_bad_arg_list(lexical_cast<string>(args));
    for_each(args.begin_child(),args.end_child(),count_it(arg_idx),
             bind(&semantic_analyzer::index_scalar,this,_1,_2));

    arg_idx+=f->arity();
    vtree body=vtree(vertex());
    process_sexpr(src,body);
    arg_idx-=f->arity();

    name_args(f,transform_it(args.begin_child(),&scalar_name),
              transform_it(args.end_child(),&scalar_name));
    foreach (const string& s,children(args)) 
      scalars.erase(scalar_name(s));

    c.define(d,body);
  }

  process(lambda) { //lambda(arrow(list(args),body))
    if (src.front()!=arrow_name)
      throw_bad_lambda(lexical_cast<string>(src));
    func_t f=c.declare(src[0][0].arity(),arg_idx);
    try {
      make_def(src,src[0][0],src[0][1],f);
    } catch (std::runtime_error e) {
      c.erase_last_decl();
      throw e;
    }
    dst.root()=arg(f);
  }

  process(let) { //let(list(def1 ...) body)
    if (src.front()!=*func2name(lang_list::instance()) || 
        src.front_sub().childless())
      throw_bad_let(src.root());
    foreach (string s,children(src[0]))
      if (s!=def_name)
        throw_bad_let(src.root());
    
    //create the identifiers
    foreach (const_subsexpr s,sub_children(src[0])) {
      if (string2func(s.front()))
        throw_arg_shadow(s.front());
    }
    std::vector<func_t> idents;
    foreach (const_subsexpr s,sub_children(src[0])) {    
      idents.push_back(c.declare(s[1].arity(),arg_idx));
      lets[s.front()].push_front(idents.back());
      name_let(idents.back(),s.front());
    }

    //bind them and analyze the body of the let
    try {
      std::vector<func_t>::iterator ident=idents.begin();
      foreach(const_subsexpr s,sub_children(src.front_sub()))
        make_def(s,s[1],s[2],*ident++);
      process_sexpr(src[1],dst);
    } catch (std::runtime_error e) {
      foreach(func_t f,idents)
        erase_let_name(f);
      dorepeat(idents.size())
        c.erase_last_decl();
      throw e;
    }

    //cleanup
    foreach(const_subsexpr s,sub_children(src[0])) {
      lets[s.front()].pop_front();
    }
  }

  process(decl) { //decl(name arity)
    const string& name=sexpr2identifier(src[0]);
    if (string2func(name))
      throw_bad_decl_exists(name);
    name_func(c.declare(sexpr2arity(src[1]),0),name);
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
    if (src.childless())
      throw std::runtime_error("Bad list - must take arguments.");
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

  //a tuple with <2 constituents is not allowed
  process(tuple) { //tuple(arg arg arg ...)
    arity_t a=src.arity();
    if (a==0)
      throw_bad_tuple("empty");
    else if (a==1)
      throw_bad_tuple(src.front());
    dst.root()=call(lang_tuple::instance());
    process_children(src,dst);
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
    let_map::const_iterator i=lets.find(str);
    if (i!=lets.end() && !i->second.empty())
      return i->second.front();
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
      throw_bad_arity(src.root()==*func2name(lang_list::instance()) ? 
                      parent(src.begin_sub())->front() : 
                      src.root(),src.arity(),a);
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

} //namespace


void analyze(const_subsexpr src,lang::subvtree dst,lang::context& c) {
  semantic_analyzer sa(c);
  sa.process_sexpr(src,dst);
}

}} //namespace plap::lang_io
