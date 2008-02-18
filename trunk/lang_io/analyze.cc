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
#include "environment.h"
#include "core.h"


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

#define process(name) \
  void process_ ## name(subsexpr src,subvtree dst)
#define special_case(name)                       \
    if (f==id::name) {                           \
      process_ ## name(src,dst);                 \
      return;                                    \
    }

struct semantic_analyzer {
  semantic_analyzer(environment& e,const_subsexpr r) : env(e),root(r) {}
  environment& env;
  const_subsexpr root;

  process(sexpr) {
    if (src.childless())
      process_leaf(src,dst);
    else
      process_internal(src,dst);
  }

  process(leaf) { dst.root()=string2vertex(src.root()); }

  process(internal) {
    if (func_t f=string2func(src.root())) {
      validate_arity(src,f);

      special_case(def);
      special_case(lambda);
      special_case(let);
      special_case(decl);

      dst.root()=f;
      process_children(src,dst);
    } else { //see if its a scalar - if so, need to introduce an apply node
      dst.root()=id::apply;
      dst.append(string2scalar(src.root()));
      dst.append(vertex(id::list));
      process_children(src,dst.back_sub());
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
    replace_args(src[1],src[2]);
    vtree body=vtree(vertex());
    process_sexpr(src[2],body);

    if (func_t f=env.name2func(name)) { //an already-declared function?
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
        env.define_func(src[1].begin_child(),src[1].end_child(),
                        body,f);
        dst.root()=id::unit;
      }
    } else { //a previously undeclared function
      if (nested(src)) //error - defs must first be declared at global scope
        throw_undeclared_name(name);
      //otherwise, create a new function and return unit
      env.define_func(src[1].begin_child(),src[1].end_child(),body,name);
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
    dst.root()=env.create_func(args,body);
#endif
  }

  process(let) { //let(list(def1 ...) body)
#if 0
    argu..;//fixme
#endif
  }

  process(decl) { //decl(name arity)
    env.declare_func(sexpr2arity(src[1]),sexpr2identifier(src[0]));
  }
 
  //parsing an individual node always unambiguous (i.e. can be done without any
  //context other than the given environment+bindings)
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
    if (func_t f=env.name2func(str))
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
      if (const string* name=env.func2name(f))
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



/*
void sexpr2vtree(const_subsexpr src,subvtree dst,environment& env)
    throw(std::runtime_error) {
  sexpr tmp(src);
  semantic_analyzer se(env,tmp);
  se.process_sexpr(tmp,dst);
}

void stream2vtree(std::istream& in,lang::subvtree dst,lang::environment& env,
                  bool interactive) throw(std::runtime_error) {

}

void string2vtree(const string& str,lang::subvtree dst,
                  lang::environment& env) throw(std::runtime_error) {
  stringstream ss;
  ss << str;
  stream2vtree(ss,dst,env);
}
*/


void string2sexpr(const std::string& str,tree<std::string>& dst)
    throw(std::runtime_error) {

}

} //namespace


void analyze(const_subsexpr src,lang::subvtree dst,lang::environment& env) {

}

}} //namespace plap::lang_io
