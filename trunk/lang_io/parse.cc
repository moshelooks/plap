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
#include <boost/lexical_cast.hpp>
#include "algorithm.h"
#include "tree_io.h"
#include "environment.h"
#include "core.h"

namespace plap { namespace lang_io {

using namespace lang;
using namespace util;


void throw_bad_arity(const std::string& name,arity_t actual, arity_t tgt) {
  throw std::runtime_error(std::string("Bad arity for ")+name+" - expected "+
                           boost::lexical_cast<std::string>(tgt)+", got "+
                           boost::lexical_cast<std::string>(actual)+".");
}

#define make_exception(nm,dsc)                                          \
  void throw_ ## nm(const std::string& str) {                           \
    throw std::runtime_error(dsc);                                      \
  }
make_exception(undeclared_name,"Bad nested definition of '"+str+
               "' - name must be declared first at global scope.")
make_exception(bad_identifier,"Bad identifier '"+str+"'.")
make_exception(bad_number,"Expected a number, got '"+str+"'.")
make_exception
    (bad_arity_decl,
     "Arity in declarion must be a positive integer literal - instead got '"+
     str+"'.");

std::istream& stream2sexpr(std::istream& in,sexpr& dst) {
  //fixme - add a stringstream buffer that handles syntactic sugar
  return in >> dst;
}

#define process(name) \
  void process_ ## name(const_sub_sexpr src,vsubtree dst,bool nest=true)
#define special_case(name)                       \
    if (f==id::name) {                           \
      process_ ## name(src,dst,nest);            \
      return;                                    \
    }

struct semantic_analyzer {
  semantic_analyzer(environment& e) : env(e) {}
  environment& env;
  //bindings scalars,lets;

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
    //for_each(src.begin_sub_child(),src.end_sub_child(),dst.begin_sub_child(),
    //fixme       boost::bind(&semantic_analyzer::process_sexpr,this,_1,_2));
  }

  process(def) { //def(name list(arg1 arg2 ...) body)
#if 0
    //validate and set up arguments
    const std::string& name=sexpr2identifier(src[0]);
    argument_list args=bind_arguments(src[1],scalars);
    vtree body(vertex());
    process_sexpr(src[2],body);
    scalars.pop(args);

    if (func_t* f=env.name2func(name)) { //an already-declared function?
      validate_arity(src[1],f);
      if (!def_match(i->second->args(),args))//fixme
        throw_bad_arity(name,i->second->args(),args);//fixme
      if (nest) { //set it up to be created at runtime - def(func args body)
        dst.root()=id::def;
        dst.append(i->second);
        dst.append(id::list);
        dst.back_sub();//fixeme what do do with args???
        dst.splice(dst.end_child(),body);
      } else { //create it now and return unit
        env.extend_func(i->second,args,body);
        dst.root()=id::unit;
      }
    } else { //a previously undeclared function
      if (nest) //error - defs must first be declared at global scope
        throw_undeclared_name(name);
      //otherwise, create a new function and return unit
      env.create_func(name,args,body);
      dst.root()=id::unit;
    }
#endif
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

  process(let) { //let(args body) - where each arg is an def
#if 0
    argu..;//fixme
#endif
  }

  process(decl) { //decl(name arity)
    env.create_func(sexpr2arity(src[1]),sexpr2identifier(src[0]));
  }
 
  //parsing an individual node always unambiguous (i.e. can be done without any
  //context other than the given environment+bindings)
  //throws if invalid
  vertex string2vertex(const std::string& str) {
    assert(!str.empty());
    if (func_t f=string2func(str))
      return f;
    char c=str[0];
    if (c=='-' || c=='.' || (c>='0' && c<='9')) {
      try {
        return boost::lexical_cast<contin_t>(c);
      } catch (...) {
        throw_bad_number(str);
      }
    }
    return string2scalar(str);
  }

  //returns a func_t if available, else NULL
  func_t string2func(const std::string& str) {
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
  vertex string2scalar(const std::string& str) {
#if 0
    if (str[0]=='$') {
      bindings::const_iterator i=scalars.find(str.substr(1));
      if (i==scalars.end()) {
        validate_identifier(str.substr(1),e);
        throw_bad_identifier(e,str);
      }
      return i->second;
    }
    throw_bad_identifier(str);
#endif
    return vertex();
  }

  const std::string& sexpr2identifier(const_sub_sexpr src) {
    if (!src.childless())
      throw_bad_identifier(boost::lexical_cast<std::string>(src));
    return src.root();
  }

  lang::arity_t sexpr2arity(const_sub_sexpr src) {
    if (!src.childless())
      throw_bad_arity_decl(boost::lexical_cast<std::string>(src));
    try {
      return boost::lexical_cast<arity_t>(src.root());
    } catch (...) {
      throw_bad_arity_decl(boost::lexical_cast<std::string>(src));
    }
    return arity_t(0);
  }

  void validate_arity(const_sub_sexpr src,func_t f) {
    if (src.arity()!=f->arity())
      if (const std::string* name=env.func2name(f))
        throw_bad_arity(*name,src.arity(),f->arity());
      else
        throw_bad_arity("anonymous function",src.arity(),f->arity());
  }
  
  /**  void bind_arguments(const_sub_sexpr src,bindings& scalars) {

       }**/


};

void sexpr2vtree(const_sub_sexpr src,vsubtree dst,environment& env)
    throw(std::runtime_error) {
  semantic_analyzer se(env);
  se.process_sexpr(src,dst,false);
}

}} //namespace plap::lang
