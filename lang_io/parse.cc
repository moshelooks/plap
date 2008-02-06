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

#include <boost/lexical_cast.hpp>
#include "algorithm.h"
#include "tree_io.h"
#include "environment.h"

namespace plap { namespace lang_io {

using namespace lang;
using namespace util;

//error handling
struct error_info { };
struct bad_parse : public std::runtime_error {
  bad_parse(const error_info& e,const std::string& name) : info(e) { 
    info.set_type("parse error");
    info.set_name("bad "+name);
  }
  error_info info;
};
#define LANG_PARSE_exception(name)                            \
  struct bad_## name : public bad_parse {                     \
    bad_ ## name(const error_info& e,const std::string& inst) \
    : bad_parse(e,std::string(#name)+"'"+inst+"'") {}         \
  };
LANG_PARSE_exception(scalar);
LANG_PARSE_exception(identifier);
LANG_PARSE_exception(name);
#undef LANG_PARSE_exception

std::istream& stream2sexpr(std::istream& in,sexpr& dst) {
  //fixme - add a stringstream buffer that handles syntactic sugar
  return in >> dst;
}

struct semantic_analyzer {
  semantic_analyzer(environment& e) : env(e) {}
  environment& base;
  bindings scalars,lets;

#define process(name) \
  void process_ ## name(const_sub_sexpr src,vsubtree dst,bool nest=true)
#define special_case(name)                       \
    if (f==id::name) {                           \
      process_ ## name(src,dst,nest);            \
      return;                                    \
    }

  process(sexpr) {
    if (src.childless()) {
      dst.root()=to_vertex(src.root());
      return;
    }

    func_t f=to_func(src.root());
    if (!f) { //a scalar - need to introduce an apply node
      dst.root()=id::apply;
      dst.append(to_vertex(f));
      dst.append(id::list);

      assert(src.root()[0]=='$'); //if these are false we should have 
      assert(nest);               //thrown already

      process_children(src,dst.back_sub());
      return;
    }
    validate_arity(src,f);

    special_case(def);     
    special_case(lambda); 
    special_case(let);
    special_case(decl);

    dst.root()=f;
    process_children(src,dst);
  }

  process(children) {
    dst.append(src.arity(),vertex());
    for_each(src.begin_sub_child(),src.end_sub_child(),dst.begin_sub_child(),
             boost::bind(&semantic_analyzer::process_sexpr,this,_1,_2));
  }

  process(def) { //def(name list(arg1 arg2 ...) body)
    //validate and set up arguments
    const std::string& name=sexpr2identifier(src[0]);
    argument_list args=bind_arguments(src[1],scalars);
    vtree body(vertex());
    process_sexpr(src[2],body);
    scalars.pop(args);

    if (func_t* f=env.name2func(name)) { //adding to an existing function?
      if (!def_match(i->second->args(),args))
        throw_bad_def_match(name,i->second->args(),args);
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
        throw_undeclared_name(src,name);
      //otherwise, create a new function and return unit
      env.create_func(name,args,body);
      dst.root()=id::unit;
    }
  }
  
  process(lambda) { //lambda(args body)
    argument_list args=bind_arguments(src[0],scalars); //fixme
    vtree body(vertex());
    process_sexpr(src[1],body);
    scalars.pop(args); //fixme
    dst.root()=env.create_func(args,body);
  }

  process(let) { //let(args body) - where each arg is an def
    argu..;//fixme
  }

  process(decl) { //decl(name arity)
    env.create_func(sexpr2identifier(src[0]),sexpr2arity(src[1]));
  }
 
  //parsing an individual node always unambiguous (i.e. can be done without any
  //context other than the given environment+bindings)
  vertex to_vertex(const std::string& str) {
    assert(!str.empty());
    char c=str.front();
    if (c=='$') {
      bindings::const_iterator i=scalars.find(str.substr(1));
      if (i==scalars.end()) {
        validate_identifier(str.substr(1),e);
        throw bad_scalar(e,str);
      }
      return i->second;
    }
    if (c=='-' || c=='.' (c>='0' && c<='9'))
      return boost::lexical_cast<contin_t>(c);

    if (func_t f=to_func(str))
      return f;
    throw_bad_identifier(str);
  }

  func_t to_func(const std::string& str) {
    bindings::const_iterator i=lets.find(str);
    if (i!=lets.end())
      return i->second;
    i=env.func_bindings().find(str); //fixme
    if (i==env.func_bindings().end()) {
      validate_identifier(e,str);
      return NULL;
    }
    return i->second;
  }
  
  void bind_arguments(const_sub_sexpr src,bindings& scalars) {

  }


};

void sexpr2vtree(const_sub_sexpr src,vsubtree dst,environment& env)
    throw(std::runtime_error) {
  semantic_analyzer se(env);
  se.process_sexpr(src,dst,false);
}

}} //namespace plap::lang
