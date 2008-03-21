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

#include "core.h"
#include <vector>
#include "iterator_shorthands.h"
#include "algorithm.h"

using boost::bind;
using util::foreach;

namespace plap { namespace lang {

void lang_list::operator()(context& c,const_subvtree s,subvtree d) const { 
  assert(!s.childless());
  assert(d.childless());
  d.root()=call(this);
  d.append(s.arity(),vertex());
  util::for_each(s.begin_sub_child(),s.end_sub_child(),d.begin_sub_child(),
                 boost::bind(&context::eval,&c,_1,_2));
}

//let injects bindings into the context
void lang_let::operator()(context& c,const_subvtree s,subvtree d) const { 
  assert(s.arity()==2);
  assert(s.front()==lang_list::instance());
  std::vector<func_t> closures;
  foreach (vertex v,children(s.front_sub())) {
    assert(arg_cast<func_t>(v)->body());
    if (arg_cast<func_t>(v)->closure()) {
      closures.push_back(arg_cast<func_t>(v));
      c.bind_closure(closures.back());
    }
  }
  c.eval(s.back_sub(),d);
  foreach (func_t f,closures)
    c.unbind_closure(f);
}

void lang_ident::operator()(context& c,const_subvtree s,subvtree d) const {
  if (_arity==0 || s.childless()) {
    d=*c.ident_binding(this);
  } else {
    assert(s.arity()==_arity);
    context::bindings& b=c.push_bindings(_arity,_offset);
    std::for_each(s.begin_sub_child(),s.end_sub_child(),b.begin(),
                  boost::bind(&context::eval,&c,_1,_2));
    c.eval(*c.binding(this),d);
    c.pop_bindings();
  }
}

void lang_closure::operator()(context& c,const_subvtree s,subvtree d) const {
  assert(false); //we shouldn't get called directly, only inside an apply-expr
}
  
#if 0

  assert(s.arity()==2);
  
    what offset to use when 
    
  if (vtree* v=c.let_lookup(this)) {

        d=
      d=lang_lambda::instance


    
    rhs


  friend struct context;
  arity_t _arity,_offset;
  enum { simple,unbound_closure,closure } _state;

  lang_variable(arity_t a,arity_t o) : _arity(a),_offset(o) {}
  
  bool external_arg(vertex v) const { 
    arity_t a=test_lang_arg_cast(v);
    assert(a==variadic_arity || a<_arity+_offset);
    return (a!=variadic_arity && a<_offset);
  }
  void copy_arg(context& c,const_subvtree leaf) const {
    if (external_arg(leaf.root()))
      leaf=c.bindings()[lang_arg_cast(leaf.root())];
  }



for_each(s.front_sub().begin_sub_child(),s.front_sub().end_sub_child(),
           bindings.begin(),bind(&func::operator(),boost::ref(c),
                                 bind(&arg_cast<func_t>,
                                      bind(&const_subvtree::root,_1)),_2));
  c.eval(s.back_sub(),d);
  if (d.childless())
    if (func_t f=test_func_arg_cast(d.root())) {
      bmap::const_iterator i=bindings.find(f);
      if (i!=bindings.end())
        d=i->second;
    }
}

void lang_variable::operator()(context& c,const_subvtree s,subvtree d) const {
  assert(!_body.empty());
  if (s.childless()) {
    d=_body;
    return;
  }
  
  assert(_body.childless());
  assert(test_func_arg_cast(_body.root()));
  (*arg_cast<func_t>(_body.root()))
  



  

  if (_state==simple) { 
    if (s.childless()) //case #1
      d=_body;
    else //case #2
      eval(c,s,d);
  } else if (_state==unbound_closure) { //case #3
    assert(s.childless());
    vtree tmp=_body;
    std::for_each(sub_leaves(tmp.begin_sub()),sub_leaves(tmp.end_sub()),
                  boost::bind(&lang_variable::copy_arg,this,boost::ref(c),_1));
    d.root()=this;
    _state=bound_closure;
  } else { //bound_closure
    if (s.childless()) { //case #4
    d=s;
    push_bindings(c,s);
    d=this;
  }
      

  if (s.childless()) {
    assert(!_closure);
    if (const vtree* v=s.root()->body()) {     //case #4
      bind(c,s);
      (*arg_cast<func_t>(s.root()))(c,*v,d);
      c.pop_bindings();
    } else {                                   //case #2
      d=_body;
    }
  } else if (_closure) {                       //case #3
   
  } else {                                     //case #1
    eval(c,s,d);
  }
#endif
}

void lang_variable::eval(context& c,const_subvtree s,subvtree d) const {
  context::bindings& b=c.push_bindings(_arity,_offset);
  std::for_each(loc.begin_sub_child(),loc.end_sub_child(),b.begin(),
                boost::bind(&context::eval,&c,_1,_2));
  c.eval(_body,d);
  c.pop_bindings();
}

void lang_variable::set_body(subvtree b) { 
  _body.splice(_body.end(),b); 
  foreach (vertex v,_body)
    if (external_arg(v)) {
      _state=unbound_closure;
      return;
    }
  _state=simple;
}


}} //namespace plap::lang

 assert(s.arity()==_arity);



  assert(!_body.empty());

  if (_state==simple) { 
    if (s.childless()) //case #1
      d=_body;
    else //case #2
      eval(c,s,d);
  } else if (_state==unbound_closure) { //case #3
    assert(s.childless());
    vtree tmp=_body;
    std::for_each(sub_leaves(tmp.begin_sub()),sub_leaves(tmp.end_sub()),
                  boost::bind(&lang_variable::copy_arg,this,boost::ref(c),_1));
    d.root()=this;
    _state=bound_closure;
  } else { //bound_closure
    if (s.childless()) { //case #4
    d=s;
    push_bindings(c,s);
    d=this;
  }
      

  if (s.childless()) {
    assert(!_closure);
    if (const vtree* v=s.root()->body()) {     //case #4
      bind(c,s);
      (*arg_cast<func_t>(s.root()))(c,*v,d);
      c.pop_bindings();
    } else {                                   //case #2
      d=_body;
    }
  } else if (_closure) {                       //case #3
   
  } else {                                     //case #1
    eval(c,s,d);
  }
#endif
}

void lang_variable::eval(context& c,const_subvtree s,subvtree d) const {
  context::bindings& b=c.push_bindings(_arity,_offset);
  std::for_each(loc.begin_sub_child(),loc.end_sub_child(),b.begin(),
                boost::bind(&context::eval,&c,_1,_2));
  c.eval(_body,d);
  c.pop_bindings();
}

void lang_variable::set_body(subvtree b) { 
  _body.splice(_body.end(),b); 
  foreach (vertex v,_body)
    if (external_arg(v)) {
      _state=unbound_closure;
      return;
    }
  _state=simple;
}
#endif

}} //namespace plap::lang
