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

#include "pretty_print.h"
#include <stdexcept>
#include <sstream>
#include <numeric>
#include <boost/bind.hpp>
#include <boost/lexical_cast.hpp>
#include "iterator_shorthands.h"
#include "vtree.h"
#include "dorepeat.h"
#include "context.h"
#include "cast.h"
#include "func.h"

namespace plap { namespace lang_io {

namespace {
using namespace lang;
using namespace util;
using namespace std;
using namespace boost;

template<typename Visitor>
void type_dispatch(func_t f,Visitor& visit,const_subvtree s,vertex v) {
  if (!s.childless()) {
    visit(s,vertex_cast<func_t>(v));
    return;
  }
  if (f==number_type::instance())
    visit(vertex_cast<contin_t>(v));
  else if (f==bool_type::instance())
    visit(bool(vertex_cast<disc_t>(v)));
  else if (f==char_type::instance())
    visit(char(vertex_cast<disc_t>(v)));
  else if (f==symbol_type::instance())
    visit(vertex_cast<disc_t>(v));
  else
    visit(s,vertex_cast<func_t>(v));
}

struct pretty_printer {
  static const size_t indent_max=5;
  static const size_t indent_incr=2;

  ostream* o;
  context& c;
  size_t indent;
  size_t linemax;
  bool sexpr;

  pretty_printer(ostream& o_,context& c_,size_t indent_,size_t linemax_) 
      : o(&o_),c(c_),indent(indent_),linemax(linemax_),sexpr(false) {}

  struct directive {
    string prefix,infix,suffix;
    
    directive(context& c,const_subvtree& loc,func_t& f) {
      string s=lexical_cast<string>(*f);
      if (s==list_name) {
        prefix="[";
        infix=",";
        suffix="]";
      } else if (s==plus_name) {
        infix="+";
        assert(loc.arity()==1);
        f=vertex_cast<func_t>(loc.front());
        loc=loc.front_sub();
      } else if (const string* name=c.func2name(f)) {
        prefix=*name+" ";
        infix=" ";
      } else {
        prefix=lexical_cast<string>(*f)+"_"+
            lexical_cast<string>((unsigned int)f)+" ";
        infix=" ";
      }
    }
  };

  /** doesn't yet handle args - fixme
  template<typename T>
  void operator()(func_t parent,vertex v) {
    if (is_arg<T>(v)
      (*o) << ('$'+c.argnames(f)[arg_idx(s.root())]);
    else
      print_value<t>(v);
      }**/

  void operator()(bool b) const {
    if (!sexpr)
      dorepeat(indent) (*o) << ' ';
    (*o) << (b ? true_name : false_name);    
  }
  void operator()(char c) const { 
    if (!sexpr)
      dorepeat(indent) (*o) << ' ';
    (*o) << c;
  }
  void operator()(disc_t s) const { 
    if (!sexpr)
      dorepeat(indent) (*o) << ' ';
    (*o) << c.idx2symbol(s);
  }
  void operator()(contin_t c) const { 
    if (!sexpr)
      dorepeat(indent) (*o) << ' ';
    (*o) << c;
  }

  void operator()(const_subvtree s,func_t f) { 
    directive d=directive(c,s,f);

    if (!sexpr) {
      dorepeat(indent) (*o) << ' ';
    } else if (sexpr && d.prefix!="" && d.prefix!="[") {
      d.prefix="("+d.prefix;
      d.suffix+=")";
    }
    (*o) << d.prefix;

    vector<string> sexprs(s.arity());
    size_t n=d.prefix.size()+d.suffix.size()+(sexprs.size()-1)*d.infix.size();
    bool tmp=sexpr;
    ostream* out=o;
    sexpr=true;
    int idx=0;
    for (vtree::const_sub_child_iterator i=s.begin_sub_child();
         i!=s.end_sub_child();++i,++idx) {
      stringstream ss;
      o=&ss;      
      type_dispatch(c.arg_type(f,idx),*this,*i,i->root());
      sexprs[idx]=ss.str();
      n+=sexprs[idx].size()+1;
    }
    sexpr=tmp;
    o=out;

    if (sexpr || (n+indent<linemax || s.childless())) { //print on one line?
      for (size_t i=0;i<sexprs.size();++i) {
        (*o) << sexprs[i];
        if (i+1!=sexprs.size())
          (*o) << d.infix;
      }
      (*o) << d.suffix << endl;
    } else if (d.prefix.size()<indent_max &&  //print first expr inline?
               sexprs.front().size()+d.prefix.size()+indent<linemax) { 
      (*o) << sexprs.front() << endl;
      indent+=d.prefix.size();
      int idx=1;
      for (vtree::const_sub_child_iterator i=++s.begin_sub_child();
           i!=s.end_sub_child();++i,++idx)
        type_dispatch(c.arg_type(f,idx),*this,*i,i->root());
    } else {
      (*o) << endl;
      indent+=indent_incr;
      int idx=0;
      for (vtree::const_sub_child_iterator i=s.begin_sub_child();
           i!=s.end_sub_child();++i,++idx)
        type_dispatch(c.arg_type(f,idx),*this,*i,i->root());
    }
  }
};
#if 0

struct pretty_printer {

  pretty_printer(ostream& (*o)_,const_subvtree s,context& c_,size_t indent,
                 size_t linemax_) : (*o)((*o)_),c(c_),linemax(linemax_) {
    assert(get<func_t>(&s.root()));
    type_dispatch(s.root(),indent_print(this,indent),

    //handles the case of a type-specified single node

    func_t f=vertex_cast<func_t>(s.root());
    
  ostream& (*o);
  context& c;
  size_t indent_start,linemax;

  template<typename T> 
  void print_helper(const_subvtree s) {
    dorepeat(indent_start) (*o) << ' ';
    print<T>(s.front());
    (*o) << endl;
  }
  template<typename T>
  void print(vertex v) {
    if (is_arg<T>(s.root()))
      (*o) << ('$'+c.argnames(f)[arg_idx(s.root())]);
    else
      print_value<t>(v);
  }
  template<typename T>
  void print_value(vertex);

  void indent_print(const_subvtree s) { indent_print(s,indent_start); }



  void indent_print(const_subvtree s,size_t indent) {
#if 0
    assert(get<func_t>(&s.root()));

    dorepeat(indent) (*o) << ' ';

    func_t f=vertex_cast<func_t>(s.root());
    string name=lexical_cast<string>(*f);
    (*o) << name;

    //decide how to format it based on how big it is
    size_t n=accumulate(transform_it(s.begin_sub_child(),&name_length),
                        transform_it(s.end_sub_child(),&name_length),
                        name.size())-1;
    
    dispatch_by_type(f,indent_print
    


    if (n+indent<linemax || s.childless()) { //print on one line?
      (*o) << ' ';
      if (!s.childless()) {
        for_each(s.begin_sub_child(),--s.end_sub_child(),
                 bind(&sexpr_print,ref((*o)),_1,true));
        sexpr_print((*o),s.back_sub());
      }
      (*o) << endl;
    } else if (name.size()<indent_max &&  //print first expr inline?
               name_length(s.front_sub())+name.size()+indent<linemax) { 
      (*o) << ' ';
      sexpr_print((*o),s.front_sub());
      (*o) << endl;
      for_each(++s.begin_sub_child(),s.end_sub_child(),
               bind(&indent_print,this,_1,indent+name.size()+1,s));
    } else {
      (*o) << endl;
      for_each(s.begin_sub_child(),s.end_sub_child(),
               bind(&pretty_print,ref((*o)),_1,
                           indent+indent_incr,linemax));
      (*o) << endl;
    }
    return (*o);
#endif
  }

  template<typename T>
  void sexpr_print(ostream& (*o),const_subvtree s,bool space=false);
#if 0
template<>
void sexpr_print<bool>(ostream& (*o),const_subvtree s,bool space=false) {
  assert((s.childless() &&  get<disc_t>(&s.root())) ||
         (!s.childless() && get<func_t>(&s.root())));
  
  if (s.childless())
  (*o) << "(" 


get<func_t>(&s.root()));

  //func_t f=vertex_cast<func_t>(s.root());
  //if (is_pair(f))

  if (space)
    (*o) << ' ';
  //return 42;
}
#endif
  size_t name_length(const_subvtree s) {
    stringstream ss;
    sexpr_print(ss,s);
    return ss.str().size();
  }
};

template<>void pretty_printer::print<contin_t>(vertex v) {
    (*o) << vertex_cast<contin_t>(v);
  }
  template<>void pretty_printer::print<bool>(vertex v) {
    (*o) << (vertex_cast<disc_t>(v) ? true_name : false_name);
  }
  template<>void pretty_printer::print<char>(vertex v) {
    (*o) << char(vertex_cast<disc_t>(v));
  }
  template<>void pretty_printer::print<disc_t>(vertex v) {
    (*o) << c.idx2symbol(vertex_cast<disc_t>(v));
  }
#endif
} //namespace

ostream& pretty_print(ostream& o,const_subvtree s,context& c,
                      size_t indent,size_t linemax) {
  assert(vertex_cast<func_t>(s.root()));
  pretty_printer pp(o,c,indent,linemax);
  pp(s,vertex_cast<func_t>(s.root()));
  return o;
}

}} //namespace plap::lang_io
