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

struct pretty_printer {
  static const size_t indent_max=5;
  static const size_t indent_incr=2;

  //handles the case of a type-specified single node
#define LANG_IO_by_type(s,tocall,otherwise) {     \
    assert(get<func_t>(&s.root()));               \
    func_t f=vertex_cast<func_t>(s.root());       \
    if (f==number_type::instance())               \
      tocall<contin_t>(s);                        \
    else if (f==bool_type::instance())            \
      tocall<bool>(s);                            \
    else if (f==char_type::instance())            \
      tocall<char>(s);                            \
    else if (f==symbol_type::instance())          \
      tocall<disc_t>(s);                          \
    else                                          \
      otherwise(s);                               \
  }

  pretty_printer(ostream& out_,const_subvtree s,context& c_,size_t indent,
                 size_t linemax_)
      : out(out_),c(c_),indent_start(indent),linemax(linemax_) { 
    //LANG_IO_by_type()
    //indent_print(s,indent);
  }
  ostream& out;
  context& c;
  size_t indent_start,linemax;

  template<typename T> 
  void print_helper(const_subvtree s) {
    dorepeat(indent_start) out << ' ';
    print<T>(s.front());
    out << endl;
  }
  template<typename T>
  void print(vertex v) {
    if (is_arg<T>(s.root()))
      out << ('$'+c.argnames(f)[arg_idx(s.root())]);
    else
      print_value<t>(v);
  }
  template<typename T>
  void print_value(vertex);

  void indent_print(const_subvtree s) { indent_print(s,indent_start); }
  void indent_print(const_subvtree s,size_t indent) {
#if 0
    assert(get<func_t>(&s.root()));

    dorepeat(indent) out << ' ';

    func_t f=vertex_cast<func_t>(s.root());
    string name=lexical_cast<string>(*f);
    out << name;

    //decide how to format it based on how big it is
    size_t n=accumulate(transform_it(s.begin_sub_child(),&name_length),
                        transform_it(s.end_sub_child(),&name_length),
                        name.size())-1;
    
    dispatch_by_type(f,indent_print
    


    if (n+indent<line_max || s.childless()) { //print on one line?
      out << ' ';
      if (!s.childless()) {
        for_each(s.begin_sub_child(),--s.end_sub_child(),
                 bind(&sexpr_print,ref(out),_1,true));
        sexpr_print(out,s.back_sub());
      }
      out << endl;
    } else if (name.size()<indent_max &&  //print first expr inline?
               name_length(s.front_sub())+name.size()+indent<line_max) { 
      out << ' ';
      sexpr_print(out,s.front_sub());
      out << endl;
      for_each(++s.begin_sub_child(),s.end_sub_child(),
               bind(&indent_print,this,_1,indent+name.size()+1,s));
    } else {
      out << endl;
      for_each(s.begin_sub_child(),s.end_sub_child(),
               bind(&pretty_print,ref(out),_1,
                           indent+indent_incr,line_max));
      out << endl;
    }
    return out;
#endif
  }

  template<typename T>
  void sexpr_print(ostream& out,const_subvtree s,bool space=false);
#if 0
template<>
void sexpr_print<bool>(ostream& out,const_subvtree s,bool space=false) {
  assert((s.childless() &&  get<disc_t>(&s.root())) ||
         (!s.childless() && get<func_t>(&s.root())));
  
  if (s.childless())
  out << "(" 


get<func_t>(&s.root()));

  //func_t f=vertex_cast<func_t>(s.root());
  //if (is_pair(f))

  if (space)
    out << ' ';
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
    out << vertex_cast<contin_t>(v);
  }
  template<>void pretty_printer::print<bool>(vertex v) {
    out << (vertex_cast<disc_t>(v) ? true_name : false_name);
  }
  template<>void pretty_printer::print<char>(vertex v) {
    out << char(vertex_cast<disc_t>(v));
  }
  template<>void pretty_printer::print<disc_t>(vertex v) {
    out << c.idx2symbol(vertex_cast<disc_t>(v));
  }

} //namespace

ostream& pretty_print(ostream& out,const_subvtree s,context& c,
                      size_t indent,size_t line_max) {
  pretty_printer(out,s,c,indent,line_max);
  return out;
}

}} //namespace plap::lang_io
