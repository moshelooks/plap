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
#include "cast.h"
#include "func.h"

namespace plap { namespace lang_io {

namespace {
using namespace lang;
void sexpr_print(std::ostream& out,lang::const_subvtree s,bool space=false) {
  assert(boost::get<func_t>(&s.root()));

  //func_t f=vertex_cast<func_t>(s.root());
  //if (is_tuple(f))

  if (space)
    out << ' ';
  //return 42;
}
std::size_t name_length(lang::const_subvtree s) {
  std::stringstream ss;
  sexpr_print(ss,s);
  return ss.str().size();
}
} //namespace

std::ostream& pretty_print(std::ostream& out,lang::const_subvtree s,
                           std::size_t indent,std::size_t line_max) {
  static const std::size_t indent_max=5;
  static const std::size_t indent_incr=2;
  using namespace lang;
  using namespace util;
  using std::string;
  assert(boost::get<func_t>(&s.root()));

  string name=boost::lexical_cast<string>(*vertex_cast<func_t>(s.root()));
  dorepeat(indent) out << ' ';
  out << name;

  //decide how to format it based on how big it is
  std::size_t n=std::accumulate(transform_it(s.begin_sub_child(),&name_length),
                                transform_it(s.end_sub_child(),&name_length),
                                name.size())-1;
  if (n+indent<line_max || s.childless()) { //print on one line?
    out << ' ';
    if (!s.childless()) {
      std::for_each(s.begin_sub_child(),--s.end_sub_child(),
                    boost::bind(&sexpr_print,boost::ref(out),_1,true));
      sexpr_print(out,s.back_sub());
    }
    out << std::endl;
  } else if (name.size()<indent_max &&  //print first expr inline?
             name_length(s.front_sub())+name.size()+indent<line_max) { 
    out << ' ';
    sexpr_print(out,s.front_sub());
    out << std::endl;
    std::for_each(++s.begin_sub_child(),s.end_sub_child(),
                  boost::bind(&pretty_print,boost::ref(out),_1,
                              indent+name.size()+1,line_max));
  } else {
    out << std::endl;
    std::for_each(s.begin_sub_child(),s.end_sub_child(),
                  boost::bind(&pretty_print,boost::ref(out),_1,
                              indent+indent_incr,line_max));
    out << std::endl;
  }
  return out;
}

}} //namespace plap::lang_io
