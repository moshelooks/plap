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
#include "foreach.h"
#include "iterator_shorthands.h"
#include "vtree.h"
#include "dorepeat.h"
#include "cast.h"
#include "func.h"
#include "names.h"

namespace plap { namespace lang_io {

namespace {
using namespace lang;
using namespace util;
using namespace std;
using namespace boost;

struct directive {
  string prefix,infix,suffix;
    
  directive(const_subvtree& loc,func_t& f) {
    string s=lexical_cast<string>(*f);
    if (s==list_name) {
      prefix="[";
      infix=",";
      suffix="]";
    } else if (s==plus_name) {//fixme
      infix="+";
      assert(loc.arity()==1);
      f=vertex_cast<func_t>(loc.front());
      loc=loc.front_sub();
    } else if (const string* name=func2name(f)) {
      prefix=*name+" ";
      infix=" ";
    } else {
      prefix=lexical_cast<string>(*f)+"_"+
          lexical_cast<string>((unsigned int)f)+" ";
      infix=" ";
    }
  }
};

struct vertex_name_visitor : public vertex_visitor<string> {
  vertex_name_visitor(const argname_seq& a) : args(a) {}
  const argname_seq& args;

  string operator()(contin_t c) const { return lexical_cast<string>(c); }
  string operator()(disc_t d) const { 
    if (is_arg(d))
      return '$'+args[d];
    return symbol2name(d); 
  }
  string operator()(func_t f) const { return *func2name(f); }
};

struct pretty_printer {
  static const size_t indent_max=5;
  static const size_t indent_incr=2;

  ostream* o;
  size_t indent;
  size_t linemax;
  bool sexpr;
  const argname_seq& args;

  pretty_printer(ostream& o_,size_t indent_,size_t linemax_,
                 const argname_seq& a=argname_seq())
      : o(&o_),indent(indent_),linemax(linemax_),sexpr(false),args(a) {}

  string to_string(const_subvtree s) {
    stringstream ss;
    o=&ss;
    (*this)(s);
    return ss.str();
  }

  void operator()(const_subvtree s,const string& prefix="") {
    if (!sexpr)
      dorepeat(indent) (*o) << ' ';

    if (s.childless()) {
      (*o) << prefix << leaf_visit(vertex_name_visitor(args),s.root()) << endl;
      return;
    }

    func_t f=NULL;//fixmevleaf_cast<func_t>(s.root());
    directive d=directive(s,f);

    if (sexpr && d.prefix!="" && d.prefix!="[") {
      d.prefix="("+d.prefix;
      d.suffix+=")";
    }
    (*o) << prefix << d.prefix;

    vector<string> sexprs(s.arity());

    bool tmp=sexpr;
    ostream* out=o;
    sexpr=true;
    transform(s.begin_sub_child(),s.end_sub_child(),sexprs.begin(),
              bind(&pretty_printer::to_string,this,_1));
    sexpr=tmp;
    o=out;

    size_t n=accumulate(transform_it(sexprs.begin(),bind(&string::size,_1)),
                        transform_it(sexprs.end(),bind(&string::size,_1)),
                        prefix.size()+d.prefix.size()+d.suffix.size()+
                        (sexprs.size()-1)*(d.infix.size()+1)+1);


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
      for_each(++s.begin_sub_child(),s.end_sub_child(),*this);
    } else {
      (*o) << endl;
      indent+=indent_incr;
      for_each(s.begin_sub_child(),s.end_sub_child(),*this);
      //bind(&pretty_printer::operator(),this,_1));
    }
  }
};
} //namespace

void pretty_print(ostream& o,const_subvtree s,size_t indent,size_t linemax) {
  pretty_printer pp(o,indent,linemax);
  pp(s);
}

void pretty_print(ostream& o,func_t f,size_t indent,size_t linemax) {
  std::string name=lexical_cast<std::string>(*f);
  if (name==func_name) {
    assert(dynamic_cast<const func*>(f));
    
    std::stringstream ss;
    ss << name << " ";
    foreach (const std::string& s,func2arg_names(f))
      ss << '$' << s << " ";
    ss << "= ";
    
    pretty_printer pp(o,indent,linemax,func2arg_names(f));
    pp(static_cast<const func*>(f)->body(),ss.str());
  } else {
    o << name << "{built-in function}";
  }
}

}} //namespace plap::lang_io
