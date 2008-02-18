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

#include "repl.h"
#include "parse.h"
#include "environment.h"
#include "io.h"
#include "tree_io.h"
//fixme#include "pretty_print.h"

namespace plap { namespace lang_io {

void repl(std::istream& in,std::ostream& out,lang::environment& env,
          const std::string& prompt) throw(std::runtime_error) {
  out << util::sexpr_format;
  util::io_loop<sexpr>(in,out,&indent_parse,
                       boost::bind(&eval_print,ref(_1),ref(env),
&pretty_print)
  while (in.good()) {
    out << prompt << std::endl;

    sexpr s;
    parse(in,s);
    if (!in.good())
      break;
    out << std::endl;

    assert(!s.empty());

    out << "goes to " << s << std::endl;

    /*
    vtree expr(vertex());
    sexpr_to_vtree(s,expr,env);
    
    //evaluate it
    vtree result(vertex());
    eval(expr,result);
    pretty_print(out,result);
    out << endl;
    */
  }
}

}} //namespace plap::lang
