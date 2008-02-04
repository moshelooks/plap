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
#include "environment.h"
#include "pretty_print.h"

void repl(std::istream& in,std::ostream& out,lang::environment& env,
          const std::string prompt="") {
  while (in.good()) {
    out << prompt;
    out.flush();

    sexpr s("");
    stream_to_sexpr(cin,s);
    if (!in.good())
      break;
    out << endl;

    vtree expr(vertex());
    sexpr_to_vtree(s,expr,env);
    
    //evaluate it
    vtree result(vertex());
    eval(expr,result);
    pretty_print(out,result);
    out << endl;
  }
}
