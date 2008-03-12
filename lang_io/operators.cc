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

#include "operators.h"
#include <boost/assign/list_of.hpp>

namespace plap { namespace lang_io { namespace lang_io_private {
bool sexpr_io=false;
const std::vector<infix_map> infix_by_arity=boost::assign::list_of<infix_map>
    (boost::assign::map_list_of     //nullary operators
     ("[]","[]"))//fixme

    (boost::assign::map_list_of     //unary operators
     ("not","!")
     ("negative","-")
     ("lambda","\\")
     ("fact","<-"))

    (boost::assign::map_list_of    //binary operators
     ("plus","+")
     ("minus","-")
     ("times","*")
     ("div","/")

     ("equal","==")
     ("not_equal","!=")
     ("less","<")
     ("less_equal","<=")
     ("greater",">")
     ("greater_equal",">=")

     ("and","&&")
     ("or","||")

     ("arrow","->")

     ("cons",":")
     ("concat","~")
     ("range","..")
     ("xrange","...")
     
     ("decl","^"))

    (boost::assign::map_list_of    //ternary operators
     ("def","="));

const infix_map infix_variadic=boost::assign::map_list_of   //variadic ops
    ("pair","(")
    ("list","[");

}}} //namespace plap::lang_io::lang_io_private
