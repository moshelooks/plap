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

namespace plap { namespace lang_io {

namespace lang_io_private {
bool sexpr_io=false;
const std::vector<infix_map> infix_by_arity=boost::assign::list_of<infix_map>
    (boost::assign::map_list_of     //nullary operators
     ("tuple","()")
     ("list","[]"))

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
     ("nequal","!=")
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
     ("xrange","..."))

    (boost::assign::map_list_of    //ternary operators
     ("def","="));

const infix_map infix_vararg=boost::assign::map_list_of   //variadic operators
    ("tuple","(")
    ("list","[");

} //namespace lang_io_private

const std::string def_symbol="=";
const std::string strlit_symbol="\"";
const std::string apply_symbol="(";

const std::string def_name="def";
const std::string strlit_name="strlit";
const std::string apply_name="apply";

const std::string list_name="list";
const std::string lambda_name="lambda";
const std::string let_name="let";
const std::string decl_name="decl";

}} //namespace plap::lang_io
