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

#include "names.h"
#include <boost/assign/list_of.hpp>
#include "core.h"
#include "builtin.h"

//nil is its own thing - not a func (no arguments), 
//fixme  not a symbol - or should it be? $nil <-> []?

namespace plap { namespace lang_io {

//fixme put in parse
const char def_symbol[]="=";
const char strlit_symbol[]="\"";
const char apply_symbol[]="(";
const char cons_symbol[]=":";

namespace lang_io_private {
func_index func_names=boost::assign::map_list_of
    //("list",any_list::instance())

    ("apply",apply::instance())
    /*    ("def",def::instance())
    ("lambda",lambda::instance())
    ("let",let::instance())
    ("nil",nil::instance())
    ("pair",pair::instance())

    ("plus",lang_plus::instance())
    ("if",lang_if::instance())

    ("decl",decl::instance())*/;

arg_index arg_names; //core & builtin functions don't need arg names
symbol_index symbol_names=boost::assign::list_of
    ("true")
#define NAMES_arg(z,n,u) ("arg")
    BOOST_PP_REPEAT(LANG_ARG_MAX,NAMES_arg,~)
    ("false");



} //namespace lang_io_private

}} //namespace plap::lang_io
