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
#include "iterator_shorthands.h"
#include "core.h"
#include "builtin.h"

namespace plap { namespace lang_io {

const char def_symbol[]="=";
const char anon_func_name[]="anonymous_function";

/*
const char apply_symbol[]="(";
const char cons_symbol[]=":";*/

namespace lang_io_private {
func_index func_names=boost::assign::map_list_of
    ("apply",(func_t)lang_apply::instance())
    ("list",lang_list::instance())
    ("def",lang_def::instance())

    //conditionals
    ("if",lang_if::instance())

    //arithmetic operators
    ("plus",lang_plus::instance())
    ("times",lang_times::instance())

    //comparison operators
    //    ("less",lang_less::instance())


    /**("lambda",lambda::instance())
    ("let",let::instance())

    ("pair",pair::instance())**/

    ("decl",lang_decl::instance());

arg_index arg_names; //core & builtin functions don't need arg names

using namespace util;
std::string tostr(char c) { return std::string(1,c); }
symbol_index symbol_names=boost::assign::list_of<std::string>
    ("false")
    ("true")
    ("[]").
    repeat(LANG_ARG_MAX,"arg").
    range(
        transform_it(count_it(boost::integer_traits<char>::const_min),&tostr),
        transform_it(count_it(boost::integer_traits<char>::const_max),&tostr));

} //namespace lang_io_private

}} //namespace plap::lang_io
