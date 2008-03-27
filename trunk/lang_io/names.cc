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
const char char_symbol[]="'";
const char string_symbol[]="\"";
const char anon_func_name[]="anonymous_function";

/*
const char apply_symbol[]="(";
const char cons_symbol[]=":";*/

namespace lang_io_private {
#define namef(name) (#name,(func_t)lang_ ## name :: instance())
func_index func_names=boost::assign::map_list_of
    //control flow
    namef(do)
    namef(if)

    //arithmetic operators
    namef(plus)
    namef(times)
    namef(minus)
    namef(div)
    namef(negative)

    //comparison operators
    namef(less)
    namef(less_equal)
    namef(greater)
    namef(greater_equal)
    namef(equal)
    namef(not_equal)

    //logical operators
    namef(and)
    namef(or)
    namef(not)

    //list operators
    namef(cons)
    namef(concat)
    namef(range)
    namef(xrange)

    //fp constructs
    namef(apply)
    namef(accumulate)
    namef(hd)
    namef(tl)

    //util
    namef(assert)
    namef(print)
    namef(println)
    namef(expand)
    namef(set)
    namef(symbol2index)
    namef(index2symbol)

    /**("let",let::instance())

    ("pair",pair::instance())**/
    //core
    //fixme namef(lambda)
    //fixmenamef(arrow)
    //namef(ident)
    namef(list);
    //fixmenamef(def)
    //fixmenamef(decl);
#undef namef

arg_index arg_names; //core & builtin functions don't need arg names

using namespace util;
std::string tostr(char c) { return "'"+std::string(1,c)+"'"; }
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
