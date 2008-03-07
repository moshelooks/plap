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

namespace plap { namespace lang_io {

namespace lang_io_private {
func_index func_names;
arg_index arg_names;
symbol_index symbol_names;
} //namespace lang_io_private

const char def_symbol[]="=";
const char strlit_symbol[]="\"";
const char apply_symbol[]="(";
const char cons_symbol[]=":";

const char def_name[]="def";
const char strlit_name[]="strlit";
const char apply_name[]="apply";

const char list_name[]="list";
const char lambda_name[]="lambda";
const char let_name[]="let";
const char decl_name[]="decl";
const char pair_name[]="pair";

const char nil_name[]="nil";
const char true_name[]="true";
const char false_name[]="false";

const char plus_name[]="plus";

const char if_name[]="if";

const char func_name[]="func";

}} //namespace plap::lang_io
