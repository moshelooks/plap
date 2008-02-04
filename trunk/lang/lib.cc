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

#include "lib.h"
#include "environment.h"

namespace lang {

#define LANG_LIB_cf(name) func& name=env.create_func(#name);
#define LANG_LIB_arithmetic_nary(name) {                                \
    LANG_LIB_cf(name);                                                  \
    env.bind(name,make_eager_def<                                       \
             list_of<int_t> >(&lang_ ## name<disc_t>));                 \
    env.bind(name,make_eager_def<                                       \
             list_of<float_t> >(&lang_ ## name<contin_t>));             \
  }
#define LANG_LIB_arithmetic_binary(name) {                              \
    LANG_LIB_cf(name);                                                  \
    bind(name,make_eager_def<                                           \
         int_t,int_t>(&lang_ ## name<disc_t>));                         \
    bind(name,make_eager_def<float_t,                                   \
         float_t>(&lang_ ## name<contin_t>));                           \
  }
#define LANG_LIB_comparison(name) {                                     \
    LANG_LIB_cf(name);                                                  \
    bind(name,make_eager_def<                                           \
         int_t,int_t>(&lang_ ## name<disc_t>));                         \
    bind(name,make_eager_def<float_t,                                   \
         float_t>(&lang_ ## name<contin_t>));                           \
  } 
    

void initialize_lib(environment& env) {
  //arithmetic functions
  LANG_LIB_arithmetic_nary("plus");
  LANG_LIB_arithmetic_nary("times");
  LANG_LIB_arithmetic_binary("minus");
  LANG_LIB_arithmetic_binary("div");

  //comparison operators
  LANG_LIB_comparison("equal");
  LANG_LIB_comparison("less");
  LANG_LIB_comparison("less_equal");
  LANG_LIB_comparison("greater");
  LANG_LIB_comparison("greater_equal");

  //
       
}

} //~namespace lang
