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

#include "builtin.h"
#include "core.h"
#include "eager_func.h"
#include "lazy_func.h"

namespace plap { namespace lang {

#define LANG_LIB_cf(name) func& name=c.create_func(#name);
#define LANG_LIB_arithmetic_nary(name) {                                \
    LANG_LIB_cf(name);                                                  \
    c.bind(name,make_eager_def<                                         \
             list_of<int_t> >(&lang_ ## name<disc_t>));                 \
    c.bind(name,make_eager_def<                                         \
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

/*template<typename Op,typename T,typename U,const char* Name>
struct eager_func<Op,T(U)>
    : public stateless_func<eager_func<Op,T(U)>,1,Name> {
  void operator()(context& c,const_subvtree loc,subvtree dst) const {
*/   
    

void initialize_lib(context& c) {
  c.insert_builtin(nil::instance(),false);
  //c.insert_builtin(lang_list<const_subvtree>::instance(),false);
  c.insert_builtin
      (make_eager<func_of<disc_t(list_of<disc_t>)>,
                                 lang_io::plus_name>(&lang_plus),true);
  c.insert_builtin(make_lazy(&lang_if,lang_io::if_name),true);


  

#if 0 
foo
fixme
  //arithmetic functions
  LANG_LIB_arithmetic_nary(plus);
  LANG_LIB_arithmetic_nary(times);
  LANG_LIB_arithmetic_binary(minus);
  LANG_LIB_arithmetic_binary(div);

  //comparison operators
  LANG_LIB_comparison(equal);
  LANG_LIB_comparison(less);
  LANG_LIB_comparison(less_equal);
  LANG_LIB_comparison(greater);
  LANG_LIB_comparison(greater_equal);

  //
#endif  
}

}} //namespace plap::lang
