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

template<typename T>
struct accumulate_adapter {
  typedef T result_type;
  template<typename It>
  T operator()(It f,It l) const { return accumulate(f,l,T(0)); }
};

test_case(lang_plus) {
  environment env;

  func& f=env.create_func("plus");

  def* d=make_eager_def(accumulate_adapter<disc_t>(),list_of<disc_t>());
  env.bind(f,d);
  
  vtree src=tree_of(vertex(d))(tree_of(vertex(list_t()))(1,2,3);
  vtree dst(0);
  (*d)(src,dst,env);
  check_eq(dst,tree_of(vertex(6)));
}

#if 0

template<typename T>
struct transform_adapter {
  template<typename It,typename Out>
  void operator()(It f,It l,Out l

todo - add make_eager_transform_def

template<typename T>
struct accumulate_func {
  template<typename Iterator>
  T operator()(Iterator f,Iterator l) { return std::accumulate(f,l,T(0)); }
};

struct foreach_func {
  template<typename Iterator>
  void operator()(Iterator f,Iterator l) { 
    _dst=list..
    return std::foreach



  put the func_def in the tree, not the func

  env.register_builtin(f,make_list_adapter(bind(std::accumulate<
                                    

                                    accumulate_func<disc_t>()));
  env.register_builtin(f,unary_func(list_type<float_type>(),float_type(),
                                    accumulate_func<contin_t>()));

  vtree res(0);

  env.eval(tree_of(&f)(1,2,3),res);
  check_eq(res,tree_of("6"));

  env.eval(tree_of(&f)(42),res);
  check_eq(res,tree_of("42"));

  env.eval(tree_of(&f)(tree_of(&f)(1,2),7),res);
  check_eq(res,tree_of("10"));
}

test_case(lang_foreach) {
  func& f=env.register_func("foreach",std::foreach<);

  env.register_builtin(f,foreach_func())

//  register_declaration(register_rewrite("to_enf",lang_bool),
//                     &reduct::reduce_to_enf);
#endif
