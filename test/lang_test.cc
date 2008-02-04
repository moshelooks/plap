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

test_case(lang_small_sum) {
  cons<disc_t> c;
  eval<disc_t> e;
  def* d=make_eager_def<list_of<disc_t> >(&lang_plus<disc_t>);
  vtree src=tree_of(vertex(d))(tree_of(vertex(&c))(vertex(disc_t(1)),
                                                   vertex(disc_t(2)),
                                                   vertex(disc_t(3))));
  vtree dst=vtree(vertex());

  e(src,dst);
  check(dst.childless());
  check_eq(vertex_cast<disc_t>(dst.root()),disc_t(6));
}

struct tovertex {
  typedef vertex result_type;
  vertex operator()(disc_t i) const { return vertex(i); }
};

test_case(lang_big_sum) {
  disc_t lim=100000;

  cons<disc_t> c;
  eval<disc_t> e;
  def* d=make_eager_def<list_of<disc_t> >(&lang_plus<disc_t>);
  vtree src=tree_of(vertex(d))(tree_of(vertex(&c)));
  src.append(src[0].begin(),
             transform_it(count_it(disc_t(0)),tovertex()),
             transform_it(count_it(disc_t(lim)),tovertex()));
  vtree dst=vtree(vertex());
  e(src,dst);

  disc_t result=std::accumulate(count_it(disc_t(0)),count_it(lim),disc_t(0));
  check(dst.childless());
  check_eq(vertex_cast<disc_t>(dst.root()),result);
}



test_case(lang_foreach_print) {
  cons<disc_t> c;
  eval<disc_t> e;
  def* fe=make_eager_def
      <list_of<const_vsubtree>,
      func_of<const_vsubtree(const_vsubtree)> >(&lang_foreach);
   def* pr=make_eager_def<const_vsubtree >(&lang_print);
   vtree src=tree_of(vertex(fe))(tree_of(vertex(&c))(vertex(disc_t(1)),
                                                    vertex(disc_t(2)),
                                                    vertex(disc_t(3))),
                                vertex(pr));
  vtree dst=vtree(vertex(disc_t(42)));
  e(src,dst);
}

#if 0

  //check_eq(dst,tree_of(vertex(6)));

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
