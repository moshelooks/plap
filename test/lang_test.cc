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

#if 0

#define check_eval(str,res) check_eq(env.eval(str),res)

test_case(lang_eval_examples) {
  environment env;
  require_eq(env.eval("import \"doc/examples.txt\""),id::unit);

  check_eval("fact 0",1);
  check_eval("fact 1",1);
  check_eval("fact 2",2);
  check_eval("fact 3",6);
  check_eval("fact 9",362880);

  check_eval("fact1 0",1);
  check_eval("fact1 1",1);
  check_eval("fact1 2",2);
  check_eval("fact1 3",6);
  check_eval("fact1 9",362880);

  check_eval("fib 1+1",2);
  check_eval("fib 1-1",1);
  check_eval("fib 14",610);

  check_eval("I 42",42);
  check_eval("(K 1) 2",1);
  check_eval("S (\$x -> \$y -> times $x $y) negative 5",-25);

  check_eval("nth [1,2,3] 0",1);
  check_eval("nth [0..10] 5",5);
  check_eval("nth ['a','b','c'] 1",'b');
}  

test_case(lang_env_declare_func) {
  environment env;

  func_t foo=env.declare_func(1,"foo");

  check_eq(env.name2func("foo"),foo);

  check(env.func2name(foo));
  check_eq(*env.func2name(foo),"foo");

  check(!env.name2func("goo"));
}

test_case(lang_parse_expr) {
  environment env;
  func_t foo=env.declare_func(3,"foo");

  string target_str="foo(1 2)";
  vtree target=tree_of(vertex(foo))(vertex(disc_t(1)),vertex(disc_t(2)));

  sexpr s=sexpr(std::string("")); 
  stringstream ss;
  ss << target_str;
  stream2sexpr(ss,s);
  check_tree(s,3,target_str);

  vtree dst=vtree(vertex());
  //this should fail since foo has arity 3
  check_throw(sexpr2vtree(s,dst,env),runtime_error);
  dst.prune();
  s.append(std::string("3"));
  //should work now
  sexpr2vtree(s,dst,env);

  check_eq(dst.size(),4u);
  check_eq(vertex_cast<func_t>(dst.root()),foo);
  check_eq(vertex_cast<number_t>(dst[0].root()),1);
  check_eq(vertex_cast<number_t>(dst[1].root()),2);
  check_eq(vertex_cast<number_t>(dst[2].root()),3);
}

test_case(lang_declare_func) {
  //fixme
}

test_case(lang_define_func) {
  environment env;
  string str="def(foo list($x $y) $x)";

  vtree dst=vtree(vertex());
  string2vtree(str,dst,env);

  vtree target=tree_of(vertex(disc_t(0))); //should be unit
  check_eq(dst,target);

  check(env.name2func("foo"));
  environment::argname_seq an=env.argnames(env.name2func("foo"));
  check_eq(an.size(),2u);
  check_eq(an[0],"x");
  check_eq(an[1],"y");

  string call="foo(1 2)";
  string2vtree(call,dst,env);
  vtree target2=tree_of(vertex(env.name2func("foo")))(vertex(disc_t(1)),
                                                      vertex(disc_t(2)));
  check_eq(dst,target2);

  vtree result=vtree(vertex());
  //fixmeenv.eval(dst,result);
  check_eq(result,tree_of(vertex(disc_t(1))));
}

test_case(lang_let) {
  environment env;
  string str="let(list(decl(foo list($x $y) $x)) foo(1 2))";
  vtree dst=vtree(vertex());
  string2vtree(str,dst,env);

  check_eq(dst,tree_of(vertex(disc_t(1))));
}  

test_case(lang_parse) {

}

#if 0


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
      <list_of<const_subvtree>,
      func_of<const_subvtree(const_subvtree)> >(&lang_foreach);
   def* pr=make_eager_def<const_subvtree >(&lang_print);
   vtree src=tree_of(vertex(fe))(tree_of(vertex(&c))(vertex(disc_t(1)),
                                                    vertex(disc_t(2)),
                                                    vertex(disc_t(3))),
                                vertex(pr));
  vtree dst=vtree(vertex(disc_t(42)));
  e(src,dst);
}


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

#endif
