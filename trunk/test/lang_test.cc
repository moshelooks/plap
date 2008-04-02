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

static context c;
static vtree vtr;

#define str2vtr(src) {                                  \
    vtr=vtree(vertex());                                \
    cout << sexpr_format;                               \
    stringstream ss;                                    \
    ss << src;                                          \
    tree<string> tmpXXX;                                \
    indent_parse(ss,tmpXXX);                            \
    analyze(tmpXXX,vtr,c);                              \
  }
#define check_eval(src,goal) {                  \
    str2vtr(src);                               \
    vtree res=vtree(vertex());                  \
    c.eval(vtr,res);                            \
    stringstream ss;                            \
    pretty_print(ss,res);                       \
    check_eq(ss.str(),goal+string("\n"));       \
  }

test_case(lang_def_examples) {
  check_throw(str2vtr("def a b c"));
  check_throw(str2vtr("def a"));
  check_throw(str2vtr("def x (list $a b) c"));
  check_throw(str2vtr("def x (list ($a $b $c)) d"));
  check_throw(str2vtr("def x (list $a $b $c) d e"));

  check_eval("foo $x = []","[]");
  check_eval("goo $x = foo","[]");
  check_throw(str2vtr("moo $x = blablabla"));
  
  check_eval("blub^4","[]");
  check_eval("moo $x = blub","[]");
  check_throw(str2vtr("blub $x = moo"));
  check_throw(str2vtr("moo^1"));
  check_throw(str2vtr("blub $x $y $z $q $m= moo"));
  check_eval("blub $x $y $z $q = moo","[]");
}

test_case(lang_if_examples) {
  check_eval("if true 0 1","0");
  check_eval("if false 0 1","1");

  check_eval("if 0<1 0 1","0");
  check_eval("if 1<0 0 1","1");
  check_eval("if 1<1 0 1","1");

  check_eval("if 1+1==2 0 1","0");
  check_eval("if 1+1!=2 0 1","1");
}

test_case(lang_def_and_call) {
  check_eval("boo $x = true","[]");
  check_eval("boo 42","true");

  check_eval("foo2 $x = $x+1","[]");
  check_eval("foo2 42","43");
}

test_case(lang_recursion) {
  check_eval("groo $x = if $x==0 0 $x+(groo $x-1)","[]");
  check_eval("groo 0","0");
  check_eval("groo 1","1");
  check_eval("groo 2","3");
  check_eval("groo 3","6");
}

test_case(lambda_test) {
  check_eval("(\\$x -> $x+1) 1","2");
}

test_case(let_test) {
  check_eval("let [x=5] x+1","6");
  check_eval("let [doo $x = $x+1] (doo 42)","43");
  check_eval("mmm $x = let [nnn $y = $y+1] $x+(nnn 42)","[]");
  check_eval("mmm 100","143");
  check_throw(str2vtr("nnn 42")); //out of scope

  check_eval("qq $a = let [ww $b = (let [ee $c = $c*2] (ee $b+1))] (ww $a*2)",
             "[]");
  check_eval("qq 10","42");
  //wont work until typecheck can verify the arity of ooo
  //check_eval("let [ooo = \\$x -> $x+1] (ooo 42)","43");
}

test_case(test_closure) {
  check_eval("((\\$x -> (\\$y -> $x+$y)) 42) 100","142");
  check_eval("blup $x = \\$y -> $x+$y","[]");
  check_eval("(blup 4) 5","9");
  check_eval("S $x = \\$y -> \\$z -> ($x $z) ($y $z)","[]");
  check_eval("((S \\$x -> \\$y -> $x*$y) negative) 3","-9");
}

test_case(lang_import_and_test) {
  check_throw(str2vtr("import \"nonexistent_fileadfkasdf\""));
  check_throw(str2vtr("import 1+1"));

  //this contains a bunch of assertion tests written in combo
  //fixmecheck_eval("import \"combo/test.co\"","[]");
}

