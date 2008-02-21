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

namespace plap { namespace test {

#undef check_parse
#define check_parse(src,goal) {                         \
    cout << sexpr_format;                               \
    stringstream ss;                                    \
    ss << src;                                          \
    tree<string> tmpXXX;                                \
    indent_parse(ss,tmpXXX);                            \
    check_eq(lexical_cast<string>(tmpXXX),goal);        \
  }
#define check_parse_throw(src) {                        \
    cout << sexpr_format;                               \
    stringstream ss;                                    \
    ss << src;                                          \
    tree<string> tmpXXX;                                \
    check_throw(indent_parse(ss,tmpXXX),runtime_error); \
  }

test_case(parse_sexpr) {
  string s="(1 (2 2.5 2.8) 3 (4 5 6))";
  check_parse(s,s);
  check_parse("((1) (2) (3))","(1 2 3)");
}

test_case(parse_infix) {
  check_parse("(foo bar + bar baz)","(foo (plus bar bar) baz)");
  check_parse("(1+23*4)","(plus 1 (times 23 4))");
  check_parse("(a<b && c>d+e)","(and (less a b) (greater c (plus d e)))");


  check_parse("(foo x p = \\y z -> z+q)",
              "(def foo (list x p) (lambda (arrow (y z) (plus z q))))");

  check_parse("([1,2,3,foo bar, baz])","(list 1 2 3 (foo bar) baz)");

  check_parse("([(a b)])","(list (a b))");

  check_parse("([(a b),c+d,a (x x)])","(list (a b) (plus c d) (a (x x)))");

  check_parse("(a b:c d)","(a (cons b c) d)");

  check_parse("([]:[]:(a b+d))","(cons (cons list list) (a (plus b d)))");
  check_parse("([]:[]:a b+d)",
              "(apply (cons (cons list list) a) (list (plus b d)))");

  check_parse("(a+b)","(plus a b)");
  check_parse("(a==b)","(equal a b)");
  check_parse("(!a)","(not a)");
  check_parse("(-a)","(negative a)");
  check_parse("(a-b/c)","(minus a (div b c))");

  check_parse("([1..2])","(range 1 2)");

  check_parse("(\"  \")","(\"    )");
  check_parse("(\"foo \\\" ff\")","(\" f o o   \\ \"   f f)");
  check_parse("(\"sfs\\sdf\")","(\" s f s \\ s d f)");
  check_parse("(\"df\\\"fa\\\"\\\"fds\")",
              "(\" d f \\ \" f a \\ \" \\ \" f d s)");

  check_parse("[1,2,3]~[4,5,6]","(concat (list 1 2 3) (list 4 5 6))");
  check_parse("a~b:c~d","(cons (concat a b) (concat c d))");

  check_parse("(1 2 3) 4 5 6","(apply (1 2 3) (list 4 5 6))");
  check_parse("(1 2 3) (4 5) 6","(apply (1 2 3) (list (4 5) 6))");
  check_parse("(1) 2 3","(1 2 3)");
  check_parse("((1 2 3))","(1 2 3)");

  check_parse("(1 2)||(3 4)||5","(or (or (1 2) (3 4)) 5)");

  check_parse("[[[]]]","(list (list list))");

  check_parse("(1 2 3) (4 5 6)","(apply (1 2 3) (list (4 5 6)))");
  check_parse("(1 2 3) (list 4 5 6)","(apply (1 2 3) (list (list 4 5 6)))");
  check_parse("(1 2 3) list 4 5 6","(apply (1 2 3) (list list 4 5 6))");
}

test_case(parse_fail_infix) {
  check_parse_throw("[[[]]");
  check_parse_throw("(");
  check_parse_throw("a:");
  check_parse_throw(":a");

  check_parse_throw("1.2.");
  check_parse_throw(".2.2");
  check_parse_throw("2x");
  check_parse_throw("x2.2");
  check_parse_throw("2.x");

  check_parse_throw("a~");
  check_parse_throw("a =");
  check_parse_throw("= a");

  check_parse_throw("\\");
  //fixmecheck_parse_throw("(1,2)");
  check_parse_throw("a$b");
  check_parse_throw("a$");
  check_parse_throw("2$a");
}

test_case(parse_comments) {
  check_parse("#nothing here","");
  check_parse("1 2 3 ### 456","(1 2 3)");
  check_parse("1 2 3 #blabla\n  7 8","(1 2 3 (7 8))");
}

}} //namespace plap::test
