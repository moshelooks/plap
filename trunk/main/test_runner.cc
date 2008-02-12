// Copyright 2007 Google Inc. All Rights Reserved.
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

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/included/unit_test.hpp>

//all includes needed by the tests should go here
#include "tree_io.h"
#include "tree_iterator.h"
#include "foreach.h"
#include "dorepeat.h"
#include "environment.h"
#include "parse.h"
#include "cast.h"
//#include "eager_def.h"
//#include "lib.h"

#include <iterator>
#include <numeric>
#include <boost/lexical_cast.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/bind.hpp>

#define test_case BOOST_AUTO_TEST_CASE
#define check BOOST_CHECK
#define check_eq BOOST_CHECK_EQUAL
#define check_throw BOOST_CHECK_THROW
#define check_tree(tr,sz,name)              \
{                                           \
  check_eq(tr.empty(),(sz==0));             \
  check_eq(tr.size(),(unsigned int)sz);     \
  check_eq(tr.childless(),(sz<=1));         \
  check_eq(lexical_cast<string>(tr),name);  \
}
#define print(x) { cout << x << endl; }

using namespace plap::util;
using namespace plap::lang;
using namespace plap::lang_io;

using namespace std;
using namespace boost;
using namespace boost::assign;

#include "tree_test.cc"
#include "lang_io_test.cc"
//#include "lang_test.cc"
