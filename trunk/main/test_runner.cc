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
#include "foreach.h"
#include "dorepeat.h"
#include "environment.h"
#include "vertex_aux.h"
#include "types.h"
#include "def.h"

#include <iterator>
#include <numeric>
#include <boost/lexical_cast.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/bind.hpp>

#define test_case BOOST_AUTO_TEST_CASE
#define check_eq BOOST_CHECK_EQUAL
#define check BOOST_CHECK
#define print(x) { cout << x << endl; }

using namespace util;
using namespace lang;

using namespace std;
using namespace boost;
using namespace boost::assign;

#include "tree_test.cc"
#include "lang_test.cc"
