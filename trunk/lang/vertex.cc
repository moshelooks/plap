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

#include "vertex.h"
#include <limits>
#include <boost/integer_traits.hpp>
#include <boost/static_assert.hpp>
#include <boost/preprocessor/control/if.hpp>

namespace plap { namespace lang {

namespace {
BOOST_STATIC_ASSERT(std::numeric_limits<number_t>::has_quiet_NaN);
BOOST_STATIC_ASSERT(sizeof(id_t)==sizeof(number_t));
BOOST_STATIC_ASSERT(sizeof(id_t)==sizeof(func_t));
BOOST_STATIC_ASSERT(sizeof(id_t)==4);//fixme for 64bit || sizeof(id_t)==8);
} //namespace

const id_t vertex::funcarg_mask=0x7F800000; //fixme64bit0x7FF0000000000000
const id_t vertex::symbolarg_mask=0xFF800000; //fixme64bit,0xFFF0000000000000);

const id_t vertex::false_id=0;
const id_t vertex::true_id=1;
const id_t vertex::nil_id=2;

const id_t vertex::smallest_lang_arg=3;
const id_t vertex::largest_lang_arg=vertex::smallest_lang_arg+LANG_ARG_MAX;

const id_t vertex::smallest_char=vertex::largest_lang_arg+1;
const id_t vertex::largest_char=vertex::smallest_char
    +boost::integer_traits<char>::const_max
    -boost::integer_traits<char>::const_min;
const id_t vertex::char_offset=vertex::smallest_char
    -boost::integer_traits<char>::const_min;

}} //namespace plap::lang
