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

#ifndef PLAP_LANG_TYPES_H__
#define PLAP_LANG_TYPES_H__

namespace lang {

struct type {};
struct bool_type : public type {};
struct int_type : public type {};
struct float_type : public type {};
struct empty_list_type : public type {};

template<typename Type> //Type must be a type
struct list_type : public type { 
  list_type() {}
  list_type(empty_list_type) {} 
};

} //~namespace lang

#endif  // PLAP_LANG_TYPES_H__
