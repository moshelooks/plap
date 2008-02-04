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

// a singly linked list - currently wraps the gcc implementation - change this
// if you have a different compiler

#ifndef PLAP_UTIL_SLIST_H__
#define PLAP_UTIL_SLIST_H__

#include <ext/slist>

namespace plap { namespace util {

using __gnu_cxx::slist;

}} //namespace plap::util
#endif //PLAP_UTIL_SLIST_H__
