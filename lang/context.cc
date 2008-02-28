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

#include "context.h"
#include "core.h"

namespace plap { namespace lang {
#if 0
func& context::create_func(const std::string& name) { 
  func& f=create_func();
  _names.insert(make_pair(&f,name));
  return f;
}
func& context::create_func() { 
  _funcs.push_front(func());
  return _funcs.front();
}

void context::bind(func& f,def* d) {
  
}
#endif

}} //namespace plap::lang
