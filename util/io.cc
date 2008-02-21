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

#include "io.h"
#include <algorithm>
#include <stdexcept>

namespace plap { namespace util {

void read_balanced(std::istream& in,std::string& dst,
                   char lparen,char rparen,bool ignore_comments) {
  std::string::size_type nparen=0;
  char c;
  do {
    c=in.get();
    if (c==lparen)
      ++nparen;
    else if (c==rparen)
      --nparen;
    dst.push_back(c);
  } while (in && c!=EOF && (nparen>0 || (c!=' ' && c!='\t' && c!='\n')));
  if (c==EOF || c==' ' || c=='\t' || c=='\n') {
    in.putback(c);
    dst.erase(dst.length()-1); //get rid of trailing space
  }
  if (nparen!=0)
    throw std::runtime_error("paren mismatch reading: '"+dst+"'");
}

}} //namespace plap::util
