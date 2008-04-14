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
#include <boost/bind.hpp>

namespace plap { namespace util {

bool all_whitespace(const std::string& s) {
  return std::find_if(s.begin(),s.end(),!boost::bind(&whitespace,_1))==s.end();
}

void sexpr_getter::get_balanced(std::string& dst) {
  dst.clear();
  char c;
  do {
    c=get();
    if (c==EOF)
      break;
    dst.push_back(c);
  } while (*_in && (all_whitespace(dst) || !balanced()));
  if (!balanced())
    error(dst);
}

void sexpr_getter::get_balanced_lines(std::string& dst) {
  dst.clear();
  do {
    std::string tmp;
    getline(tmp);
    dst.append(tmp);
    dst.push_back('\n');
  } while (*_in && !all_whitespace(dst) && !balanced());
  if (!balanced())
    error(dst);
}

char sexpr_getter::get() { 
  char c=_in->get();
  if (*_in && c!=EOF) {
    switch(_mode) {
      case normal:
        if (lparen(c)) {
          ++_nparen;
        } else if (rparen(c)) {
          if (!_nparen--)
            error("");
        } else if (c=='"') {
          _mode=inquote;
        } else if (c==_marker) {
          _mode=incomment;
          return get();
        }
        break;
      case incomment:
        if (c=='\n')
          _mode=normal;
        return get();
      case inquote:
        if (c=='"') {
          _mode=normal;
        } else if (c=='\\') {
          _mode=escaped;
          if (_in->peek()!='"')
            return get();
        }
        break;
      case escaped:
        if (c=='t')
          c='\t';
        else if (c=='n')
          c='\n';
        else if (c!=_marker && c!='"' && c!='\\')
          throw std::runtime_error("Unrecognized escape sequence '\\"+
                                   std::string(1,c)+std::string("'."));
        _mode=inquote;
        break;
    }
  }
  return c;
}

void sexpr_getter::getline(std::string& dst) {
  char c;
  do {
    dst.clear();
    do {
      c=get();
      if (c==EOF)
        return;
      dst.push_back(c);
    } while (*_in && c!='\n');
  } while (all_whitespace(dst));
}

void sexpr_getter::error(const std::string& at) {
  std::string tothrow="Unbalanced parens";
  if (!at.empty())
    tothrow+=" in expression "+
        (at.size()>10 ? " starting with '"+at.substr(0,10) : "'"+at)+"'";
  tothrow+=".";
  throw std::runtime_error(tothrow);
}

}} //namespace plap::util
