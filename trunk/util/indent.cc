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

#include "indent.h"
#include <string>
#include <stack>
#include <stdexcept>

namespace plap { namespace util {

namespace {
using std::string;
const char whitespace[]=" \t";
bool is_whitespace(char c) { return (c==' ' || c=='\t'); }
string::size_type count_indent(const string& s) {
  return s.find_first_not_of(whitespace);
}
bool all_whitespace(const string& s) {
  return count_indent(s)==string::npos;
}
bool has_whitespace(const string& s,string::size_type from) {
  return s.find_first_of(whitespace,from)!=string::npos;
}
void chomp_trailing_whitespace(string& s) {
  s.erase(s.find_last_not_of(whitespace)+1);
}

void zap_comments(string& s) {
  string::size_type from=s.find_first_of('#');
  if (from!=string::npos && (from==0 || s[from-1]!='\\'))
    s.erase(from);
}

void process_line(std::istream& in,string& s,bool indented) {
  s.clear();
  do {
    std::getline(in,s);
    zap_comments(s);
    if (!all_whitespace(s) || (indented && !is_whitespace(in.peek())))
      break;
  } while (in.good());
  chomp_trailing_whitespace(s);
}
} //namespace

void indent2parens(std::istream& in,std::ostream& out) {
  std::stack<string::size_type> indents;
  string s;
  do {
    process_line(in,s,!indents.empty());
    if (s.empty())
      break;

    string::size_type indent=count_indent(s);
    if (!indents.empty() && indent<=indents.top()) {
      for (out << ')';!indents.empty() && indent<indents.top();indents.pop())
        out << ')';
      if (indents.empty() || indent!=indents.top())
        throw std::runtime_error("Bad indention around '"+s+"'.");
    } else {
      indents.push(indent);
    }
    out << '(' << s.substr(indent);
  } while (in.good() && is_whitespace(in.peek()));
  while (!indents.empty()) {
    out << ')';
    indents.pop();
  }
}

}} //namespace plap::util
