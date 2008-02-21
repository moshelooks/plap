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
#include "io.h"

namespace plap { namespace util {

namespace {
using std::string;
using std::istream;
using std::ostream;

bool whitespace(char c) { return (c==' ' || c=='\t'); }
bool newline(char c) { return (c=='\n'); }
bool eof(char c) { return (c==EOF); }

struct indent_parser {
  indent_parser() : mode(indenting),indent(0) {}

  enum { indenting,commaed,incomment,escaped,normal } mode;
  string::size_type indent;
  std::stack<string::size_type> indents;

  bool dump(ostream& out) {
    if (indents.empty())
      return false;
    do {
      out << ')';
      indents.pop();
    } while (!indents.empty());
    return true;      
  }

  bool operator()(istream& in,ostream& out) {
    char c=in.get();
    
    if (c=='#' && mode!=escaped) {
      mode=incomment;
      return true;
    } else if (mode==incomment) {
      if (newline(c) || eof(c))
        mode=indenting;
      else
        return true;
    }

    if (mode==indenting && whitespace(c)) {
      ++indent;
      return true;
    } else if (newline(c)) {
      if (mode!=commaed) {
        mode=indenting;
        indent=0;
      }
      return (whitespace(in.peek()) || !dump(out));
    } else if (eof(c)) {
      in.putback(c);
      dump(out);
      return false;
    }

    if (mode==indenting) {
      if (!indents.empty() && indent<=indents.top()) {
        for (out << ')';!indents.empty() && indent<indents.top();indents.pop())
          out << ')';
        if (indents.empty() || indent!=indents.top())
          throw std::runtime_error("Bad indention.");
      } else {
        indents.push(indent);
      }
      if (indent!=0)
        out << ' ';
      out << '(';
    }
    
    if (c==',')
      mode=commaed;
    else if (c=='\\')
      mode=escaped;
    else
      mode=normal;

    out << c;
    return true;
  }
};
} //namespace

void indent2parens(istream& in,ostream& out,string::size_type start) {
  assert(start==0);
  io_loop(in,out,indent_parser(),true);
}

}} //namespace plap::util
