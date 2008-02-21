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
#include <sstream>
#include <stdexcept>
#include "dorepeat.h"
#include "io.h"

#include <iostream>
using namespace std;

namespace plap { namespace util {

namespace {
using std::string;
using std::istream;
using std::ostream;
using std::stringstream;

//const char whitespace_chars[]=" \t";
bool whitespace(char c) { return (c==' ' || c=='\t'); }
/*string::size_type chomp_indent(string& s) {
  string::size_type n=min(s.find_first_not_of(whitespace_chars),string::npos);
  s.erase(0,n);
  return n;
  }*/

struct indent_parser {
  indent_parser() : indenting(true),indent(0),comma_mode(false),
                    incomment(false),prev(0) { }
  bool indenting;
  string::size_type indent;
  std::stack<string::size_type> indents;
  string line;
  bool comma_mode,incomment;
  char prev;

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

    if (c=='#' && prev!='\\') {
      incomment=true;
      return true;
    } else if (incomment) {
      if (c=='\n' || c==EOF)
        incomment=false;
      else
        return true;
    }

    if (indenting && whitespace(c)) {
      ++indent;
      prev=c;
      return true;
    } else if (c=='\n') {
      if (prev==',') {
        comma_mode=true;
      } else {
        indenting=true;
        indent=0;
        comma_mode=false;
      }
      prev=c;
      //fixme - advance by start here
      return (whitespace(in.peek()) || !dump(out));
    } else if (c==EOF) {
      in.putback(c);
      dump(out);
      return false;
    }
    if (indenting) {
      indenting=false;
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

    /* if (c=='[')  {
      in.putback(c);
      string tmp;
      read_balanced(in,tmp,'[',']');
      stringstream ss_in,ss_out;
      ss_in << tmp.substr(1,tmp.length()-2);
      io_loop(ss_in,ss_out,indent_parser(),true);
      tmp=ss_out.str();
      cout << "read '" << tmp << "'" << endl;
      out << "([" << tmp.substr(1,tmp.length()-2) << "])";
      } else*/ {
      out << c;
    }
    prev=c;
    return true;
  }

};
} //namespace

void indent2parens(istream& in,ostream& out,string::size_type start) {
  assert(start==0);
  io_loop(in,out,indent_parser(),true);
}


}} //namespace plap::util
