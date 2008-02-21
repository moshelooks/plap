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

#if 0
struct indent_parser {
  indent_parser(string::size_type s) : start(s) {}
  string::size_type start;
  std::stack<string::size_type> indents;

  void finish_expr(ostream& out) {
    while (!indents.empty()) {
      out << ')';
      indents.pop();
    }
  }

  bool operator()(istream& in,ostream& out) {
    string s;
    string::size_type indent=readline(in,out,s,!indents.empty());
    if (s.empty()) {
      finish_expr(out);
      return false;
    }

    flush_indents(out,indent);
    out << '(' << s;
    
    if (in && is_whitespace(in.peek()))
      return true;
    finish_expr(out);
    return false;
  }

  void flush_indents(ostream& out,string::size_type indent) {
    if (!indents.empty() && indent<=indents.top()) {
      for (out << ')';!indents.empty() && indent<indents.top();indents.pop())
        out << ')';
      if (indents.empty() || indent!=indents.top())
        throw std::runtime_error("Bad indention.");
    } else {
      indents.push(indent);
    }
  }

  string::size_type readline(istream& in,ostream& out,
                             string& line,bool indented) {
    char c;
    string::size_type indent=0;
    if (indented) {
      dorepeat(start) {
        c=in.get();
        if (c=='\n')
          return 0;
        else if (!in || !is_whitespace(c))
          throw std::runtime_error("Insufficient indent within expression.");
      }
    }

    while (true) {
      c=in.get();
      if (!in)
        return 0;
      else if (c=='\n')
        if (indented)
          throw std::runtime_error("Insufficient indent within expression.");
        else
          indent=0;
      else if (!is_whitespace(c))
        break;
      else
        ++indent;
    }
    in.putback(c);

    do {
      c=in.get();
#define UTIL_INDENT_do_parens(lparen,rparen)            \
      if (c==lparen) {                                  \
        flush_indents(out,indent);                      \
        out << line;                                    \
        line.clear();                                   \
        in.putback(c);                                  \
        string tmp;                                     \
        read_balanced(in,tmp,lparen,rparen);            \
        std::cout << "READ '" << tmp << "'" << std::endl;\
        tmp=string(++tmp.begin(),--tmp.end());           \
        std::cout << "OK" << tmp << std::endl;           \
        out << lparen << '{';                                \
        stringstream ss;                                \
        ss << tmp;                                      \
        indent2parens(ss,out);                          \
        out << '}' << rparen;                            \
        std::cout << "OK" << std::endl;                 \
        continue;                                       \
      }
      
      UTIL_INDENT_do_parens('(',')');
      UTIL_INDENT_do_parens('[',']');
      
      line.push_back(c);
    } while (in && c!='\n');
    std::cout << "XOK" << std::endl;
    if (!line.empty() && (*line.rbegin()=='\n' || *line.rbegin()==EOF))
      line.erase(--line.end());
    std::cout << "TOK" << std::endl;
    chomp_trailing_whitespace(line);
    std::cout << "ZOK" << std::endl;
    return indent;
  }
};
#endif

void zap_comments(string& s) {
  string::size_type from=s.find_first_of('#');
  if (from!=string::npos && (from==0 || s[from-1]!='\\'))
    s.erase(from);
}

void process_line(std::istream& in,string& s,bool indented) {
  s.clear();

  char c=in.get();
  while (in.good() && c!=EOF) {
    in.putback(c);
    
    c=in.get();
    if (c!='\n')
      s.push_back(c);
    else {
      //std::getline(in,s);
      c=in.get();
        in.putback(c);
      if (!all_whitespace(s) || (indented && !is_whitespace(c)))
        break;
    }

    c=in.get();
  }
  chomp_trailing_whitespace(s);
}
} //namespace

void indent2parens(istream& in,ostream& out,string::size_type indent) {
  //io_loop(in,out,indent_parser());
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
