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

#ifndef PLAP_UTIL_IO_H__
#define PLAP_UTIL_IO_H__

#include <istream>
#include <string>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/concepts.hpp>
#include <boost/iostreams/operations.hpp>

namespace plap { namespace util {

void read_balanced(std::istream& in,std::string& str,
                   char lparen='(',char rparen=')',bool ignore_comments=false);

#define UTIL_IO_begin_loop                      \
  out << prompt; out.flush();                   \
  char c; c=in.get();                           \
  while (in.good()) { in.putback(c);
#define UTIL_IO_end_loop                        \
  out << prompt; out.flush();                   \
  c=in.get(); }

template<typename In,typename Reader,typename Writer>
void io_loop(std::istream& in,std::ostream& out,Reader read,Writer write,
             const std::string& prompt="") {
  UTIL_IO_begin_loop;
  In i;
  if (!read(in,i) || !in.good())
    break;
  write(out,i);
  UTIL_IO_end_loop;
}
template<typename ReadWrite>
void io_loop(std::istream& in,std::ostream& out,ReadWrite rw,
             const std::string& prompt="") {
  UTIL_IO_begin_loop;
  if (!rw(in,out) || !in.good())
      break;
  UTIL_IO_end_loop
}
#undef UTIL_IO_begin_loop;
#undef UTIL_IO_end_loop;

struct comment_zap_filter : public boost::iostreams::input_filter {
  comment_zap_filter() : _escaped(false),_incomment(false) {}
  template<typename Source>
  int get(Source& src) {
    int c;
    do {
      c=boost::iostreams::get(src);
      if (!_escaped && c=='#')
        _incomment=true;
      else if (c=='\n')
        _incomment=false;
      _escaped=(c=='\\' && !_escaped);
    } while (c!=EOF && c!=boost::iostreams::WOULD_BLOCK && _incomment);
    return c;
  }
 protected:
  bool _escaped,_incomment;
};

inline boost::iostreams::filtering_istream& 
zap_comments(boost::iostreams::filtering_istream& dst,std::istream& in) {
  dst.push(comment_zap_filter());
  dst.push(in);
  return dst;
}

}} //namespace plap::util
#endif //PLAP_UTIL_IO_H__
