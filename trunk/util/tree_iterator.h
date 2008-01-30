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

#ifndef PLAP_UTIL_TREE_ITERATOR_H__
#define PLAP_UTIL_TREE_ITERATOR_H__

namespace util {

namespace util_private {

template<typename Container,typename Out>
struct out_iter
    : public boost::iterator_facade<Out,Out,std::output_iterator_tag> {

  out_iter(Container& c,typename Container::iterator i) : _c(&c),_i(i) {}
 protected:
  Container* _c;
  typename Container::iterator _i;

  friend class boost::iterator_core_access;
  Out& dereference() { return *static_cast<Out*>(this); }
};

} //~namespace util_private

template<typename Container>
struct append_iterator
    : public util_private::out_iter<Container,append_iterator<Container> > {

  append_iterator(Container& c,typename Container::iterator i)
      : util_private::out_iter<Container,append_iterator<Container> >(c,i) {}

  append_iterator& operator=(typename Container::const_reference v) {
    this->_c->append(this->_i,v);
    return *this;
  }
};
template<typename Container>
struct prepend_iterator
    : public util_private::out_iter<Container,prepend_iterator<Container> > {

  prepend_iterator(Container& c,typename Container::iterator i)
      : util_private::out_iter<Container,prepend_iterator<Container> >(c,i) {}

  prepend_iterator& operator=(typename Container::const_reference v) {
    this->_c->prepend(this->_i,v);
    return *this;
  }
};

template<typename Container>
append_iterator<Container>
appender(Container& c,typename Container::iterator i) { 
  return append_iterator<Container>(c,i);
}

template<typename Container>
prepend_iterator<Container>
prepender(Container& c,typename Container::iterator i) { 
  return prepend_iterator<Container>(c,i);
}

} //~namespace util

#endif  // PLAP_UTIL_TREE_ITERATOR_H__
