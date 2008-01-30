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

//This header mainly exists because the boost iterator-creation helpers have
//intolerably long names, e.g.:
//boost::make_permutation_iterator(e,i)
//vs.
//util::permute_it(e,i)
//
//also repeat_it and others are added

#ifndef PLAP_UTIL_ITERATOR_SHORTHANDS_H__
#define PLAP_UTIL_ITERATOR_SHORTHANDS_H__

#include <boost/iterator/counting_iterator.hpp>
#include <boost/iterator/permutation_iterator.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/reverse_iterator.hpp>

namespace util {

template<typename Incrementable>
inline boost::counting_iterator<Incrementable> 
count_it(Incrementable x) {
  return boost::make_counting_iterator(x);
}

template<typename ElementIterator,typename IndexIterator>
inline boost::permutation_iterator<ElementIterator,IndexIterator>
permute_it(ElementIterator e,IndexIterator i) {
  return boost::make_permutation_iterator(e,i);
}

template<typename UnaryFunction,typename Iterator>
inline boost::transform_iterator<UnaryFunction,Iterator>
transform_it(Iterator it,UnaryFunction fun) { 
  return boost::make_transform_iterator(it,fun);
}

template<typename Iterator>
inline boost::indirect_iterator<Iterator> 
indirect_it(Iterator it) {
  return boost::make_indirect_iterator(it);
}

template<typename BidirectionalIterator>
inline boost::reverse_iterator<BidirectionalIterator>
reverse_it(BidirectionalIterator x) {
  return boost::make_reverse_iterator(x);
}

template<typename Value>
struct repetition_iterator
    : public boost::iterator_facade<repetition_iterator<Value>,Value,
                                    boost::random_access_traversal_tag,
                                    const Value&> {
  repetition_iterator(const Value& v,std::size_t i) : _v(v),_i(i) {}
 protected:
  Value _v;
  std::size_t _i;

  friend class boost::iterator_core_access;
  const Value& dereference() const { return _v; }
  bool equal(const repetition_iterator& rhs) const { return this->_i==rhs._i; }
  void advance(std::ptrdiff_t d) { _i+=d; }
  void increment() { ++_i; }
  void decrement() { --_i; }
  std::ptrdiff_t distance_to(const repetition_iterator& rhs) const { 
    return rhs._i-this->_i; 
  }
};
template<typename Value>
inline repetition_iterator<Value>
repeat_it(const Value& v,std::size_t i=0) {
  return repetition_iterator<Value>(v,i);
}

} //namespace util

#endif  // PLAP_UTIL_ITERATOR_SHORTHANDS_H__
