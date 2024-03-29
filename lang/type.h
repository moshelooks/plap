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

#ifndef PLAP_LANG_TYPE_H__
#define PLAP_LANG_TYPE_H__

#include "vtree.h"

namespace plap { namespace lang {

template<typename>
struct list_of;
template<typename>
struct func_of;
template<typename,typename>
struct pair_of;

//core types are bool, char, id_t (for symbol), and contin_t
//a core type of const_subvtree may be used for laziness
//a valid type is a core type or list_of<Type> or func_of<Type(Type,...,Type)>
//or pair_of<Type,...,Type> where all Type are valid types
namespace lang_private {
template<typename T>
struct core_type {
  //this will be triggered if this type is instantiated -
  //ensures that only valid core types may be used
  BOOST_STATIC_ASSERT(sizeof(T)==0);
};

template<>struct core_type<bool>     {};
template<>struct core_type<char>     {};
template<>struct core_type<id_t>     {};
template<>struct core_type<number_t> {};

template<>struct core_type<const_subvtree> {};

template<typename T>
struct core_type<list_of<T> > : core_type<T> {};
template<typename T,typename U>
struct core_type<func_of<T(U)> > : core_type<T>,core_type<U> {};
} //namespace lang_private

//fwd declarations needed for list_of, func_of, pair_of
template<typename T>
T literal_cast(const_subvtree);

template<typename T>
struct list_of : lang_private::core_type<T> {
  typedef T                          value_type;
  typedef value_type*                pointer;
  typedef value_type&                reference;
  typedef const value_type&          const_reference;
  typedef std::size_t                size_type;
  typedef std::ptrdiff_t             difference_type;

  typedef boost::transform_iterator<T (*)(util::const_subtree<lang::vertex>), 
                                    const_vsub_child_it> const_iterator;
  typedef const_iterator iterator;

  list_of(const_subvtree s) : src(s) {}

  const_iterator begin() const { 
    return util::transform_it(this->src.begin_sub_child(),&literal_cast<T>);
  }
  const_iterator end() const { 
    return util::transform_it(this->src.end_sub_child(),&literal_cast<T>);
  }

  size_type size() const { return this->src.arity(); }
  bool empty() const { return this->src.childless(); }
  
  const T front() const { return literal_cast<T>(this->src.front_sub()); }
  const T back() const { return literal_cast<T>(this->src.back_sub()); }

  const_subvtree src;
};

template<typename T>
struct func_of;

template<typename T,typename U>
struct func_of<T(U)> : lang_private::core_type<T>,lang_private::core_type<U> {
  func_of(const_subvtree s) : _s(s) {}
  T operator()(context& c,const U& u) {
#if 0
     //break constness to add args
    subvtree sub=*reinterpret_cast<subvtree*>(&this->_s);
    sub.append(sub.begin(),u);
    (*vertex_cast<def_t>(sub.root()))(sub,tmp);
    sub.erase(sub.end_child()); //remove the args ****/
    return literal_cast<T>(tmp);
#endif
    assert(false);
    return T();
  }
 protected:
  const_subvtree _s;
};

template<typename T,typename U>
struct pair_of : public std::pair<T,U> {};

}} //namespace plap::lang
#endif //PLAP_LANG_TYPE_H__
