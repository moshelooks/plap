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

//a valid type is a core type or list_of<Type> or func_of<Type(Type,...,Type)>
//or tuple_of<Type,...,Type> where all Type are valid types

//core types
typedef contin_t number_t;
typedef char     char_t;
typedef disc_t   ordinal_t;
typedef disc_t   categorical_t;
typedef bool     bool_t;

//fwd declaration
template<typename T>
T literal_cast(const_vsubtree);
template<typename T>
T vertex_cast(const vertex&);

namespace lang_private {
template<typename Type>
struct type : boost::equality_comparable<Type> {
  type(const_vsubtree s) : _s(s) { assert(vertex_cast<def_t>(s.root())); }
  bool operator==(const Type& rhs) const { return _s==rhs._s; }
  Type operator=(const Type& rhs) { _s=rhs._s; }
 protected:
  const_vsubtree _s;
};
}//namespace lang_private

template<typename T>
struct list_of {
  typedef T                          value_type;
  typedef value_type*                pointer;
  typedef value_type&                reference;
  typedef const value_type&          const_reference;
  typedef std::size_t                size_type;
  typedef std::ptrdiff_t             difference_type;

  typedef boost::transform_iterator<T (*)(util::const_subtree<lang::vertex>), 
                                    const_vsub_child_it> const_iterator;
  typedef const_iterator iterator;

  list_of(const_vsubtree s) : _s(s) {}

  const_iterator begin() const { 
    return util::transform_it(this->_s.begin_sub_child(),&literal_cast<T>);
  }
  const_iterator end() const { 
    return util::transform_it(this->_s.end_sub_child(),&literal_cast<T>);
  }

  size_type size() const { return this->_s.arity(); }
  bool empty() const { return this->_s.childless(); }
  
  const T& front() const { return literal_cast<T>(this->_s.front_sub()); }
  const T& back() const { return literal_cast<T>(this->_s.back_sub()); }
 protected:
  const_vsubtree _s;
};

template<typename T>
struct func_of;

template<typename T,typename U>
struct func_of<T(U)> {
  func_of(const_vsubtree s) : lang_private::type<func_of<T(U)> >(s) {}
  T operator()(const U& u) {
#if 0
     //break constness to add args
    vsubtree sub=*reinterpret_cast<vsubtree*>(&this->_s);
    sub.append(sub.begin(),u);
    (*vertex_cast<def_t>(sub.root()))(sub,tmp);
    sub.erase(sub.end_child()); //remove the args ****/
    return literal_cast<T>(tmp);
#endif
    assert(false);
    return T();
  }
};

}} //namespace plap::lang

#endif //PLAP_LANG_TYPE_H__
