// Copyright 2007 Google Inc. All Rights Reserved.
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

/****
     Implementation Overview

     A tree is a generalization of a doubly linked list with sentinel node to a
     data structure where each node has three pointers instead of two. For a
     normal node the first two function identically to the pointers of a node
     in a double linked list (previous and next, respectively), and the last
     points to the sentinel node of the node's children, or NULL if the node is
     childless. For a sentinel node, the first pointer (previous) points at the
     last real node in the list, the second pointer (next) points at the
     sentinel node's parent (or the node itself if it has no parent), and the
     third points at the first real node in the list. A tree always contains an
     initial "end" sentinel node that is always on the same level as the root
     of the tree.

     This leads to an invariant that any node x in a non-empty tree is a
     sentinel iff x->next->prev != x. Accordingly, no memory overhead is
     required to discriminate between sentinels and real nodes. 

     Because, excepting the "end" sentinel, internal node are in one-to-one
     correspondence with sentinel nodes, the "structural" memory overhead of a
     tree with n internal nodes and m leaves is exactly 6n + 3m + 1 pointers.
****/

/****
     Warnings and Injunctions

     Currently, any iterator type may be happily implicitly converted to any
     other. This is often convenient, at the cost of risking some serious error
     which the compiler would otherwise flag. For example, a valid range of
     pre-order iterators, when converted to child iterators, may become
     invalid. It is the user's responsiblity to ensure that ranges passed to
     functions remain valid *after* any implicit conversions have been
     performed.

     If this turns out to be too big a nuisance, future versions may take a
     more conservative approach to implicit iterator-type conversions.
****/

/****
     Iterator Types

     pre_iterator
     const_pre_iterator
     sub_pre_iterator
     const_sub_pre_iterator
     child_iterator
     const_child_iterator
     sub_child_iterator
     const_sub_child_iterator
     post_iterator
     const_post_iterator
     sub_post_iterator
     const_sub_post_iterator
****/

#ifndef PLAP_UTIL_TREE_H__
#define PLAP_UTIL_TREE_H__

#include "tree_private.h"

namespace util {

template<typename T>
struct const_subtree
    : public util_private::subtr<util_private::const_node_policy
                                 <T,const_subtree<T> > > {
  
  typedef util_private::subtr<util_private::const_node_policy
                              <T,const_subtree<T> > > super;

  template<typename OtherTr>
  const_subtree(const OtherTr& other) : super(other.root_node()) { 
    assert(!other.empty());
  }
 protected:
  template<typename,typename>
  friend struct util_private::sub_iter_base;

  const_subtree(const util_private::node_base* n) : super(n) {}
};

template<typename T>
struct subtree
    : public util_private::subtr<util_private::mutable_node_policy
                                 <T,subtree<T> > > {
  
  typedef util_private::subtr<util_private::mutable_node_policy
                              <T,subtree<T> > > super;

  template<typename OtherTr>
  subtree(OtherTr& other) : super(other.root_node()) {}

  template<typename OtherTr>
  subtree& operator=(const OtherTr& rhs) {
    assert(!rhs.empty());
    if (static_cast<const void*>(&rhs)!=static_cast<const void*>(this)) {
      this->root()=rhs.root();
      append(this->begin(),rhs.begin_sub_child(),rhs.end_sub_child());
    }
    return *this;
  }

  subtree& operator=(const T& t) { 
    this->prune();
    this->root()=t;
    return *this;
  }

 protected:
  template<typename,typename>
  friend struct util_private::tr;
  template<typename,typename>
  friend struct util_private::mutable_tr;
  template<typename,typename>
  friend struct util_private::sub_iter_base;
  template<typename>
  friend struct const_subtree;

  typedef util_private::node_base node_base;

  subtree(node_base* n) : super(n) {}

  node_base* root_node() { return this->_node; }
  node_base* end_node() { 
    node_base* n=this->_node->next;
    ascend(n);
    return n;
  }
  const node_base* root_node() const { return super::root_node(); }
  const node_base* end_node() const { return super::end_node(); }
};

template<typename T>
struct tree : public util_private::mutable_tr<T,tree<T> > {
  typedef T value_type;

  tree() : _end() {}
  tree(const T& t) : _end(new util_private::node<T>(&_end,&_end,t)) { 
    _end.prev=_end.next; 
  }
  template<typename OtherTr>
  explicit tree(const OtherTr& other) : _end() { init(other); }
  tree(const tree& other) : _end() { init(other); }
  ~tree() { clear(); }

  tree& operator=(const tree& rhs) {
    if (&rhs!=this) {
      this->clear();
      if (!rhs.empty())
        this->append(this->insert(this->end(),rhs.root()),
                     rhs.begin_sub_child(),rhs.end_sub_child());
    }
    return *this;
  }

  bool empty() const { return this->_end.next==&this->_end; } 

  void clear() { 
    if (!this->empty()) 
      this->erase(this->begin()); 
  }

 protected:
  friend struct util_private::tr<T,tree<T> >;
  friend struct util_private::mutable_tr<T,tree<T> >;
  friend struct const_subtree<T>;
  friend struct subtree<T>;
  typedef util_private::node_base node_base;

  node_base _end;

  const node_base* root_node() const { return this->_end.next; }
  const node_base* end_node() const { return &this->_end; }
  node_base* root_node() { return this->_end.next; }
  node_base* end_node() { return &this->_end; }

  template<typename OtherTr>
  void init(const OtherTr& other) {
    if (!other.empty())
      insert(this->end(),other.root_sub());
  }
};

template<typename T>
struct tree_placeholder : public tree<T> {
  tree_placeholder(const T& t) : tree<T>(t) {}
  template<typename OtherTr>
  tree_placeholder(const OtherTr& t) : tree<T>(t) {}

 protected:
  typedef const tree_placeholder& ctp;
  tree_placeholder& x(ctp c) { this->prepend(this->begin(),c); return *this; }
 public:
  tree_placeholder& operator()(ctp c1) { return x(c1); }

  /**
    for higher arity n run this awk script, and paste the output below

    awk 'BEGIN { n=10; for (i=2;i<=n;++i) {
           printf("  tree_placeholder& operator()(");
           for (j=1;j<i;++j) printf("ctp c%i,",j);
           print "ctp c"i") {";
           printf("    this->operator()(");
           for (j=2;j<i;++j) printf("c%i,",j);
           print "c"i");";
           print "    return x(c1);\n  }"; } }'
  **/
  tree_placeholder& operator()(ctp c1,ctp c2) {
    this->operator()(c2);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3) {
    this->operator()(c2,c3);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4) {
    this->operator()(c2,c3,c4);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4,ctp c5) {
    this->operator()(c2,c3,c4,c5);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4,ctp c5,ctp c6) {
    this->operator()(c2,c3,c4,c5,c6);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4,
                               ctp c5,ctp c6,ctp c7) {
    this->operator()(c2,c3,c4,c5,c6,c7);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4,
                               ctp c5,ctp c6,ctp c7,ctp c8) {
    this->operator()(c2,c3,c4,c5,c6,c7,c8);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4,ctp c5,
                               ctp c6,ctp c7,ctp c8,ctp c9) {
    this->operator()(c2,c3,c4,c5,c6,c7,c8,c9);
    return x(c1);
  }
  tree_placeholder& operator()(ctp c1,ctp c2,ctp c3,ctp c4,ctp c5,
                               ctp c6,ctp c7,ctp c8,ctp c9,ctp c10) {
    this->operator()(c2,c3,c4,c5,c6,c7,c8,c9,c10);
    return x(c1);
  }
};

template<typename T>
tree_placeholder<T> tree_of(const T t) { return tree_placeholder<T>(t); }

} //~namespace util

#endif  // PLAP_UTIL_TREE_H__
