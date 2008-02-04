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

#ifndef PLAP_UTIL_ALGORITHM_H__
#define PLAP_UTIL_ALGORITHM_H__

#include <algorithm>
#include <numeric>
#include <boost/bind.hpp>
#include <ext/algorithm>

namespace plap { namespace util {
  //these needs to be changed for non-gcc
  using __gnu_cxx::copy_n;
  using __gnu_cxx::lexicographical_compare_3way;
  using __gnu_cxx::random_sample_n;
  using __gnu_cxx::random_sample;
  using __gnu_cxx::is_heap;
  using __gnu_cxx::is_sorted;

  //binary and ternary and quaternary for_each
  template<typename It1,typename It2,typename F>
  F for_each(It1 from1,It1 to1,It2 from2,F f) {
    for (;from1!=to1;++from1,++from2)
      f(*from1, *from2);
    return f;
  }
  template<typename It1,typename It2,typename It3,typename F>
  F for_each(It1 from1,It1 to1,It2 from2,It3 from3,F f) {
    for (;from1!=to1;++from1,++from2,++from3)
      f(*from1,*from2,*from3);
    return f;
  }
  template<typename It1,typename It2,typename It3,typename It4,typename F>
  F for_each(It1 from1,It1 to1,It2 from2,It3 from3,It4 from4,F f) {
    for (;from1!=to1;++from1,++from2,++from3,++from4)
      f(*from1,*from2,*from3,*from4);
    return f;
  }

  //accumulate over a range of containers
  template <class iter, class T>
  T accumulate2d(iter first,iter last, T init) {
    for ( ;first!=last;++first)
      init=std::accumulate(first->begin(),first->end(),init);
    return init;
  }

  //erase the intersection of sorted ranges [from1,to1) and [from2,to2) from c,
  //leaving the difference 
  template<typename Erase,typename It1,typename It2,typename Comp>
  void erase_set_intersection(Erase erase,It1 from1,It1 to1,
                              It2 from2,It2 to2,Comp comp) {
    assert(is_sorted(from1,to1,comp));
    assert(is_sorted(from2,to2,comp));

    while (from1!=to1 && from2!=to2)
      if (comp(*from1,*from2)) {
        ++from1;
      } else if (comp(*from2,*from1)) {
        ++from2;
      } else {
        erase(from1++);
        ++from2;
      }
  }

  //erase the difference of sorted ranges [from1,to1) and [from2,to2) from c,
  //leaving the intersection
  template<typename Erase,typename It1,typename It2,typename Comp>
  void erase_set_difference(Erase erase,It1 from1,It1 to1,
                            It2 from2,It2 to2,Comp comp) {
    assert(is_sorted(from1,to1,comp));
    assert(is_sorted(from2,to2,comp));

    while (from1!=to1 && from2!=to2)
      if (comp(*from1,*from2)) {
        erase(from1++);
      } else if (comp(*from2,*from1)) {
        ++from2;
      } else {
        ++from1;
        ++from2;
      }
    while (from1!=to1)
      erase(from1++);
  }

  //insert [from2,to2)-[from1,to1) with inserter
  //i.e., if insert inserts into the container holding [from1,to1),
  //it will now hold the union
  template<typename Insert,typename It1,typename It2,typename Comp>
  void insert_set_complement(Insert insert,It1 from1,It1 to1,
                             It2 from2,It2 to2,Comp comp) {
    assert(is_sorted(from1,to1,comp));
    assert(is_sorted(from2,to2,comp));

    while (from1!=to1 && from2!=to2)
      if (comp(*from1,*from2)) {
        ++from1;
      } else if (comp(*from2,*from1)) {
        insert(from1,*from2);
        ++from2;
      } else {
        ++from1;
        ++from2;
      }
    while (from2!=to2) {
      insert(from1,*from2);
      ++from2;
    }      
  }

  template<typename It1,typename It2,typename Comp>
  bool has_empty_intersection(It1 from1,It1 to1,
                              It2 from2,It2 to2,Comp comp) {
    assert(is_sorted(from1,to1,comp));
    assert(is_sorted(from2,to2,comp));

    while (from1!=to1 && from2!=to2)
      if (comp(*from1,*from2))
        ++from1;
      else if (comp(*from2,*from1))
        ++from2;
      else
        return false;
    return true;
  }

  //Predicate maps to the range [0,n)
  //n-1 values (the pivots) are copied to out
  template<typename It,typename Pred,typename Out>
  Out n_way_partition(It begin,It end,const Pred p,int n,Out out) {
    //could be made more efficient if needed
    for (int i=0;i<n-1;++i)
      *out++=begin=std::partition(begin,end,boost::bind(p,_1)==i);
    return out;
  }

}} //namespace plap::util

#endif //PLAP_UTIL_ALGORITHM_H__
