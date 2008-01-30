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
#ifndef dorepeat

//dorepeat(n) foo
//repeats foo n times
//maybe its mean to other people use this, but its certainly nicer that typing 
//for (int i=0;i<n;++i) when you never use i!

#define __DOREPEAT_CONCAT_3_( a, b ) a##b
#define __DOREPEAT_CONCAT_2_( a, b ) __DOREPEAT_CONCAT_3_( a, b )
#define __DOREPEAT_CONCAT( a, b ) __DOREPEAT_CONCAT_2_( a, b )
#define __DOREPEAT_UNIQUE_NAME __DOREPEAT_CONCAT( DOREPEAT_UNIQUE_NAME_, __LINE__ )

#define dorepeat(N) \
for (unsigned int __DOREPEAT_UNIQUE_NAME=N; __DOREPEAT_UNIQUE_NAME>0;--__DOREPEAT_UNIQUE_NAME)

#endif
