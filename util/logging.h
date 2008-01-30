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

#ifndef PLAP_UTIL_LOGGING_H__
#define PLAP_UTIL_LOGGING_H__

#ifdef NLOGGING
#  define BOOST_LOG_INIT( format )                              ((void)0)
#  define BOOST_LOG_ADD_OUTPUT_STREAM( stream, max_log_level )  ((void)0)
#  define BOOST_LOG(level, _trace)                              ((void)0)
#  define BOOST_LOG_UNFORMATTED(level, _trace)                  ((void)0)
#else
#  include "logging_private.h"
#endif

#endif  // PLAP_UTIL_LOGGING_H__
