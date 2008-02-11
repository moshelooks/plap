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

#include <iostream>
#include "repl.h"
#include "environment.h"
/*

#include "parse.h"
#include "tree_io.h"

#include <sstream>
*/
int main() { 
  using namespace std;
  const string prompt="\033[22;32m> \033[00;m";
  //const string prompt="\033[22;32m> [\033[00m";
  plap::lang::environment env;
  cout << "ctrl+D exits" << endl;
  while (cin.good()) {
    //cin >> tr;
    //cout << endl << tr << endl;
    //indent_parse(cin,ss);
    //cout << endl << "got '" << ss.str() << "'" << endl;
    try {
      plap::lang_io::repl(std::cin,std::cout,env,prompt);
    } catch (std::runtime_error e) {
      cerr << "\033[22;31m" << e.what() << "\033[00;m" << endl;
    } catch (...) {
      cerr << "\033[22;31munkown exception caught - failing...\033[00;m" 
           << endl;
      return 1;
    }
  }
  return 0;
}
