/**
 * @file   subject_extract.cpp
 *
 * For free functions you have to decide:
 *
 * - Static versus non-static function.
 * - Pass arguments by value, reference, or pointer.
 * - Pass arguments as const or non-const.
 * - Use of optional arguments with default values.
 * - Return result by value, reference, or pointer.
 * - Return result as const or non-const.
 * - Operator or non-operator function.
 * - Use of exception specifications.
 *
 * @author
 * @date
 *
 * @brief  It is useful to see code
 *
 * Put this comment if you want to say some implementation details
 */

// _____________________________________________________________________________
//                                                               Includes/Using

#include "subject_extract.h"

#include <iostream>

#include <boost/regex.hpp>
#include <glog/logging.h>

// Helper name space or global
namespace helper_package {

using std::string;
using std::getline;

// _____________________________________________________________________________
//                                                               Implementation

int check_input()
{

  // Initialize Google's logging library in main
  LOG(INFO) << "Start program...";

  string line;
  while (std::cin)
  {
    getline(std::cin, line);
    string result = get_subject_txt(line);
    if (result.length() > 0)
      LOG(INFO) << "Subject is: " << result;
  }

  return 0;
}

string get_subject_txt(const string inLine)
{
  boost::regex pattern("^Subject: (Re: |Aw: )*(.*)");

  boost::smatch matches;
  if (boost::regex_match(inLine, matches, pattern))
  {
    return matches[2];
  }
  else
  {
    return "";
  }
}}

// _____________________________________________________________________________
//                                                              Utility methods

// write them as static or put them in namespace if there are not used from outside
