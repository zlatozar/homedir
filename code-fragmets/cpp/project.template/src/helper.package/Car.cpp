/**
 * @file   Car.cpp
 *
 * For member functions you have to decide:
 *
 * - Virtual versus non-virtual member function.
 * - Pure virtual versus non-pure virtual member function.
 * - Const versus non-const member function.
 * - Public, protected, or private member function.
 * - Use of the explicit keyword for non-default constructors.
 *
 * In addition to these options that control the logical interface of a function,
 * there are a couple of organizational attributes that you can specify for a function,
 * such as:
 *
 * - Friend function versus non-friend function.
 * - Inline function versus non-inline function.
 *
 * @author
 * @date
 *
 * @brief  It is useful to see code
 *
 * Put this comment if you want to say some implementation details
 */

// _____________________________________________________________________________
//                                                                     Includes

#include "Car.h"

#include <iostream>
#include <sstream>

#include <glog/logging.h>

using namespace std;

namespace helper_package {

// _____________________________________________________________________________
//                                                              Friends methods

// _____________________________________________________________________________
//                                                         Class Implementation

/**
 * If we initialize simple type it is equivalent to assignment for
 * custom types copy constructor is invoked.
 * @note If we have description in header file and here Doxygen will take both
 * and concatenate them
 */
Car::Car() : _speed(0), _someVariable(0)
{
}

Car::~Car()
{
}

float Car::getSpeed() const
{
  return _speed;
}

void Car::setSpeed(float inSpeed)
{
  _speed = inSpeed;
}

// Initialize class static variables

// _____________________________________________________________________________
//                                                              Utility methods

// Free/Static helper methods

/**
 * Note that you must not repeat the static keyword in front of the definition of
 * doubleToString. As long as it precedes the first instance of the function name in
 * header file there is no need to repeat it.
 */
string Car::doubleToString(double inValue)
{
  ostringstream ostr;

  ostr << inValue;
  return (ostr.str());
}

// doxygen warns that there is no documentation
double Car::stringToDouble(const string& inString)
{
  double temp;

  istringstream istr(inString);

  istr >> temp;
  if (istr.fail() || !istr.eof()) {
    return (0);
  }
  return (temp);
}}
