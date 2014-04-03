/**
 * @file   Car.cpp
 *
 * If you return a pointer or reference to a private data member, then you
 * should declare the result to be const, as otherwise users will be able to modify your
 * internal state without going through your public API. In this case, you must also think
 * about whether the returned pointer or reference will survive longer than your class.
 * If this is possible, you should consider returning a reference-counted pointer,
 * such as a boost::shared_ptr
 *
 * @author
 * @date
 *
 * @brief  It is useful to see code
 *
 * Extended explanation
 */
#ifndef _HELPER_CAR_H_
#define _HELPER_CAR_H_

// _____________________________________________________________________________
//                                                                     Includes

#include <string>
#include "base/macros.h"

// namespace follow directory structure
namespace helper_package {
  /**
   * @ingroup Helper
   * @class Car
   * @brief Just an example
   *
   * Simple car in auto game.
   * As a rule of thumb we declare everything as virtual except constructor
   *
   * @author
   * @version 1.0.0
   */
  class Car {

  public:
    Car();
    /**
     * @brief Frees all the allocated objects.
     */
    virtual ~Car();

    /**
     * Get the speed of the car
     *
     * @return speed in miles per hour.
     */
    virtual float getSpeed() const;

    /**
     * Set the speed of the car
     *
     * @note Good candidate to inline here
     * @param inSpeed new speed of the car.
     */
    virtual void setSpeed(float inSpeed);

  protected:
    static std::string doubleToString(double inValue);
    static double stringToDouble(const std::string& inString);

    /// Speed of the car
    float _speed;

    /// Extra variable that do not change class state
    mutable float _someVariable;

  private:
    DISALLOW_COPY_AND_ASSIGN(Car);
  };
}

// _____________________________________________________________________________
//                                                               Inline methods

// Small and fast methods that change the object state

// _____________________________________________________________________________
//                                                               Static methods

// Static method from class interface

#endif /* _HELPER_CAR_H_ */
