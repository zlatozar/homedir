/**
 * Copyright (C). Copyright Year(s). License.
 *
 * @file   SmartPointer.h
 *
 * @author
 * @date
 *
 * @brief A reference-counting smart pointer. Not thread safe.
 *
 * If you only use smart pointers(@p auto_ptr) for simple cases, such as allocating
 * memory that is only used within a function, this issue will not be a problem.
 * For example you can use @p auto_ptr to implement exception safe code(guard pattern)
 * similar to a finally block in other languages or to when ownership transfer occurs.
 *
 * However, if your program stores several smart pointers in a data structure
 * or otherwise complicates the use of smart pointers by copying them,
 * assigning them, or passing them as arguments to functions, adding another
 * level of safety is essential. You can use this class to accomplish this.
 */

#ifndef _SMARTPOINTER_H_
#define _SMARTPOINTER_H_

// _____________________________________________________________________________
//                                                                     Includes

#include <map>
#include <iostream>

/**
 * @class SmartPointer
 * @brief
 *
 * A reference-counting smart pointer is safer than the built-in @p auto_ptr
 * because it keeps track of the number of references to a pointer and deletes
 * the memory only when it is no longer in use.
 *
 * @note Also you can read about @p shared_ptr<T>
 * @author
 * @version 1.0.0
 */
template <typename T>
class SmartPointer
{
 public:
  explicit SmartPointer(T* inPtr);
  ~SmartPointer();

  SmartPointer(const SmartPointer<T>& src);
  SmartPointer<T>& operator=(const SmartPointer<T>& rhs);

  T& operator*();
  const T& operator*() const;
  T* operator->();
  const T* operator->() const;

  operator void*() const { return _ptr; }

 protected:
  T* _ptr;
  /// static map for reference counts
  static std::map<T*, int> sRefCountMap;

  void initPointer(T* inPtr);
  void finalizePointer();
};

// _____________________________________________________________________________
//                                                      Template Implementation

template <typename T>
std::map<T*, int>SmartPointer<T>::sRefCountMap;

template <typename T>
SmartPointer<T>::SmartPointer(T* inPtr)
{
  initPointer(inPtr);
}

template <typename T>
SmartPointer<T>::SmartPointer(const SmartPointer<T>& src)
{
  initPointer(src._ptr);
}

template <typename T>
SmartPointer<T>&
SmartPointer<T>::operator=(const SmartPointer<T>& rhs)
{
  if (this == &rhs) {
    return (*this);
  }
  finalizePointer();
  initPointer(rhs._ptr);

  return (*this);
}

  template <typename T>
  SmartPointer<T>::~SmartPointer()
  {
    finalizePointer();
  }

template<typename T>
void SmartPointer<T>::initPointer(T* inPtr)
{
  _ptr = inPtr;
  if (sRefCountMap.find(_ptr) == sRefCountMap.end()) {
    sRefCountMap[_ptr] = 1;
  } else {
    sRefCountMap[_ptr]++;
  }
}

template<typename T>
void SmartPointer<T>::finalizePointer()
{
  if (sRefCountMap.find(_ptr) == sRefCountMap.end()) {
    std::cerr << "ERROR: Missing entry in map!" << std::endl;
    return;
  }
  sRefCountMap[_ptr]--;
  if (sRefCountMap[_ptr] == 0) {
    // no No more references to this object - delete it and remove from map
    sRefCountMap.erase(_ptr);
    delete _ptr;
  }
}

template <typename T>
const T* SmartPointer<T>::operator->() const
{
  return (_ptr);
}

template <typename T>
const T& SmartPointer<T>::operator*() const
{
  return (*_ptr);
}

template <typename T>
T* SmartPointer<T>::operator->()
{
  return (_ptr);
}

template <typename T>
T& SmartPointer<T>::operator*()
{
  return (*_ptr);
}

#endif /* _SMARTPOINTER_H_ */
