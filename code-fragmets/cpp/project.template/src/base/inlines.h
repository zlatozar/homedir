/**
 * Copyright (C). Copyright Year(s). License.
 *
 * @file   inline.h
 *
 * @author
 * @date
 *
 * @brief  Contains common inlines for a project
 *
 *
 */
#ifndef _INLINES_H_
#define _INLINES_H_

#include <string.h>    // for memcpy
#include <string>


inline void split(const std::string& s, char delim,
                  std::vector<std::string>& elems) {
  std::stringstream ss(s);
  std::string item;
  while(std::getline(ss, item, delim)) {
    elems.push_back(item);
  }
}

inline std::string& ltrim(std::string& s) {
  s.erase(s.begin(),
          std::find_if(s.begin(),
                       s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
  return s;
}

inline std::string& rtrim(std::string& s) {
  s.erase(std::find_if(s.rbegin(), s.rend(),
                       std::not1(std::ptr_fun<int, int>(std::isspace))).base(),
          s.end());
  return s;
}

inline std::string& trim(std::string& s) {
  return ltrim(rtrim(s));
}

inline void assign_to_string(std::string* str, const char* ptr, size_t n) {
  str->resize(n);
  memcpy(&*str->begin(), ptr, n);
}

template<typename T>
inline void assign_to_vector(std::vector<T>* vec, const T* ptr, size_t n) {
  vec->resize(n);
  memcpy(&vec->front(), ptr, n*sizeof(T));
}

#endif /* _INLINES_H_ */
