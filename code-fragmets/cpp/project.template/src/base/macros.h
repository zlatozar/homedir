/**
 * Copyright (C). Copyright Year(s). License.
 *
 * @file   macros.h
 *
 * @author
 * @date
 *
 * @brief  Contains common macros for a project
 *
 * Contains project macros, for compiler pre-defined macros visit:
 * @href http://predef.sourceforge.net/index.php
 *
 */
#ifndef _MACROS_H_
#define _MACROS_H_

#include <cstring>

//@{
/** More readable assertion to point class invariants */
#define PRE_CONDITION(message, condition) assert(message, (condition))
#define POST_CONDITION(message, condition) assert(message, (condition))
//@}

/**
 * @def DISALLOW_COPY_AND_ASSIGN
 * @brief A macro to disallow the copy constructor and operator= functions
 *
 * This should be used in the @p private: declarations for a class
 */
#define DISALLOW_COPY_AND_ASSIGN(TypeName)      \
  TypeName(const TypeName&);                    \
  void operator=(const TypeName&)

/**
 * @def DISALLOW_IMPLICIT_CONSTRUCTORS
 * @brief A macro to disallow all the implicit constructors, namely the
 * default constructor, copy constructor and operator= functions.
 *
 * This should be used in the @p private: declarations for a class
 * that wants to prevent anyone from instantiating it. This is
 * especially useful for classes containing @b only static methods.
 */
#define DISALLOW_IMPLICIT_CONSTRUCTORS(TypeName)        \
  TypeName();                                           \
  DISALLOW_COPY_AND_ASSIGN(TypeName)

/**
 * This template function declaration is used in defining arraysize.
 * Note that the function doesn't need an implementation, as we only
 * use its type.
 */
template <typename T, size_t N>
    char (&ArraySizeHelper(T (&array)[N]))[N];

/**
 * @def arraysize
 * @brief The arraysize(arr) macro returns the number of elements in an array arr.
 *
 * The expression is a compile-time constant, and therefore can be
 * used in defining new arrays, for example.  If you use @p arraysize on
 * a pointer by mistake, you will get a compile-time error.
 * One caveat is that arraysize() doesn't accept any array of an
 * anonymous type or a type defined inside a function.
 */
#define arraysize(array) (sizeof(ArraySizeHelper(array)))

/**
 * @def CALL_MEMBER_FN
 * @brief Used to call class member functions
 *
 * Usage example:
 * @code
 * // FredMemFn points to a member of Fred that takes (char,float)
 * typedef int (Fred:: *FredMemFn)(char x, float y);
 *
 * // usage
 * void userCode(Fred& fred, FredMemFn memFn) {
 *   int ans = CALL_MEMBER_FN(fred, memFn)('x', 3.14);
 * }
 * @endcode
 */
#define CALL_MEMBER_FN(object, ptrToMember)     \
  ((object).*(ptrToMember))

/**
 * Define a function that always returns false. It is used to
 * define safe pointer delete.
 */
namespace __useless {
  inline bool i_am_false () {return false;}
}

/**
 * @def DELETE
 * @brief Macro deletes a scalar pointer and sets it to 0
 *
 * @note Implementors are "encouraged" to have pointers
 * get nulled out after deleteing them, but obviously aren't required to.
 */
#define DELETE(ptr)                                                     \
  do { if(ptr) {delete (ptr); (ptr) = NULL;} } while (__uselss::i_am_false())

/**
 * @def ARRAY_DELETE
 * @brief Macro deletes an array of pointers and sets it to 0
 *
 * @note Implementors are "encouraged" to have pointers
 * get nulled out after deleteing them, but obviously aren't required to.
 */
#define ARRAY_DELETE(ptr)                                               \
  do { if(ptr) {delete [] (ptr); (ptr) = NULL;} } while (__uselss::i_am_false())

#endif /* _MACROS_H_ */
