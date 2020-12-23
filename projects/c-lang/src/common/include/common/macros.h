/* $Id */

// Convention - DIRECTORY_NAME_H
#ifndef COMMON_MACROS_H
#define COMMON_MACROS_H

// _____________________________________________________________________________
//                                                                         Lang

/* LOCAL void foo (void); makes clear that 'foo' is local in its module */
#define LOCAL   static

#define INT16   short
#define UINT16  unsigned short
#define INT32   long
#define INT64   long long
#define UINT64  unsigned long long

typedef enum { OK, ERROR } status;

typedef void* generic_ptr;

// _____________________________________________________________________________
//                                                                         Math

#define PI             3.14159265

#define MIN(x, y)      ((x) < (y) ? (x) : (y))
#define MAX(x, y)      ((x) > (y) ? (x) : (y))

#define IS_ODD(num)    ((num) & 1)
#define IS_EVEN(num)   (!IS_ODD( (num) ))
#define IS_BETWEEN(num_to_test, num_low, num_high)                           \
  ((unsigned char)((num_to_test) >= (num_low) && (num_to_test) <= (num_high)))

#define COMPARE(x, y)  (((x) > (y)) - ((x) < (y)))

// _____________________________________________________________________________
//                                                                         Bits

#define BIT(x)           (1 << (x))
#define SETBIT(x, p)     ((x) | (1 << (p)))
#define CLEARBIT(x, p)   ((x) & (~(1 << (p))))
#define GETBIT(x, p)     (((x) >> (p)) & 1)
#define TOGGLEBIT(x, p)  ((x) ^ (1 << (p)))
#define LSB(x)           ((x) ^ ((x) - 1) & (x))

// _____________________________________________________________________________
//                                                       Array/String/Character

#define SWAP(T, x, y)    do { T tmp = (x); (x) = (y); (y) = tmp; } while(0)
#define ARRAY_SIZE(a)    ((sizeof(a)/sizeof(0[a])) / ((size_t)(!(sizeof(a) % sizeof(0[a])))))

#define STR_ALLOC(str)          (char *)malloc(strlen(str) + 1)
#define STR_CONCAT(str1, str2)  (str1 "" str2)
#define STR_CMP(A, o, B)        (strcmp((A), (B)) o 0)

#define ISBLANK(c)              ((c) == '\t' || (c) == ' ')

// _____________________________________________________________________________
//                                                            Testing/Debugging

#define IS_ARRAY(a)      ((void *)&a == (void *)a)
#define MEMCMP(A, o, B)  (memcmp((A), (B)) o 0)

#define DEBUG(fmt, ...)                                          \
  do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,  \
                      __LINE__, __func__, __VA_ARGS__); } while (0)

/* Example: assert(IMPLIES(n > 0, array != NULL)); */
#define IMPLIES(x, y)    (!(x) || (y))

/*
 * Placed at the beginning of a function it suppress compiler
 * warnings about unused parameters.
 */
#define UNUSED(param_name)                                 \
  ((void)(0 ? ((param_name) = (param_name)) : (param_name)))

// _____________________________________________________________________________
//                                                                   Structures

/* Obtain the offset of a field in a structure */
#define GET_FIELD_OFFSET(struct_name, field_name)     \
  ((short)(long)(&((struct_name *)NULL) -> field_name))

/* Obtain the struct element at the specified offset */
#define GET_FIELD_PTR(p_struct, offset)   \
  ((void *)(((char *)p_struct) + (offset)))

/*
 * Allocates a structure given the structure name and returns a pointer to
 * that allocated structure.
 */
#define ALLOC_STRUCT(struct_name)              \
  ((struct_name *)malloc( sizeof(struct_name) ))

/* Initializes the given structure to zeroes */
#define ZERO_INIT_STRUCT(p_struct) (memset( p_struct, '\0', sizeof( *(p_struct) )))

#endif /* COMMON_MACROS_H */

/*
 * Copyright (c) 1997-2021 by ...
 */
