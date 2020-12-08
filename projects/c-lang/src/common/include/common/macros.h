/* $Id */

// Convention - DIRECTORY_NAME_H
#ifndef COMMON_MACROS_H
#define COMMON_MACROS_H

#define INT16 short
#define INT32 long

/// Least significant bit
#define LSB(x) ((x) ^ ((x) - 1) & (x))

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

#define IS_ODD(num) ((num) & 1)

#define IS_EVEN(num) (!IS_ODD( (num) ))

#define IS_BETWEEN(num_to_test, num_low, num_high) \
        ((unsigned char)((num_to_test) >= (num_low) && (num_to_test) <= (num_high)))

#define COMPARE(x, y) (((x) > (y)) - ((x) < (y)))

#define SIGN(x) COMPARE(x, 0)

#define SWAP(x, y, T) do { T tmp = (x); (x) = (y); (y) = tmp; } while(0)

#define COUNT_OF(x) ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))

#define COLUMNS(S, E) [(E) - (S) + 1]

// Compare strings
#define STRCMP(A, o, B) (strcmp((A), (B)) o 0)

// Compare memory
#define MEMCMP(A, o, B) (memcmp((A), (B)) o 0)

// _____________________________________________________________________________
//                                                                      Testing

#define debug(fmt, ...)                                               \
  do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,       \
                          __LINE__, __func__, __VA_ARGS__); } while (0)

/// Continue with next if first is true.
///     Example: assert(IMPLIES(n > 0, array != NULL));
#define IMPLIES(x, y) (!(x) || (y))

// _____________________________________________________________________________
//                                                                   Structures

/// Obtain the offset of a field in a structure
#define GET_FIELD_OFFSET(struct_name, field_name) \
        ((short)(long)(&((struct_name *)NULL) -> field_name))

/// Obtain the struct element at the specified offset given the struct ptr
#define GET_FIELD_PTR(pStruct, offset) \
        ((void *)(((char *)pStruct) + (offset)))

/// Allocates a structure given the structure name and returns a pointer to
/// that allocated structure.
#define ALLOC_STRUCT(struct_name) ((struct_name *)malloc( sizeof(struct_name) ))

#endif

/*
 * Copyright (c) 1997-2021 by ...
 */
