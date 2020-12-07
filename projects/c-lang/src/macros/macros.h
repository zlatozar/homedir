/// Example: assert (IMPLIES(x, y))
#define IMPLIES(x, y) (!(x) || (y))

#define INT16 short
#define INT32 long

/// Least significant bit
#define LSB(x) ((x) ^ ((x) - 1) & (x))

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

#define IS_ODD(num) ((num) & 1)

#define IS_EVEN(num) (!IS_ODD( (num) ))

#define IS_BETWEEN(numToTest, numLow, numHigh) \
        ((unsigned char)((numToTest) >= (numLow) && (numToTest) <= (numHigh)))

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
//                                                                       Struct

/// Obtain the offset of a field in a struct
#define GET_FIELD_OFFSET(StructName, FieldName) \
        ((short)(long)(&((StructName *)NULL)->FieldName))

/// Obtain the struct element at the specified offset given the struct ptr
#define GET_FIELD_PTR(pStruct, nOffset) \
        ((void *)(((char *)pStruct) + (nOffset)))

/// Allocates a structure given the structure name and returns a pointer to
/// that allocated structure.
#define ALLOC_STRUCT(StructName) ((StructName *)malloc( sizeof( StructName )))

/// Initializes the given structure to zeroes using memset().
#define INIT_STRUCT(pStruct) (memset( pStruct, '\0', sizeof( *(pStruct) )))

/*
 * Copyright (c) 1997-2021 by ...
 */
