/**
 * Copyright (C). Copyright Year(s). License.
 *
 * @file   valgrind_test.cpp
 *
 * @author
 * @date
 *
 * @brief Valgrind messages are cryptic so use this test to see messages
 *
 * compile: g++ -g -Wall -o valgrind_test valgrind_test.cpp
 * run: valgrind --log-file=valgrind_errors.log -v ./valgrind_test <test number>
 */

#include <cassert>
#include <iostream>

using namespace std;

/**
 * This test provides an example of using uninitialised memory
 */
void test_1()
{
  int i;
  printf("%d\n", i);               // Error, i hasn't been initialized

  int* num = (int*) malloc(sizeof(int));
  cout << *num << endl;            // Error, *num hasn't been initialized

  free(num);
}

/**
 * This test provides an example of reading/writing memory after it
 * has been free'd
 */
void test_2()
{
  int* i = new int;
  delete i;
  *i = 4;                          // Error, i was already freed
}

/**
 * This test provides an example of reading/writing off the end of
 * malloc'd blocks
 */
void test_3()
{
  int* i = (int*) malloc(sizeof(int)*10);

  i[10] = 13;                      // Error, wrote past the end of the block
  cout << i[-1] << endl;           // Error, read from before start of the block
  free(i);
}

/**
 * This test provides an example of reading/writing inappropriate
 * areas on the stack.  Note that valgrind only catches errors below
 * the stack (so in this example, we have to pass a negative index
 * to ptr or valgrind won't catch the problem)
 */
void test_4()
{
  int i;
  int* ptr = &i;
  ptr[-8] = 7;                     // Error, writing to a bad location on stack
  i = ptr[-15];                    // Error, reading from a bad stack location
}


/**
 * This test provides an example of memory leaks -- where pointers
 * to malloc'd blocks are not freed
 */
void test_5()
{
  int* i = new int;
  static double* j = new double;

  i = NULL;

  // Note that neither i or j were freed here, although j being static means
  // that it will be considered still reachable instead of definitely lost!
}

/**
 * This test provides an example of mismatched use of
 *  malloc/new/new [] vs free/delete/delete []
 */
void test_6()
{
  int* i = new int;
  free(i);                         // Error, new/free mismatch

  double* j = new double[50];
  delete j;                        // Error, new[],delete mismatch
}

/**
 * This test provides an example of overlapping src and dst
 * pointers in memcpy() and related functions
 */
void test_7()
{
  char big_buf[1000];
  char* ptr_1 = &big_buf[0];
  char* ptr_2 = &big_buf[400];

  memcpy(ptr_1, ptr_2, 500);       // Error, dst region overlaps src region
}

/**
 * This test provides an example of doubly freed memory
 */
void test_8()
{
  int* i = new int;
  delete i;
  delete i;                        // Error, i delete'd twice
}


/**
 * This test provides an example of passing unaddressable bytes to a
 * system call.  Note that the file descriptors for standard input
 * (stdin) and standard output (stdout) are 0 and 1 respectively,
 * which is used in the read(2) and write(2) system calls (see the
 * respective man pages for more information).
 */
void test_9()
{
  char* buf = new char[50];
  printf("Please type a bunch of characters and hit enter.\n");

  read(0, buf, 1000);              // Error, read data overflows buffer
  write(1, buf, 1000);             // Error, data comes from past end of buffer

  delete[] buf;
}

/**
 * Used to specify particular test
 *
 * @param argc
 * @param argv number of test that you want to run
 *
 * @return success if you specify a test
 */
int main(int argc, char**argv)
{

  if (argc!=2) {
    cerr << "Syntax:" << endl;
    cerr << "  " << argv[0] << " " << endl;
    return -1;
  }

  int test_number = atoi(argv[1]);

  switch (test_number) {
    case 1: test_1(); break;
    case 2: test_2(); break;
    case 3: test_3(); break;
    case 4: test_4(); break;
    case 5: test_5(); break;
    case 6: test_6(); break;
    case 7: test_7(); break;
    case 8: test_8(); break;
    case 9: test_9(); break;
    default: cout << "No test or invalid test specified (only 1-9 are valid)."
                  << endl;
      return (-1);
  }
  return (0);
}
